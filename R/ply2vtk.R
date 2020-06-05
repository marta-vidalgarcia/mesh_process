### ply2vtk - FUNCTION TO BATCH-CONVERT PLY meshes to VTK meshes

#' @name ply2vtk
#' @title ply2vtk
#'
#' @description
#'   Batch-converts PLY mesh files into VTK mesh files
#'
#' @usage
#'   plyvtk(files = NULL, ID = NULL, vtk_path = NULL, print.progress = TRUE)
#'
#' @param files Optional parameter. The default is all the PLY meshes in the current directory.
#' @param ID optional vector with IDs used for saving the VTK meshes. Default is the same name as the PLY meshes
#' @param files Optional parameter indicating where to save the VTK files. The default is the current directory.
#' @param print.progress optional parameter indicating whether to print the progress
#'
#' @return
#'   This function returns a table with the meshes information (name, number of faces and vertices) and the volume for each surface mesh.
#'
#' @details
#'   Takes a a list of PLY meshes and transforms those files into VTK. For more information check the R package _freesurferformats_ (Schäfer, 2020)
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#' # Convert all PLY meshes to VTK meshes
#' setwd("./data")
#' dir(pattern = "*.ply")
#' ply2vtk(files = NULL, ID = NULL, vtk_path = NULL, print.progress = TRUE)
#' setwd("../")
#'
#' # Convert a subset PLY meshes to VTK meshes, and change ID
#' setwd("./data")
#' files_ply <- dir(pattern = "*.ply")[1:2]
#' ID_vtk <- c("specimen1", "specimen2")
#' ply2vtk(files = files, ID = NULL, vtk_path = NULL, print.progress = TRUE)
#' setwd("../")
#'
#' @references
#' Schlager S (2017). “Morpho and Rvcg - Shape Analysis in R.” In Zheng G, Li S, Szekely G (eds.), _Statistical Shape and Deformation Analysis_, 217-256. Academic Press. ISBN 9780128104934.
#' Tim Schäfer (2020). freesurferformats: Read and Write 'FreeSurfer' Neuroimaging File Formats. R package version 0.1.10. https://CRAN.R-project.org/package=freesurferformats
#'
#' @importFrom Rvcg vcgImport
#' @importFrom freesurferformats write.fs.surface.vtk
#'
#' @export
#'

ply2vtk <- function(files = NULL, ID = NULL, vtk_path = NULL, print.progress = TRUE){
  if (is.null(files) == TRUE){
    files_list <- dir(getwd(), pattern = "*.ply")
  }
  else {
    files_list <- files
  }
  if (length(files_list) == 0){
    stop(paste0("Wrong directory. There are no PLY meshes to import."))
  }
  if (is.null(ID) == TRUE){
    ID_vtk <- substr(files_list,1,nchar(files_list)-4)
  }
  else {
    ID_vtk <- ID
  }
  if (isFALSE(length(files_list) == length(ID_vtk)) == TRUE){
    stop(paste0("The length of the ID vector does not match the length of
                mesh files to be converted."))
  }
  ply_path <- getwd()
  if (is.null(vtk_path) == TRUE){
    vtk_path <- getwd()
  }
  else {
    vtk_path <- vtk_path
  }
  pb <- txtProgressBar(1, length(files_list), style=3)
  for (i in 1:length(files_list)){
    temp_mesh <- Rvcg::vcgImport(files_list[i])
    freesurferformats::write.fs.surface.vtk(paste0(vtk_path, "/",
                                                   ID_vtk[i],".vtk"),
                                            t(temp_mesh$vb)[,1:3],
                                            t(temp_mesh$it)[,1:3])
    if (print.progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
    rm(temp_mesh)
  }
}
