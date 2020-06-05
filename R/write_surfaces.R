### write_surfaces - FUNCTION TO BATCH-WRITE ALL THE MESHES ###

#' @name write_surfaces
#' @title write_surfaces
#'
#' @description Writes multiple meshes as PLY, STL, OBJ or VTK files
#'
#' @usage
#'   write_surfaces(surfaces = surfaces, pattern = c("PLY", "STL", "VTK", "OBJ"), binary = FALSE, print.progress = TRUE)
#'
#' @param surfaces an object of the class "mesh_array" that contains two lists: a table with the meshes information (name, number of faces and vertices) and all the meshes that had previously been imported using the function import_surfaces.
#' @param pattern Parameter indicating which format the meshes should be saved as. Options are _"PLY"_, _"STL"_, _"VTK"_ or _"OBJ"_.
#' @param binary optional logical parameter indicating whether to write binary files (only for _"PLY"_ or _"STL"_). For more information check *?vcgStlWrite* and *?vcgPlyWrite* in Rvcg.
#' @param print.progress optional vector with IDs for saving the *.mps files
#'
#' @details
#'   Takes an object of the class "mesh_array", which has been processed with the read_surfaces function, and saves the meshes as PLY, STL, or VTK meshes. The name of the meshes is (mesh_ID)_post.(mesht_type), for example, *mesh1_post.vtk*.
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#' data(meshes)
#'
#' # Example 1 - save VTK files
#' write_surfaces(surfaces = meshes, pattern = "VTK", print.progress = TRUE)
#'
#' #' # Example 2 - save binary PLY files, don't print progress
#' write_surfaces(surfaces = meshes, pattern = "PLY", binary = TRUE, print.progress = FALSE)
#'
#' @importFrom Rvcg vcgPlyWrite vcgStlWrite vcgObjWrite
#' @importFrom freesurferformats write.fs.surface.vtk
#'
#' @references
#' Schlager S (2017). “Morpho and Rvcg - Shape Analysis in R.” In Zheng G, Li S, Szekely G (eds.), _Statistical Shape and Deformation Analysis_, 217-256. Academic Press. ISBN 9780128104934.
#' @export
#'

write_surfaces <- function(surfaces = surfaces, pattern = "VTK", binary = FALSE, print.progress = TRUE){
  if(class(surfaces) != 'mesh_array') {
    stop("The imported surfaces are not of class 'mesh_array'")
  }
  pb <- txtProgressBar(1, length(surfaces$meshes), style=3)
  if (pattern == "PLY"){
    for (i in 1: length(surfaces$meshes)){
      Rvcg::vcgPlyWrite(surfaces$meshes[[i]], binary,
                        filename = paste0(names(surfaces$meshes)[i], "_post.ply"))
      if (print.progress == TRUE) {
        setTxtProgressBar(pb, i)
      }
    }
  }
  if (pattern == "OBJ"){
    for (i in 1: length(surfaces$meshes)){
      Rvcg::vcgObjWrite(surfaces$meshes[[i]], filename = paste0(names(surfaces$meshes)[i], "_post.obj"))
      if (print.progress == TRUE) {
        setTxtProgressBar(pb, i)
      }
    }
  }
  if (pattern == "STL"){
    for (i in 1: length(surfaces$meshes)){
      Rvcg::vcgStlWrite(surfaces$meshes[[i]], binary,
                        filename = paste0(names(surfaces$meshes)[i], "_post.stl"))
      if (print.progress == TRUE) {
        setTxtProgressBar(pb, i)
      }
    }
  }
  if (pattern == "VTK"){
    path <- getwd()
    for (i in 1: length(surfaces$meshes)){
      freesurferformats::write.fs.surface.vtk(paste0(path, "/", names(surfaces$meshes)[i],
                                                     "_post.vtk"), t(temp_mesh$vb)[,1:3],
                                              t(temp_mesh$it)[,1:3])
      if (print.progress == TRUE) {
        setTxtProgressBar(pb, i)
      }
    }
  }
}
