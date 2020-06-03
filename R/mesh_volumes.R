### mesh_volumes - FUNCTION TO BATCH-EXTRACT VOLUMES

#' @name mesh_volumes
#' @title mesh_volumes
#'
#' @description
#'   Estimates the volume of multiple meshes' surfaces.
#'
#' @usage
#'   mesh_volumes(surfaces = surfaces)
#'
#' @param surfaces an object of the class "mesh_array" that contains two lists: a table with the meshes information (name, number of faces and vertices) and all the meshes that had previously been imported using the function import_surfaces.
#'
#' @return
#'   This function returns a table with the meshes information (name, number of faces and vertices) and the volume for each surface mesh.
#'
#' @details
#'   Takes an object of the class "mesh_array" and returns the volume for each mesh.
#'   For more information check the R package Rvcg (Schlager, 2017)
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#' data(meshes)
#' vol <- mesh_volumes(meshes)
#'
#' @references
#' Schlager S (2017). “Morpho and Rvcg - Shape Analysis in R.” In Zheng G, Li S, Szekely G (eds.), _Statistical Shape and Deformation Analysis_, 217-256. Academic Press. ISBN 9780128104934.
#'
#' @importFrom Rvcg vcgVolume vcgClean
#'
#' @export
#'

mesh_volumes <- function(surfaces = surfaces){
  if(class(surfaces) != 'mesh_array') {
    stop("The imported surfaces are not of class 'mesh_array'")
  }
  mesh_obj <- surfaces$meshes
  Volume <- vector(mode="numeric", length = length(mesh_obj))
  for (i in 1: length(mesh_obj)){
    Volume[i] <- Rvcg::vcgVolume(Rvcg::vcgClean(mesh_obj[[i]],sel=0:6,iterate=T))
  }
  volume_table <- as.data.frame(cbind(surfaces$mesh_info, Volume))
  return(volume_table)
}
