### display_surfaces - FUNCTION TO PLOT ALL THE MESHES ###

#' @name display_surfaces
#' @title display_surfaces
#'
#' @description
#'   Plots multiple mesh surfaces.
#'
#' @usage
#'   display_surfaces(surfaces = surfaces, col = NULL, alpha = 1)
#'
#' @param surfaces an object of the class "mesh_array" that contains two lists: a table with the meshes information (name, number of faces and vertices) and all the meshes that had previously been imported using the function import_surfaces.
#' @param col an optional vector selecting the colour the meshes will be plotted with. If there is only one colour selected, all the meshes will be plotted with that colour, otherwise each mesh will be plotted with the corresponding colour from the colour vector. Default is grey.
#' @param alpha an optional variable (from 0 to 1) indicating the level of 'transparency' to plot the meshes with. Default is no transparency (1).
#'
#' @details
#'   Takes an object of the class "mesh_array" and plots all the meshes together
#'   in the same rgl window.
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#'
#' data(meshes)
#'
#' # Example 1
#' display_surfaces(surfaces = meshes)
#' rgl.close() # from rlg. Close current rgl window
#'
#' # Example 2
#' display_surfaces(surfaces = meshes, col = c("green", "blue", "yellow"), alpha = 0.6)
#' rgl.close() # from rlg. Close current rgl window
#'
#'
#' @references Daniel Adler, Duncan Murdoch and others (2020). rgl: 3D Visualization Using OpenGL. R package version 0.100.54. https://CRAN.R-project.org/package=rgl
#'
#' @importFrom rgl open3d next3d shade3d
#'
#' @export
#'

display_surfaces <- function(surfaces = surfaces, col = NULL, alpha = 1){
  if(class(surfaces) != 'mesh_array') {
    stop("The imported surfaces are not of class 'mesh_array'")
  }
  mesh_obj <- surfaces$meshes
  if (is.null(col) == TRUE){
    mesh_col <- rep("grey", length(mesh_obj))
  }
  if (!is.null(col) == TRUE){
    if (length(col) == 1){
      mesh_col <- rep(col, length(mesh_obj))
    }
    if (length(col) == length(mesh_obj)){
      mesh_col <- col
    }
    else {
      stop("The length of the colour vector does not match the number of meshes
           to be plotted.")
    }
  }
  rgl::open3d()
  rgl::mfrow3d(round(sqrt(length(mesh_obj))),
               round(length(mesh_obj)/round(sqrt(length(mesh_obj)))))
  for (i in 1: length(mesh_obj)){
    rgl::next3d()
    rgl::shade3d(mesh_obj[[i]], col = mesh_col[i], alpha = alpha)
  }
}
