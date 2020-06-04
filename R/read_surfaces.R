#### read_surfaces - FUNCTION TO IMPORT MESHES ###

#' @name read_surfaces
#' @title read_surfaces
#'
#' @description
#'   Imports multiple meshes' surfaces.
#'
#' @usage
#'   read_surfaces <- function(dir = NULL, pattern = pattern, files = NULL,
#'   target_faces = NULL, dec_percent = NULL, mesh_topo = FALSE, mesh_qual = FALSE,
#'   mesh_bound = FALSE, mergeVert = FALSE, vcgIsolated = FALSE, IsoFacenum = NULL,
#'   IsoDiameter = NULL, clean = FALSE, cleansed = NULL)
#'
#' @param dir Optional parameter if the directory that contains the mesh is other than the current working directory. The default (NULL) is the current working directory.
#' @param files Optional parameter. The default is all the files with a pattern (e.g. ply or stl).
#' @param pattern string parameter that determines what type of mesh to import (e.g. 'ply' or 'stl')
#' @param target_faces Number of target faces for all the meshes. For more information check *?vcgQEdecim* in Rvcg.
#' @param dec_percent Degree of face reduction (from 0 to 1) across all meshes. For more information check *?vcgQEdecim* in Rvcg.
#' @param mesh_topo Optional logical parameter, indicating if the mesh topology will be preserved across all the meshes. The default is FALSE. For more information check *?vcgQEdecim* in Rvcg.
#' @param mesh_qual Optional logical parameter, indicating whether vertex quality will be considered across all the meshes. The default is FALSE. For more information check *?vcgQEdecim* in Rvcg.
#' @param mesh_bound Optional logical parameter, indicating whether mesh boundary will be preserved across all the meshes. The default is FALSE. For more information check *?vcgQEdecim* in Rvcg.
#' @param mergeVert Optional logical parameter to merge duplicated vertices across all meshes. For more information check *?mergeVertices* in rgl.
#' @param vcgIsolated Optional parameter to remove isolated pieces. The default is FALSE. For more information check *?vcgIsolated* in Rvcg.
#' @param IsoFacenum Optional parameter that determines the threshold to remove isolated pieces. The default is FALSE. For more information check *?vcgIsolated* in Rvcg.
#' @param IsoDiameter Optional parameter that determines the threshold to remove isolated pieces (pices with an smaller diamter are removed). The default is FALSE. For more information check *?vcgIsolated* in Rvcg.
#' @param clean Optional cleaning parameter from Rvcg that batch-cleans all meshes. The default is FALSE. For more information check *?vcgIsolated* in Rvcg.
#' @param cleansel Optional cleaning parameter from Rvcg that determines which cleanign algorithms are applied across all meshes and in which order. The default (NULL) is options 0 to 6. For more information check *?vcgIsolated* in Rvcg.
#'
#' @return
#'   This function returns an object of the class 'mesh_array' will keep the dimnames
#'   associated with each mesh (which correspond to the filenames).
#'
#' @details
#'   Imports multiple mesh files (e.g. PLY, STL) and batch-processes them (e.g. reducing the number of faces, merging vertices, removing isolated pieces, etc. with a few functions from Rvcg). For more information check the R package Rvcg (Schlager, 2017)
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#' # PLY meshes
#' dir <- "./data/"
#'
#' # Import PLY meshes without any processing
#' test1 <- read_surfaces(dir = "./data", pattern = "ply")
#'
#' # Import STL meshes and reduce number of faces
#' test2 <- read_surfaces(dir = dir, files = NULL, pattern = "stl", target_faces = 50000, clean = TRUE)
#'
#' @references
#' Schlager S (2017). “Morpho and Rvcg - Shape Analysis in R.” In Zheng G, Li S, Szekely G (eds.), _Statistical Shape and Deformation Analysis_, 217-256. Academic Press. ISBN 9780128104934.
#'
#' @importFrom Rvcg vcgImport vcgQEdecim vcgIsolated
#' @importFrom rgl mergeVertices
#'
#' @export
#'

read_surfaces <- function(dir_meshes = NULL, pattern = pattern, files = NULL, target_faces = NULL, dec_percent = NULL, mesh_topo = FALSE,
                            mesh_qual = FALSE, mesh_bound = FALSE, mergeVert = FALSE, vcgIsolated = NULL, IsoFacenum = NULL,
                          IsoDiameter = NULL, clean = FALSE){
  if (is.null(dir_meshes) == TRUE){
    org_path <- getwd()
  } else {
    org_path <- getwd()
    setwd(dir_meshes)
  }
  if (is.null(files) == TRUE){
    files_list <- dir(pattern = pattern)
  } else {
    files_list <- files
  }
  if (length(files_list) == 0){
    stop(paste0("Wrong directory. There are no meshes of pattern = ", pattern, " to import."))
  }
  mesh_list <- vector(mode="list", length = length(files_list))
  meshes_info <- matrix(nrow=length(files_list), ncol= 3)
  colnames(meshes_info) <- c("specimen", "number_faces", "number_vertices")
  for (i in 1: length(mesh_list)){
    mesh_list[[i]] <- Rvcg::vcgImport(files_list[i])
    # Options: reduce number of faces
    if (is.null(target_faces) == TRUE){
      if(is.null(dec_percent) == TRUE){
        mesh_list[[i]] <- mesh_list[[i]]
      } else{
        mesh_list[[i]] <- Rvcg::vcgQEdecim(mesh_list[[i]], percent = dec_percent, topo = mesh_topo,
                                           quality = mesh_qual, bound = mesh_bound)
      }
    } else {
      mesh_list[[i]] <- Rvcg::vcgQEdecim(mesh_list[[i]], tarface = target_faces, topo = mesh_topo,
                                         quality = mesh_qual, bound = mesh_bound)
    }
    if(is.null(dec_percent) == TRUE){
      mesh_list[[i]] <- mesh_list[[i]]
    } else{
      mesh_list[[i]] <- Rvcg::vcgQEdecim(mesh_list[[i]], percent = dec_percent, topo = mesh_topo,
                                         quality = mesh_qual, bound = mesh_bound)
    }
    # Optional: remove isolated pieces
    if (isTRUE(vcgIsolated) == FALSE){
      mesh_list[[i]] <- mesh_list[[i]]
    }
    if (isTRUE(vcgIsolated) == TRUE){
      mesh_list[[i]] <- Rvcg::vcgIsolated(mesh_list[[i]], facenum = IsoFacenum, diameter = IsoDiameter)
    }
    # Optional: merge vertices
    if (isTRUE(mergeVert == TRUE)){
      mesh_list[[i]] <- rgl::mergeVertices(mesh_list[[i]])
    } else {
      mesh_list[[i]] <- mesh_list[[i]]
    }
    if (isTRUE(clean) == TRUE){
      if (is.null(cleansel) == TRUE){
        mesh_list[[i]] <- Rvcg::vcgClean(mesh_list[[i]], sel = c(0:6), iterate = T)
      } else {
        mesh_list[[i]] <- Rvcg::vcgClean(mesh_list[[i]], sel = cleansel, iterate = T)
      }
    } else {
      mesh_list[[i]] <- mesh_list[[i]]
    }
    # mesh_list[[i]]$vertices_t <- t(mesh_list[[i]]$vb)[,1:3] # transposed vertices.
    # mesh_list[[i]]$faces_t <- t(mesh_list[[i]]$it) # transposed faces.
    # mesh_list[[i]]$normal_vert <- t(mesh_list[[i]]$normals)[,1:3] # transposed normals
    meshes_info[i,1] <- substr(files_list,1,nchar(files_list)-4)[i]
    meshes_info[i,2] <- length(mesh_list[[i]]$it)
    meshes_info[i,3] <- length(mesh_list[[i]]$vb)
  }
  names(mesh_list) <- substr(files_list,1,nchar(files_list)-4)
  meshes_import <- list("meshes" = mesh_list, "mesh_info" = meshes_info)
  class(meshes_import) <- 'mesh_array'
  setwd(org_path)
  return(meshes_import)
}
