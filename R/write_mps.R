#### write_mps - SAVE MPS FILES FROM A LANDMARK ARRAY
#' @name write_mps
#' @title write_mps
#'
#' @description
#'   Writes *.mps files from an array of 3D landmarks.
#'
#' @usage
#'   write_mps(arrayLMs = arrayLMs, dir = NULL, ID = NULL)
#'
#' @param arrayLMs an object of the class "array" that contains
#' three-dimensional landmarks for 'n' specimens
#' @param dir optional variable that selects which directory the mps files should be saved to
#' @param ID optional vector with IDs for saving the *.mps files
#'
#' @details
#'   Takes a landmark array and saves the landmarks for each specimen as *.mps files.
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#' test1 <- array(data = 1:10, dim = c(5,3,2))
#' write_mps(arrayLMs = test1, ID = c("spec1", "spec2"))
#'
#' test2 <- array(data = 1:10, dim = c(5,3,2), dimnames = list(c("LM1", "LM2",
#' "LM3", "LM4", "LM5"), c("x", "y", "z"), c("spec1", "spec2")))
#' write_mps(arrayLMs = test2)
#'
#' test3 <- array(data = 1:10, dim = c(5,3,2))
#' dimnames(test3)[[3]] <- c("spec1", "spec2")
#' write_mps(arrayLMs = test3, dir = "../")
#'
#' @export
#'

write_mps <- function(arrayLMs = arrayLMs, dir = NULL, ID = NULL){
  if (is.null(dir) == TRUE) {
    path <- getwd()
  } else {
    path <- getwd()
    setwd(dir)
  }
  if (is.null(ID) == TRUE) {
    dimnames_mps <- dimnames(arrayLMs)[[3]]
  } else {
    dimnames_mps <- ID
  }
  if (is.null(dimnames_mps) == TRUE) {
    stop("There are no ID names for saving the *.mps files")
  }
  if (length(dimnames_mps) != dim(arrayLMs)[3]) {
    stop("ID vector has different length than the number of specimens in the landmark array.")
  }
  for (i in 1:dim(arrayLMs)[3]){
    file_mps <- file(paste0(dimnames_mps[i], ".mps"))
    min_x <- min(arrayLMs[,1,i])
    min_y <- min(arrayLMs[,2,i])
    min_z <- min(arrayLMs[,3,i])
    max_x <- max(arrayLMs[,1,i])
    max_y <- max(arrayLMs[,2,i])
    max_z <- max(arrayLMs[,3,i])
    landmarks <- character()
    for (j in 1:dim(arrayLMs)[1]){
      landmarks <- c(landmarks, "            <point>", paste0("                <id>", (j-1), "</id>"),
                     "                <specification>0</specification>", paste0("                <x>", arrayLMs[j,1,i], "</x>"),
                     paste0("                <y>", arrayLMs[j,2,i], "</y>"),
                     paste0("                <z>", arrayLMs[j,3,i], "</z>"), "            </point>")
    }
    writeLines(c('<?xml version="1.0" encoding="UTF-8" ?>', "<point_set_file>", "    <file_version>0.1</file_version>",
                 "    <point_set>", "        <time_series>","            <time_series_id>0</time_series_id>",
                 '            <Geometry3D ImageGeometry=\"false\" FrameOfReferenceID=\"0\">',
                 '                <IndexToWorld type="Matrix3x3" m_0_0="1" m_0_1="0" m_0_2="0" m_1_0="0" m_1_1="1" m_1_2="0" m_2_0="0" m_2_1="0" m_2_2="1" />',
                 '                <Offset type=\"Vector3D\" x=\"0\" y=\"0\" z=\"0\" />', "                <Bounds>",
                 paste0('                    <Min type="Vector3D" x=', '"', min_x, '"', ' y=', '"', min_y, '"', ' z=', '"', min_z,'"',  ' />'),
                 paste0('                    <Max type="Vector3D" x=', '"', max_x, '"', ' y=', '"', max_y, '"', ' z=', '"', max_z, '"', ' />'),
                 "                </Bounds>", "            </Geometry3D>", landmarks,
                 "        </time_series>", "    </point_set>", "</point_set_file>"), file_mps)
    close(file_mps)
    rm(landmarks, file_mps)
  }
  setwd(path)
}
