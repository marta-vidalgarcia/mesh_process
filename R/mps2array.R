### mps2array - FUNCTION TO BATCH-IMPORT *.MPS LANDMARK FILES INTO AND ARRAY AND EXPORT TXT MATRICES

#' @name mps2array
#' @title mps2array
#'
#' @description
#'   Imports *.mps files into an array of 3D landmarks.
#'
#' @usage
#'   mps2array(dir = NULL, ID = NULL, save.txt = FALSE)
#'
#' @param dir optional variable that selects which directory the mps files
#' should be imported from. The default is the current directory
#' @param ID optional vector with IDs for each landmark matrix from the mps files
#' @param save.txt optional. It saves each of the 3D landmark matrices as txt files,
#' without row or column names.
#'
#' @details
#'   Takes multiple mps files and batch-imports them as a landmark. It can also
#'   save the landmarks for each specimen as txt files.
#'
#' @author Marta Vidal-Garcia
#'
#' @examples
#'
#' dir(pattern = "*.mps") # three mps files with five 3D landmarks each
#' land_array <- mps2array(dir = "./data")
#'
#' # Remove "skull" from the ID name, so that the IDs are "spec1", "spec2", and "spec3".
#' land_array_2 <- mps2array(dir = "./data", save.txt = TRUE)
#'
#' # Assign IDs for each landmark matrix.
#' ID_land <- c("specimen1", "specimen2", "specimen3")
#' land_array_3 <- mps2array(dir = "./data", ID = ID_land)
#'
#' @export
#'

mps2array <- function (dir = NULL, ID = NULL, string_del = NULL,
                       save.txt = FALSE) {
  if (is.null(dir) == TRUE) {
    path <- getwd()
  } else {
    path <- getwd()
    setwd(dir)
  }
  mps_list <- dir(pattern="*.mps")
  mps_file <- vector(mode="list", length = length(mps_list))
  lands <- vector(mode="numeric", length = length(mps_list))
  for (i in 1:length(mps_list)){
    mps_file[[i]] <- readLines(mps_list[i], encoding="UTF-8")
    lands[i] <- (length(mps_file[[i]])-17)/7
  }
  if (length(unique(lands)) != 1) {
    stop("Specimens have different number of landmarks.")
  }
  if (is.null(ID) == TRUE) {
    dimnames_mps <- gsub(".mps", "", mps_list)
  } else {
    dimnames_mps <- ID
  }
  setwd(path)
  LM_array <- array(0,c(lands[1],3,length(mps_list)))
  dimnames(LM_array)[[3]] <- dimnames_mps
  for (i in 1:length(mps_list)){
    LM_matrix <- matrix(data=NA, nrow = lands, ncol = 3)
    for (n in 0:(lands-1)){
      LM_matrix[(n+1),1] <- as.numeric(gsub("</x>",
                                            "", gsub("                <x>",
                                                     "", mps_file[[i]][(17+1+(7*n))])))
      LM_matrix[(n+1),2] <- as.numeric(gsub("</y>", "",
                                            gsub("                <y>",
                                                 "", mps_file[[i]][(17+2+(7*n))])))
      LM_matrix[(n+1),3] <- as.numeric(gsub("</z>", "",
                                            gsub("                <z>",
                                                 "", mps_file[[i]][(17+3+(7*n))])))
    }
    LM_array[,,i] <- matrix(data=mapply(LM_matrix, FUN=as.numeric), ncol=3, nrow = lands)
  }
  if (isTRUE(save.txt) == TRUE) {
    for (i in 1:dim(LM_array)[3]) {
      write.table(LM_array[, , i], paste0(dimnames(LM_array)[[3]][i],
                                          ".txt"), col.names = FALSE,
                  row.names = FALSE)
    }
  }
  return(LM_array)
}
