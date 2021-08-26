
#' Save formant information in a single RDS file
#'
#' This function saves the list containing all analyses for all files into a single RDS file in your working directory (or any other directory). 
#' 
#' @param formants a list of formant data read in with the readformants function, or created using the trackformants function.
#' @param filename the name of the RDS file to save to, without the extension (e.g., "filename" not "filename.RDS").
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @export

saveformants <- function (formants, filename = "formants", path = NA){
  
  if (is.na (path)) path = getwd()
  
  filename = paste0 (path,"/", filename, ".RDS")
  
  saveRDS (formants, filename)
  
}

