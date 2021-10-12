
#' Read sounds a single data file
#' 
#' This function writes out R internal formant data into text files that can be used in Praat. 
#' 
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param save --.
#' @export

readsounds = function (path = NA, save = TRUE){
  
  if (is.na (path)) path = getwd()
  filenames = list.files (path %+% "/sounds/", full.names=TRUE)
  sounds = lapply (filenames, readWave2)
  
  if (save) saveRDS (sounds, "sounds.RDS")
  
  invisible (sounds)
}

