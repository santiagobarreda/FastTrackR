
#' Read sounds a single data file
#' 
#' This function writes out R internal formant data into text files that can be used in Praat. 
#' 
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @export

readsounds = function (path = NA){
  
  if (is.na (path)) path = getwd()
  filenames = list.files (path %+% "/sounds/", full.names=TRUE)
  sounds = lapply (filenames, readWave2)
  
  sounds
}

