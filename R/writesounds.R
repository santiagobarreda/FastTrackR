
#' Write sound objects 
#' 
#' This function writes sound objects as a single data file or as a set of WAV files. 
#' 
#' @param sounds a list of sounds read in with the readsounds function of extracted from a textgrid with the extractvowels function.
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @export

writesounds = function (sounds, path = NA){

  if (is.na (path)) path = getwd()
  
  dir.create(path %+% "/sounds", showWarnings = FALSE)
  
  filenames = path %+% "/sounds/" %+% sapply (sounds, attr, "filename")
  
  tmp = sapply (1:length(sounds), function (i) tuneR::writeWave (sounds[[i]], filenames[i]))
  
}
