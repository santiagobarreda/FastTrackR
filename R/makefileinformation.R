
#' Make a file information file
#'
#' @param path --.
#' @param write --.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' ffs = analyze (sound)
#' spect = spectrogram (sound)
#' plotffs (ffs[[9]])
#' plotffs (ffs[[9]], spect = spect)
#' plotffs (ffs)
#' plotffs (ffs, spect = spect)
#' }
#' 
#' 
makefileinformation = function (path = NA, write=TRUE){
  
  if (is.na (path)) path = getwd()
  
  file = list.files (paste0(path,"/sounds"))
  n_files = length(files)
  
  number = 1:n_files
  label = rep ("*", n_files)
  group = rep ("1", n_files)
  color = rep (c("Blue","Green","Magenta","Black",
                 "Lime","Purple","Teal","Navy","Pink",
                 "Maroon","Olive","Grey","Red"), length.out = n_files)
  
  fileinformation = data.frame (number = number, file = file,label=label, 
                                group = group, color = color)
  
  if (write)
    write.csv (fileinformation, "fileinformation2.csv", row.names = FALSE,quote=FALSE)
  
  fileinformation
  
}



