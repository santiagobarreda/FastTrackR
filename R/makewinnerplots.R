
#' Make images for winning analysies
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param csvs .
#' @param height --.
#' @param width --.
#' @param pointsize --.
#' @param number_of_lines --.
#' @param showprogressbar --.
#' @export
#' @examples
#' \dontrun{
#' # nothing yet
#' system.time (makewinnerplots(showprogressbar=TRUE))
#' makewinnerplots(csvs = csvs[1:20])
#' }

makewinnerplots <- function (path=NA, csvs = NA, height=1000, width = 1400, 
                             pointsize = 20, number_of_lines = 250, 
                             showprogressbar = TRUE){

  if (is.na(path)) path = getwd()

  csv_files = list.files (path %+% "/csvs")

  if (is.na(csvs[1]) & length(csv_files)==0)
    stop ("No csvs available or provided.")
  if (is.na(csvs[1]) & length(csv_files)>0)
    csvs = readcsvs (path, asone = FALSE)

  dir.create(path %+% "/images_winners", showWarnings = FALSE)

  filenames = names (csvs)

  if (length(filenames)>1){
    for (i in 1:length (filenames)){
      
      filename = path %+% "/images_winners/" %+% filenames[i] %+% ".png"
      grDevices::png (filename, height = 1000, width = 1400, pointsize = 24)
    
      graphics::par (mar =c(4,4,2,1))
      sound = tuneR::readWave (path %+% "/sounds/" %+% filenames[i] %+% ".wav")
      # spectrogram resolution should relate to image resolution
      spect = spectrogram (sound, plot = FALSE, resolution = 50, timestep = number_of_lines)
      plotffs (csvs[[i]], spect = spect, lwd=3,pch=16,cex=1.25)
      graphics::title (filenames[i], cex = 0.9)
  
      grDevices::dev.off()
      if (showprogressbar) progressbar(i, length (filenames))
    }
  }
  if (length(filenames)==1){
    graphics::par (mar =c(4,4,2,1))
    sound = tuneR::readWave (path %+% "/sounds/" %+% filenames %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, resolution = 50, timestep = number_of_lines)
    plotffs (csvs, spect = spect, lwd=3,pch=16,cex=1.25)
    graphics::title (filenames, cex = 0.9)
  }
}


