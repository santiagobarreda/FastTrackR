
#' Make images for winning analyses
#'
#' This function will operate in one of two ways. If a
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param csvs csv data loaded using the readcsvs function. In NA, data is read in from the 'csvs' folder in the path directory. 
#' @param height the desired height of the image in pixels.
#' @param width the desired width of the image in pixels.
#' @param pointsize point size for plotting.
#' @param number_of_lines the number of pixels along the x axis for each spectrogram. Consider in relation to image width. 
#' @param progressbar if TRUE, information about estimated analysis time is displayed. 
#' @param alternate_output_path if not NA, images will be written directly to this path.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs ()
#' 
#' # this will make a plot
#' makewinnerplots(csvs = csvs[[1]])
#' 
#' # this will make 5 plots in the /images_winners folder
#' makewinnerplots(csvs = csvs[[1:20]])
#' }

makewinnerplots <- function (path=NA, csvs = NA, height=1000, width = 1400, 
                             pointsize = 20, number_of_lines = 250, 
                             progressbar = TRUE, alternate_output_path = NA){

  if (is.na(path)) path = getwd()

  csv_files = list.files (path %+% "/csvs")

  if (all(is.na(csvs)) & length(csv_files)==0)
    stop ("No csvs available or provided.")
  if (all(is.na(csvs)) & length(csv_files)>0)
    csvs = readcsvs (path, asone = FALSE)

  if (class(csvs)=="data.frame") csvs = split (csvs, csvs$file)
  filenames = names (csvs)
  
  start = Sys.time()
  if (class (csvs)=="list"){
    dir.create(path %+% "/images_winners", showWarnings = FALSE)

    for (i in 1:length (filenames)){
      
      filename = path %+% "/images_winners/" %+% filenames[i] %+% ".png"
      if (!is.na(alternate_output_path))
        filename = alternate_output_path %+% "/" %+% filenames[i] %+% ".png"
      
      grDevices::png (filename, height = 1000, width = 1400, pointsize = 24)
    
      graphics::par (mar =c(4,4,2,1))
      sound = tuneR::readWave (path %+% "/sounds/" %+% filenames[i] %+% ".wav")
      # spectrogram resolution should relate to image resolution
      spect = spectrogram (sound, plot = FALSE, resolution = 50, timestep = number_of_lines)
      plotffs (csvs[[i]], spect = spect, lwd=3,pch=16,cex=1.25)
      graphics::title (filenames[i], cex = 0.9)
  
      grDevices::dev.off()
      if (progressbar) progressbar(i, length (filenames),start)
    }
  }
  if (class (csvs)=="data.frame"){
    graphics::par (mar =c(4,4,2,1))
    sound = tuneR::readWave2 (path %+% "/sounds/" %+% filenames %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, resolution = 50, timestep = number_of_lines)
    plotffs (csvs, spect = spect, lwd=3,pch=16,cex=1.25)
    graphics::title (filenames, cex = 0.9)
  }
}


