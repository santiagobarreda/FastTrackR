

#' Make images comparing alternative analysies
#'
#' @param formants_plot The formant data to be used for making plots. Read in using the readformants function in this package.
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param height height of each plot in pixels.
#' @param width width of each plot in pixels.
#' @param pointsize point size for plotting.
#' @param winners if you want winners to be highlighted, provide a dataframe with the information in the winners.csv file.
#' @param number_of_lines the number of pixels along the x axis for each spectrogram. Consider in relation to image width. 
#' @param progressbar if TRUE, information about estimated analysis time is printed. 
#' @param ... additional parameters are passed to the internal call of plotffs.
#' @export
#' @examples
#' \dontrun{
#' makecomparisonplots(formants[1:10])
#' makecomparisonplots(formants[1:10], winners = winners$winners_csv[1:10,])
#' }


makecomparisonplots <- function (formants_plot, path = NA, height = 1000, width = 1400, pointsize = 20, winners = NA, 
                                 number_of_lines = 0.0015, progressbar = TRUE,...){

  if (is.na (path)) path = getwd()
  
  
  if (class(formants_plot) != "formants_single"){
    dir.create(path %+% "/images_comparison", showWarnings = FALSE)
  
    start = Sys.time ()
    for (i in 1:length (formants_plot)){
      if (progressbar & length (formants_plot) > 9) progressbar(i,length (formants_plot),start)
      
      if (!is.na(winners)[1]) winner = winners$winner[i]
      if (!is.na(winners)[1]) winner = winners$winner[i]
      
      base_filename = attr (formants_plot[[i]], "filename")
      image_filename = path %+% "/images_comparison/" %+% base_filename %+% ".png"
      grDevices::png (image_filename, height = height, width = width, pointsize = 16)
  
      sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
      # spectrogram resolution should relate to image resolution
      spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = number_of_lines)
      plotffs (formants_plot[[i]], spect = spect, lwd=3, pch=16, winner = winner, ...)
  
      grDevices::dev.off()
    }
  }
  if (class(formants_plot) == "formants_single"){

    base_filename = attr (formants_plot, "filename")

    sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = number_of_lines)
    plotffs (formants_plot, spect = spect, lwd=3, pch=16, ...)
      
  }
}




