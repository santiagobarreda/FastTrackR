

#' Make images comparing alternative analysies
#'
#' @param formants_plot --.
#' @param path --.
#' @param height --.
#' @param width --.
#' @param pointsize --.
#' @param winners --.
#' @param spectrogram_timestep --.
#' @param showprogressbar --.
#' @param ... --.
#' @export
#' @examples
#' \dontrun{
#' makecomparisonplots(formants[1:10])
#' makecomparisonplots(formants[1:10], winners = winners$winners_csv[1:10,])
#' }


makecomparisonplots <- function (formants_plot, path = NA, height = 1000, width = 1400, pointsize = 20, winners = NA, 
                                 spectrogram_timestep = 0.0015, showprogressbar = TRUE,...){

  if (is.na (path)) path = getwd()
  dir.create(path %+% "/images_comparison", showWarnings = FALSE)

  for (i in 1:length (formants_plot)){
    
    if (showprogressbar & length (formants_plot) > 9) progressbar(i,length (formants_plot))
    
    if (!is.na(winners)[1]) winner = winners$winner[i]
    base_filename = attr (formants_plot[[i]], "filename")
    image_filename = path %+% "/images_comparison/" %+% base_filename %+% ".png"
    grDevices::png (image_filename, height = height, width = width, pointsize = 16)

    sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = spectrogram_timestep)
    plotffs (formants_plot[[i]], spect = spect, lwd=3, pch=16, winner = winner, ...)

    grDevices::dev.off()
  }

}

