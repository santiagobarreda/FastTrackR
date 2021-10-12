

#' Make images comparing alternative analysies
#'
#' @param formants The formant data to be used for making plots. Read in using the readformants function in this package.
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param sounds --. 
#' @param height height of each plot in pixels.
#' @param width width of each plot in pixels.
#' @param pointsize point size for plotting.
#' @param winners if you want winners to be highlighted, provide a dataframe with the information in the winners.csv file.
#' @param number_of_lines the number of pixels along the x axis for each spectrogram. Consider in relation to image width. 
#' @param progressbar if TRUE, information about estimated analysis time is printed. 
#' @param alternate_output_path if not NA, images will be written directly to this path.
#' @param ... additional parameters are passed to the internal call of plotffs.
#' @export
#' @examples
#' \dontrun{
#' makecomparisonplots(formants[1:10])
#' makecomparisonplots(formants[1:10], winners = winners$winners_csv[1:10,])
#' }


makecomparisonplots <- function (formants=NA, path = NA, sounds = NA, height = 1000, width = 1400, 
                                 pointsize = 20, winners = NA, number_of_lines = 0.0015, progressbar = TRUE,
                                 alternate_output_path=NA,...){

  if (is.na (path)) path = getwd()
  
  sounds_exist = FALSE
  if (all(!is.na(sounds))) sounds_exist = TRUE
  
  if (!sounds_exist & file.exists (path %+% "/sounds.RDS")){
    sounds = readRDS (path %+% "/sounds.RDS")
    sounds_exist = TRUE
  }
  if (all(is.na(formants)) & file.exists (path %+% "/formants.RDS")){
    formants = readRDS (path %+% "/formants.RDS")
  }
  if (all(is.na(formants))) stop ("Formant information not provided and none found in working directory.")
  
  winners_exist = FALSE
  winner_value = NA
  if (!all(is.na(winners)))  winners_exist = TRUE
  
  if (class(formants) != "formants_single"){
    dir.create(path %+% "/images_comparison", showWarnings = FALSE)
  
    start = Sys.time ()
    for (i in 1:length (formants)){
      if (progressbar) progressbar(i,length (formants),start)
      
      if (winners_exist) winner_value = winners$winner[i]
      
      base_filename = attr (formants[[i]], "filename")
      
      image_filename = path %+% "/images_comparison/" %+% base_filename %+% ".png"
      if (!is.na(alternate_output_path))
        image_filename = alternate_output_path %+% "/" %+% base_filename %+% ".png"
      
      grDevices::png (image_filename, height = height, width = width, pointsize = 16)
      
      if (sounds_exist)
        sound = sounds[[i]]
      if (!sounds_exist)
        sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
      
      # spectrogram resolution should relate to image resolution
      spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = number_of_lines)
      plotffs (formants[[i]], spect = spect, lwd=3, pch=16, winner = winner_value, ...)
  
      grDevices::dev.off()
    }
  }
  if (class(formants) == "formants_single"){

    base_filename = attr (formants, "filename")

    sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
    
    if (sounds_exist)
      sound = sounds
    if (!sounds_exist)
      sound = tuneR::readWave (path %+% "/sounds/" %+% base_filename %+% ".wav")
    
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = number_of_lines)
    plotffs (formants, spect = spect, lwd=3, pch=16, ...)
      
  }
}




