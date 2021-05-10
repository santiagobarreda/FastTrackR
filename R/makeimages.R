
#' MAke images for winning analysies
#'
#' @param path --.
#' @param resolution --.
#' @param csvs --.
#' @param showprogressbar --.
#' @export
#' @examples
#' \dontrun{
#' # nothing yet
#' system.time (makewinnerplots(showprogressbar=TRUE))
#' }

makewinnerplots <- function (path, resolution = 1000, csvs = NA, showprogressbar = FALSE){

  if (missing (path)) path = getwd()

  csv_files = list.files (path %+% "/csvs")

  if (is.na(csvs) & length(csv_files)==0)
    stop ("No csvs available or provided.")
  if (is.na(csvs) & length(csv_files)>0)
    csvs = readcsvs (path, asone = FALSE)

  dir.create(path %+% "/images_winners", showWarnings = FALSE)

  filenames = names (csvs)

  for (i in 1:length (filenames)){
    filename = path %+% "/images_winners/" %+% filenames[i] %+% ".png"
    grDevices::png (filename, height = 1000, width = 1400, pointsize = 24)

    sound = tuneR::readWave (path %+% "/sounds/" %+% filenames[i] %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE)
    plotffs (csvs[[i]], spect = spect, main = "")

    grDevices::dev.off()
    if (showprogressbar) progressbar(i, length (filenames))
  }
}




#' Make images comparing alternative analysies
#'
#' @param path --.
#' @param resolution --.
#' @param formants --.
#' @param winners --.
#' @param showprogressbar --.
#' @export
#' @examples
#' \dontrun{
#' system.time (makecomparisonplots(showprogressbar=TRUE))
#' }

makecomparisonplots <- function (path, resolution = 1000, formants = NA, winners = NA, showprogressbar = FALSE){

  if (missing (path)) path = getwd()

  if (is.na(formants) & length(list.files (path %+% "/formants"))==0)
    stop ("No formants available or provided.")
  if (is.na(formants) & length(list.files (path %+% "/formants"))>0)
    formants = readformants (path)

  if (is.na(winners) & !file.exists("winners.csv"))
    stop ("No winners file in working directory.")
  if (is.na(winners) & file.exists("winners.csv"))
    winners = utils::read.csv ("winners.csv")

  dir.create(path %+% "/images_comparison", showWarnings = FALSE)

  filenames = sapply (formants, attr, "filename")

  for (i in 1:length (filenames)){
    filename = path %+% "/images_comparison/" %+% filenames[i] %+% ".png"
    grDevices::png (filename, height = 1000, width = 1400, pointsize = 16)

    sound = tuneR::readWave (path %+% "/sounds/" %+% filenames[i] %+% ".wav")
    # spectrogram resolution should relate to image resolution
    spect = spectrogram (sound, plot = FALSE, padding = 1, timestep = 0.005)
    plotffs (formants[[i]], spect = spect)

    grDevices::dev.off()
    if (showprogressbar) progressbar(i, length (filenames))
  }


}

