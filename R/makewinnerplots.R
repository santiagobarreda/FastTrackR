
#' Make images for winning analysies
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


