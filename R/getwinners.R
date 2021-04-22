
#' Get winning analyses
#'
#' If no path is provided the working directory is assumed to be the Fast Track directory. This is recommended as it means that is means all reading/writing can be done without ever providing a path.
#'
#' @param path the path to the working directory for the Fast Track project.
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs ()
#' }

getwinners <- function (path){
  if (missing(path)) path = getwd()
  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  csvs = list()
  for (i in 1:length(files)){
    csvs[[i]] = utils::read.csv (files[i])
    if (!asone) names (csvs)[i] = file_names[i]
    if (asone) csvs[[i]]$filename = file_names[i]
  }
  if (asone) csvs = do.call (rbind, csvs)

  return (csvs)
}

