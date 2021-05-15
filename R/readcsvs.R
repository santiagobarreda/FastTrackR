
#' Load Fast Track csv files
#'
#' This function quickly reads in all the files contained in the "/csvs" folder within a Fast Track directory. Since Fast Track exports the acoustic measurements into separate files per token, this function is a quick way to read those all in at once.
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param asone If TRUE (the default), the csv files are combined into one dataframe and filenames are indicated in a new column. If FALSE, a list of dataframes is returned and each list element is named after the file.
#' @return A dataframe or list of dataframes, as determined by the \code{asone} parameter.
#' @export
#' @examples
#' \dontrun{
#' csvs <- readcsvs()
#' }

readcsvs <- function (path, asone = TRUE){
  if (missing(path)) path = getwd()
  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  csvs = list()
  for (i in 1:length(files)){
    csvs[[i]] = utils::read.csv (files[i])
    if (!asone) names (csvs)[i] = file_names[i]
    if (asone) csvs[[i]]$file = file_names[i]
  }
  if (asone) csvs = do.call (rbind, csvs)

  attr(csvs, "object") = "csvs"

  return (csvs)
}
