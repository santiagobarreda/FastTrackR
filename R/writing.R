
#' Write Fast Track csv files
#'
#'
#' @param csvs csv data read in using the readcsvs() function, either as a dataframe or a list of dataframes.
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs ()
#' }

writecsvs <- function (csvs, path){
  if (missing(csvs)) stop ("Must provide csvs object.")
  if (missing(path)) path = getwd()

  if (class (csvs) == "data.frame"){
    tmp_csv = csvs[,-which (colnames(csvs)=="file")]
    csvs = split (tmp_csv, csvs$file)
  }

  if (class (csvs) == "list"){
    files = list.files (paste0(path,"/csvs"),full.names=TRUE)
    for (i in 1:length(files))
      utils::write.csv (csvs[[i]], files[i], row.names = FALSE,quote=FALSE)
  }
}
