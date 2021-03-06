
#' Load Fast Track csv files
#'
#' This function quickly reads in all the files contained in the "/csvs" folder within a Fast Track directory. Since Fast Track exports the acoustic measurements into separate files per token, this function is a quick way to read those all in at once.
#' 
#' Note that missing F0 data is stored as \code{0} in the .csv files. These are converted to \code{NA} when read in using this function.
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param fileinformation --.
#' @param progressbar if TRUE, a progress bar prints out in the console.
#' @param write --.
#' @return A dataframe or list of dataframes, as determined by the \code{asone} parameter.
#' @export
#' @examples
#' \dontrun{
#' csvs <- readcsvs()
#' }

readcsvs <- function (path, fileinformation = NA, progressbar = TRUE,write = FALSE){
  
  if (missing(path)) path = getwd()
  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  s_files = tools::file_path_sans_ext (list.files (paste0(path,"/csvs")))
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  n_files = length(files)
  start = Sys.time()
  csvs = lapply (1:n_files, function(i){
    if (progressbar) progressbar(i,n_files, start)
    tmp = utils::read.csv (files[i], na.strings = "0")
    tmp$file = file_names[i]
    #attr(tmp, "filename") = s_files[i]
    
    tmp
  })
  csvs = do.call (rbind, csvs)
  
  if (write) saveRDS (csvs, "csvs.RDS")

  return (csvs)
}
