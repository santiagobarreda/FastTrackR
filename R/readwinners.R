
#' Load information about the winning analyses
#'
#' This function reads in the winners.csv file from a specified path, or assumes it is located in the working directory if not path is specified. 
#' 
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @return A dataframe containing information about winning analyses.
#' @export
#' @examples
#' \dontrun{
#' winners <- readwinners()
#' }

readwinners <- function (path = NA){
  
  if (is.na(path)) path = getwd()
      
  winners_csv = utils::read.csv (paste0 (path, "/winners.csv"))
  
  return (winners_csv)
 
}
