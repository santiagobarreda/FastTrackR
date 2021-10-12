
#' Update winners after changing penalties
#'
#' 
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param selection_info --.
#' @param write if TRUE, ....
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#' getwinners ()
#' tmp = getwinners (getwd(), winners$winners, formants, write = TRUE)
#' }

updatewinners <- function (path = NA, selection_info = NA, write = FALSE){

  if (missing(path)) path = getwd()
  
  selection_info_exists = FALSE
  if (all(!is.na(selection_info))) selection_info_exists = TRUE
  
  if (!selection_info_exists & file.exists(path %+% "/selection_info.RDS")){ 
    selection_info = readRDS (path %+% "/selection_info.RDS")
    selection_info_exists = TRUE
  }
  nf = 3 + sum (colnames(selection_info$winners_csv)=="F4")
  
  
  if (!selection_info_exists) stop ("No selection information provided or available.")

  winners = selection_info$winners_csv
  labels = selection_info$labels
  
  total_errors = selection_info$total_errors
  penalties = selection_info$penalties
  
  total_errors = total_errors + penalties*5*max(total_errors)
  
  winners = apply (total_errors, 1, which.min)
  selection_info$winners_csv[,2:(nf+2)] = winners
  
  return (selection_info)
}

