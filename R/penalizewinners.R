
#' Impose boundaries on formants
#'
#' Create an updated winners dataframe or file with imposed penalties on formants. Penalties can be determined with the imposeboundaries or imposeheuristics functions included in this package. 
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param selectioninfo --. 
#' @param write if TRUE, a new winners file will be written to your folder, reflecting the penalties. 
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#'  # coming soon
#' }

penalizewinners <- function (path, selectioninfo = NA, write = FALSE){
  
  if (missing(path)) path = getwd()
  
  if (is.na(selectioninfo))
    if (class(selectioninfo) != "selection_info") 
      stop ("Invalid selectioninfo object. Please read in using the readselectioninfo function.")
  
  if (is.na(selectioninfo)){
    selectioninfo <- readselectioninfo()
  }
  
  errors = selectioninfo$total_errors 
  winners = selectioninfo$winners 
  
  
  if (is.na(fileinformation)){
    if (!file.exists (path %+% "/file_information.csv")) stop ("No file information file available.")
    fileinformation = utils::read.csv (path %+% "/file_information.csv")
  }
  
  winners = selectioninfo$winners_csv
  
  for (i in 1:nrow ()){
    use = selectioninfo$penalties[i,] > 0
    # if there is at least one usable one:
    if (sum (use) > 0){
      # find ranking of analyses
      ord = order (unlist(errors[i,]))
      # take first useable one
      winners[i,2:ncol(winners)] = ord[use][1]
    }
    if (sum (use) == 0)
      winners[i,2:ncol(winners)] = order (unlist(errors[i,]))[1]
  }
  selectioninfo$winners_csv = winners
  
  # write out if user desires
  if (write) utils::read.csv (winners, path %+% "/winners.csv")
  
  invisible (winners)
}
