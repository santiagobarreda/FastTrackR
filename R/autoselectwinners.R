
#' Automatically select winning analyses
#'
#'
#' @param formants a list of formant data read in with the readformants function.
#' @param order the order of the prediction model.
#' @param n_formants the number of formants to optimize for.
#' @param method method of selecting the winning analysis.
#' @param outputpath --.
#' @param subset a vector indicating a subset of the analyses to be considered.
#' @return A vector with the winning analysis for each file.
#' @export
#' @examples
#' \dontrun{
#' formants = readformants ()
#' winners = autoselectwinners (formants, outputpath="working")
#' }

autoselectwinners <- function (formants, order = 5, n_formants = 4, method = "classic",
                               outputpath = NA, subset = NA){

  if (method=="classic")
    output = autoselect.classic (formants, order = order, n_formants = n_formants, subset = subset)
  if (method!="classic")
    stop("Method not supported.")
  
 
  if (!is.na (outputpath) & is.na (subset[1])){
    if (outputpath == "working") outputpath = getwd()
    autoselect.write (outputpath, output)
  }
  
  output$winners_csv
}

