
#' Impose boundaries on formants
#'
#' Create an updated winners dataframe or file with imposed formant boundaries.
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param boundaries a dataframe representing the boundaries for your formants for each vowel.
#' @param selectioninfo --.
#' @param write if TRUE, a new winners file will be written to your folder. 
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#'  # coming soon
#' }


imposebounds <- function (path, boundaries=NA, selectioninfo = NA, write = TRUE){
  if (missing(path)) path = getwd()

  # or put in some default ones?
  if (is.na(boundaries)) stop ("No boundaries provided. see data(formantbounds) for a template.")

  if (!is.na(selectioninfo))
    if (class(selectioninfo) != "selection_info") 
      stop ("Invalid selectioninfo object. Please read in using the readselectioninfo function.")
  
  if (is.na(selectioninfo)){
    selectioninfo <- readselectioninfo()
  }
  
  n_formants = dim(selectioninfo$errors)[3]
  
  f1s = selectioninfo$coefficients[,,1,1]
  f2s = selectioninfo$coefficients[,,2,1]
  f3s = selectioninfo$coefficients[,,3,1]
  if (n_formants==4) f4s = selectioninfo$coefficients[,,4,1]
  
  errors = selectioninfo$total_errors 
 
  penalties = selectioninfo$penalties
  
  for (i in 1:nrow (penalties)){
    # see if sound label is in boundary information
    spot = which (selectioninfo$label[i] == boundaries$label)
    # if yes:
    if (length(spot) > 0){
      # check if each analysis falls within the bounds
      f1out = boundaries$f1lower[spot] > f1s[i,] |
              boundaries$f1upper[spot] < f1s[i,]
      f2out = boundaries$f2lower[spot] > f2s[i,] |
              boundaries$f2upper[spot] < f2s[i,]
      f3out = boundaries$f3lower[spot] > f3s[i,] |
              boundaries$f3upper[spot] < f3s[i,]
      
      # only consider those which do not defy the bounds
      penalties[i,] = penalties[i,] + as.numeric(f1out | f2out | f3out)
    }
   }
  
  selectioninfo$penalties = penalties
  
  # write out if user desires
  if (write) utils::write.csv (penalties, path %+% "/penalties.csv")
  
  invisible (selectioninfo)
}

