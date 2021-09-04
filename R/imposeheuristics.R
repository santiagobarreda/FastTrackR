
#' Impose boundaries on formants
#'
#' Create an updated winners dataframe or file with imposed penalties on formants. Penalties can be determined with the imposeboundaries or imposeheuristics functions included in this package. 
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param f1max --. 
#' @param f4min --. 
#' @param rhotic --. 
#' @param minF3F4 --. 
#' @param selectioninfo --. 
#' @param write if TRUE, a new winners file will be written to your folder, reflecting the penalties. 
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#'  # coming soon
#' }

imposeheuristics <- function (path=NA, f1max=1200, f4min=3800, rhotic=c(2000,500), minF3F4=c(500,1500), 
                              selectioninfo=NA, write = FALSE){
  
  if (is.na(path)) path = getwd()
  
  if (all(is.na(selectioninfo)))
    if (class(selectioninfo) != "selection_info") 
      stop ("Invalid selectioninfo object. Please read in using the readselectioninfo function.")

  if (all(is.na(selectioninfo))){
    selectioninfo <- readselectioninfo()
  }

  ## number of formants from error matrix
  n_formants = dim(selectioninfo$errors)[3]
  
  ## information about intercepts, used to estimate average formant frequencies
  f1s = selectioninfo$coefficients[,,1,1]
  f2s = selectioninfo$coefficients[,,2,1]
  f3s = selectioninfo$coefficients[,,3,1]
  if (n_formants==4) f4s = selectioninfo$coefficients[,,4,1]
  
  ## the penalty matrix. Anything over 0 means it is disqualified.
  penalties = selectioninfo$penalties
  
  for (i in 1:nrow (penalties)){
    
    if (!is.na (f1max)){
      penalties[i,] = penalties[i,] + as.numeric(f1s[i,]>f1max)
    }
    if (!is.na (f4min) & n_formants==4){
      penalties[i,] = penalties[i,] + as.numeric(f4s[i,]<f4min)
    }
    if (!is.na (rhotic[1])){
      part1 = f3s[i,]<2000
      part2 = (f2s[i,]-f1s[i,]) < 500
      
      penalties[i,] = penalties[i,] + as.numeric(part1 & part2)
    }
    if (!is.na (minF3F4[1]) & n_formants==4){
      part1 = (f4s[i,]-f3s[i,]) < 500
      part2 = (f2s[i,]-f1s[i,]) < 1500

      penalties[i,] = penalties[i,] + as.numeric(part1 & part2)
    }
  }
  selectioninfo$penalties = penalties
  
  ## winners need to be updated here based on penalties and minimum error
  
  # write out if user desires
  if (write) utils::read.csv (penalties, path %+% "/penalties.csv")
  
  invisible (selectioninfo)
}
