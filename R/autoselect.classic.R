
#' Automatically select winners
#' 
#' Select the best analyses using the 'classic' Fast Track method from Praat. Generates identical output to the autoselect step in Praat, except for no regression information text files are written (for now). 
#'
#' @param formants a list of formant data read in with the readformants function.
#' @param order the order of the prediction model.
#' @param n_formants the number of formants to optimize for.
#' @param outputpath if NA, nothing is written out. If "working", data is written out to the working directory. Any other path may also be specified.
#' @param subset a vector indicating a subset of the analyses to be considered.
#' @param progressbar if TRUE, a progress bar prints out in the console.
#' @return A list containing information about the selection of the winners. The list elements are:
#' 
#' 1) winners_csv: A dataframe containing the same information as the winners.csv file. 
#' 
#' 2) errors: A 3d array containing information about the RMS error for each analysis for each formant. Dimensions are [file, analysis, formant]. 
#' 
#' 3) total_errors: A 2d array containing information about the total RMS error for each analysis. Dimensions are [file, analysis]. 

#' 4) coefficients: A 4d array containing information about regression coefficients for prediction of each formant for each analysis. Dimensions are [file, analysis, formant, coefficient]. Coefficients are arranged in terms of increasing order (i.e., intercept, linear term, quadratric term,...). 

#' @export
#' @examples
#' \dontrun{
#' formants = readformants ()
#' winners = autoselect.classic (formants, progressbar = TRUE)
#' winners = autoselect.classic (formants, outputpath="working")
#' }

autoselect.classic <- function (formants, order = 5, n_formants = 4, 
                                outputpath = NA, subset = NA, progressbar = FALSE){
  n_files = length (formants)
  n_steps = length (formants[[1]])
  n_formants = ncol (formants[[1]][[1]])/2
  
  # if only a subset of analyses are going to be checked
  steps = 1:n_steps
  if (!is.na(subset[1])) steps = subset
  
  # track file names
  files = sapply (formants, attr, "filename")
  
  # empty matrix for errors
  errors = array (0, dim = c(n_files, n_steps, n_formants))
  # empty 4d array for analysis regression coefficients 
  # d1 = file, d2 = analysis, d3 = formant, d4 = coefficient
  coefficients = array (0, dim = c(n_files, n_steps, n_formants, order+1))
  
  # for each file and analysis step
  for (i in 1:n_files){
    cat ("\nSelecting best Analyses... \n")
    
    if (progressbar) progressbar (i,n_files)
    for (j in steps){
      y = as.matrix(formants[[i]][[j]][,1:n_formants])
      xs = makepredictors (nrow (y), order = order)
      mod = stats::lm (y ~ 0 + xs)
      
      errors[i,j,] = errors[i,j,] + apply (mod$residuals,2,stats::sd)
      coefficients[i,j,,] = t(mod$coefficients)
    }
  }
  total_errors = apply (errors,c(1,2), sum)
  winners = apply (total_errors, 1, which.min)
  winners = steps[winners]
  
  errors = round (errors,1)
  total_errors = round (total_errors,1)
  coefficients = round (coefficients,1)
  winners_csv = data.frame (file = files, winner = winners, F1=winners,
                            F2=winners, F3=winners)
  if (n_formants==4) winners_csv[["F4"]] = winners
  
  output = list (winners_csv=winners_csv,
                 errors=errors,total_errors=total_errors,
                 coefficients=coefficients)
  class (output) = "selection_info"
  attr(output, "path") = attr(formants,"path")
  attr(output, "n_files") = dim(coefficients)[1]
  attr(output, "n_coefficients") = dim(coefficients)[4]-1
  attr(output, "n_cutoffs") = dim(coefficients)[2]
  
  
  if (!is.na (outputpath) & is.na (subset[1])){
    if (outputpath == "working") outputpath = getwd()
    cat ("Writing data files... \n")
    autoselect.write (outputpath, output)
    cat ("\nDone. \n")
    
  }
  return (output)
  #output$winners_csv
}


