
#' Load information about the selection of the winning analyses
#'
#' This function reads in the winners.csv file from a specified path, or assumes it is located in the working directory if not path is specified. 
#' 
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param justcsv The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @return A dataframe containing information about winning analyses.
#' @export
#' @examples
#' \dontrun{
#' winners <- readwinners()
#' }

readselectioninfo <- function (path = NA, justcsv = FALSE){
  
  if (is.na(path)) path = getwd()
      
  winners_csv = utils::read.csv (paste0 (path, "/winners.csv"))
  
  if (justcsv) return (winners_csv)
  
  if (!justcsv){
    total_errors = utils::read.csv (paste0 (path, "/infos_aggregated/all_errors.csv"))
    total_errors = as.matrix (total_errors)
    total_errors = unname(total_errors)
    
    filenames = list.files(paste0 (path, "/regression_infos"), pattern="*.txt", full.names=TRUE)
    n_files = length (filenames)
    
    regression_data = lapply(filenames, utils::read.csv)
    
    n_cutoffs = ncol(total_errors)
    n_formants = as.numeric (substr (regression_data[[1]][1,],nchar(regression_data[[1]][1,]),nchar(regression_data[[1]][1,])))
    n_coefficients = as.numeric (substr (regression_data[[1]][2,],nchar(regression_data[[1]][2,]),nchar(regression_data[[1]][2,])))
    
    coefficients = array (coefficients, dim = c(n_files, n_cutoffs, n_formants, n_coefficients+1))
    errors = array (coefficients, dim = c(n_files, n_cutoffs, n_formants))
    
    for (i in 1:n_files){
      tmp = regression_data[[i]][-c(1:2,nrow(regression_data[[i]])),]
      tmp_errors = tmp[seq (1, n_cutoffs*(n_formants+1),5)]
      tmp_coefficients = tmp[-seq (1, n_cutoffs*(n_formants+1),5)]
      
      tmp_errors = do.call (rbind, strsplit (tmp_errors," "))
      errors[i,,] = apply(tmp_errors, 2, as.numeric)
      
      tmp_coefficients = do.call (rbind, strsplit (tmp_coefficients," "))
      tmp_coefficients = apply(tmp_coefficients, 2, as.numeric)
      
      ord = c(seq (1,n_formants*n_cutoffs,n_formants),seq (2,n_formants*n_cutoffs,n_formants),
              seq (3,n_formants*n_cutoffs,n_formants))
    
      if (n_formants==4) ord = c(ord, seq (4,n_formants*n_cutoffs,n_formants))
      tmp_coefficients = tmp_coefficients[ord,]
    
      coefficients[i,,,] = array (tmp_coefficients, dim = c(n_cutoffs, n_formants, n_coefficients+1))
    }  
    
    winners = list (winners_csv = winners_csv, errors = errors, total_errors = total_errors, coefficients = coefficients)
    class (winners) = "selection_info"
    attr(winners, "path") = path
    attr(winners, "n_files") = n_files
    attr(winners, "n_coefficients") = n_coefficients
    attr(winners, "n_cutoffs") = n_cutoffs
    return (winners)
  }
}



#' @export
print.selection_info = function (x, ...){
  cat ("Winner selection information for data in: \n")
  cat (attr(x, "path"),"\n\n")
  
  cat ("A list with four elements: \n\n")
  cat ("1) winners_csv: dataframe with the information in the winners.csv file.\n\n")
  cat ("2) errors: 3d matrix representing formantwise errors for each formant, analysis, and file.\n\n")
  cat ("3) total_errors: 2d matrix representing total errors for each analysis and each file.\n\n")
  cat ("4) coefficients: 4d matrix representing regression coefficients for each formant, analysis, and file.\n\n")
  
  cat ("See the help file for the autoselect.classic function for more information.\n")
}






