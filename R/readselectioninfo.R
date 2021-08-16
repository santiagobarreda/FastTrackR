
#' Load information about the selection of the winning analyses
#'
#' This function reads in the winners.csv file from a specified path, or assumes it is located in the working directory if not path is specified. 
#' 
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @return A dataframe containing information about winning analyses.
#' @export
#' @examples
#' \dontrun{
#' winners <- readselectioninfo()
#' }

readselectioninfo <- function (path = NA){
  
  if (is.na(path)) path = getwd()
  
  if (!file.exists (path %+% "/winners.csv"))
    stop ("No winners.csv file exists. Did you run the autoselect step already?")
    
  winners_csv = utils::read.csv (paste0 (path, "/winners.csv"))

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
    tmp_errors = tmp[seq (1, n_cutoffs*(n_formants+1),n_formants+1)]
    tmp_coefficients = tmp[-seq (1, n_cutoffs*(n_formants+1),n_formants+1)]
    
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
  
  if (!file.exists (path %+% "/penalties.csv")){
    penalties = matrix (0,nrow(winners_csv),ncol(errors))
    rownames (penalties) = basename(winners_csv$file)
  }
  if (file.exists (path %+% "/penalties.csv"))  penalties = utils::read.csv (path %+% "/penalties.csv")
  
  
  if (file.exists (path %+% "/file_information.csv")){
    file_information = utils::read.csv (path %+% "/file_information.csv")
    labels = file_information$label
    names (labels) = file_information$file
  }
  if (!file.exists (path %+% "/file_information.csv")) labels = NA
  

  selectioninfo = list (winners_csv = winners_csv, errors = errors, total_errors = total_errors, 
                  coefficients = coefficients, penalties = penalties, labels = labels)
  
  class (selectioninfo) = "selection_info"
  attr(selectioninfo, "path") = path
  attr(selectioninfo, "n_files") = n_files
  attr(selectioninfo, "n_coefficients") = n_coefficients
  attr(selectioninfo, "n_cutoffs") = n_cutoffs
  return (selectioninfo)
}




#' @export
print.selection_info = function (x, ...){
  cat ("Analysis selection information for data in: \n")
  cat (attr(x, "path"),"\n\n")
  
  cat ("A list with the following elements: \n\n")
  cat ("1) winners_csv: dataframe with information from the winners.csv file.\n\n")
  cat ("2) errors: 3d matrix with formantwise errors for each formant, analysis, and file.\n\n")
  cat ("3) total_errors: 2d matrix with total errors for each analysis and each file.\n\n")
  cat ("4) coefficients: 4d matrix with regression coefficients for each formant, analysis, and file.\n\n")
  cat ("5) penalties: 2d matrix of penalties imposed due to heuristic or boundary violations.\n\n")
  cat ("6) labels: labels for each file analyzed (if available).\n\n")
  
  cat ("See the help file for the autoselect.classic function for more information.\n")
}






