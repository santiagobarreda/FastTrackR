
#' Aggregate analysis data
#' 
#' Aggregate the .csv files contained in the csvs folder in a Fast Track directory. These files contain formant measurements at every 2ms, and there is one file per vowel token, so this function summarizes that data into a single dataframe with one row per token and measurements at a specified number of bins. 
#'
#' Note that if a file called aggregated_data.csv exists in the processed_data directory, this function will read that in instead—but only if the \code{csvs} object is not specified. If the file exists, but you want to process the data yourself (say, into 11 bins instead of 5), then include the \code{csvs} object in the function call. See the examples below. 
#' 
#' @param path a string. The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.#'
#' @param csvs An object from the output of \code{readcsvs()}. 
#' @param bins an integer. How many timepoints do you want formant data from? By default, you'll get formant samples at five points along the duration of each vowel. 
#' @param n_formants an integer. By default, \code{aggregatedata} will use the number of formants as is contained in \code{csvs} or in the .csv files. However, if you want to, for example, only aggregated data from F1, F2, and F3 even though you have data from F4, you can do so by setting \code{n_formants} to \code{3}.
#' @param method a string. Median by default. Other functions to come later.
#' 
#' @return A dataframe containing formant measurements and various other information for each file (=vowel token)
#' @export
#' @examples
#' \dontrun{
#' path <- "path/to/fasttrack/data"
#' 
#' # Read in the aggregated data
#' aggregatedata(path)
#' 
#' # Aggregate the csv files. 
#' # This generates a spreadsheet identical to the one produced by Praat.
#' csvs <- readcsvs(path)
#' aggregatedata(path, csvs)
#' 
#' # Reprocess existing csv data. Let's say that when I first analyzed the audio 
#' # in Praat using Fast Track, I only had the data binned into 5 timepoints. 
#' # Now, I want 11 timepoints, so I'll generate a new version of the aggregated data here.
#' aggregatedata(path, csvs, bins = 11)
#' 
#' # Only process the first three formants even though four are in the original csvs.
#' aggregatedata(path, csvs, n_formants = 3)
#' }

aggregatedata <- function (path, csvs, bins = 5, n_formants = NA, method = "median"){
  
  if (missing(path)) path = getwd()
  
  # If the file exists already, read it in instead and be done
  aggregated_path = paste0(path, "/processed_data/aggregated_data.csv")
  if (file.exists(aggregated_path) & missing(csvs)) {
    aggregated = utils::read.csv(aggregated_path)
    return(aggregated)
  }
  
  # Makes sure the csvs object is correct
  if ((class(csvs)!="data.frame") | (attr(csvs,"object")!="csvs")) 
    stop ("Please load csvs using the readcsv function and set asone=TRUE.")
  
  tmp_csvs = split (csvs, csvs$file)
  files = names (tmp_csvs)
  
  # How many formants to process?
  if (is.na (n_formants)){
    n_formants=4
    if (length (which(colnames(csvs)=="f4")) == 0) n_formants = 3
  }
  
  # What method should be used?
  if (method=="median") method = stats::median
  
  # Create an empty matrix
  aggregated = data.frame (matrix (0, length(tmp_csvs), bins*n_formants))
  duration = matrix (0, length(tmp_csvs), 1)
  colnames (aggregated) = paste0 ("f",rep(1:n_formants,bins),rep(1:bins,each=n_formants))
  
  # Loop through the files and populate that matrix with formant measurements
  for (i in 1:length (tmp_csvs)){
    n = nrow (tmp_csvs[[i]])
    tmp_csvs[[i]]$ntime = ceiling((1:n)/n*bins)
    
    if (n_formants==3) tmp_agg = stats::aggregate (cbind (f1,f2,f3) ~ ntime, tmp_csvs[[i]], method)
    if (n_formants==4) tmp_agg = stats::aggregate (cbind (f1,f2,f3,f4) ~ ntime, tmp_csvs[[i]], method)
    
    aggregated[i,] = round (c(t(tmp_agg[,-1])))
    duration[i] = diff(range(tmp_csvs[[i]]$time))
  }
  
  # add the filename and duration to the formants
  aggregated = cbind (file = files, duration = duration, aggregated)
  
  # Add in the other file information 
  fileinfo = utils::read.csv(paste0(path, "/file_information.csv"))
  fileinfo$file = gsub("\\.wav", "", fileinfo$file) # strip off .wav
  aggregated = merge(fileinfo, aggregated, by="file", all.x=TRUE)
  
}