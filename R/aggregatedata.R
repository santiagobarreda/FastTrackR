
#' Aggregate analysis data
#' 
#' Aggregate the .csv files contained in the csvs folder in a Fast Track directory. These files contain formant measurements at every 2ms, and there is one file per vowel token, so this function summarizes that data into a single dataframe with one row per token and measurements at a specified number of bins. 
#'
#' Note that if a file called aggregated_data.csv exists in the processed_data directory, this function will read that in instead—but only if the \code{csvs} object is not specified. If the file exists, but you want to process the data yourself (say, into 11 bins instead of 5), then include the \code{csvs} object in the function call. See the examples below. 
#' 
#' @param path a string. The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param csvs An object from the output of \code{readcsvs()}. 
#' @param bins an integer (default = 5). How many timepoints do you want formant data from? By default, you'll get formant samples at five points along the duration of each vowel. 
#' @param f0_bins an integer or string (default = 1). By default, the F0 values across the entire vowel token are summarized into a single value. However, if you are interested in F0 contours, you can specify how many measurements can be taken. This can be independent of the number of formant measurments. The value \code{"same"} will set this value equal to the \code{"bins"} argument. A value of 0 will result in no calculation of f0. See examples below.
#' @param n_formants an integer. By default, \code{aggregatedata} will use the number of formants as is contained in \code{csvs} or in the .csv files. However, if you want to, for example, only aggregated data from F1, F2, and F3 even though you have data from F4, you can do so by setting \code{n_formants} to \code{3}.
#' @param method a string (default = \code{"median"}). Determines what kind of summarization function is used when aggregating data. Other functions to come later.
#' @return A dataframe containing formant measurements and various other information for each file (= vowel token). The column called \code{f12} is the F1 measurement in the second bin. If only one F0 measurement is returned, the column will be named \code{f0}. Otherwise, it will follow the same convention (i.e. the F0 measurement for the third bin will be called \code{f03}).
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
#' 
#' # Get two F0 measurements per vowel token
#' aggregatedata(path, csvs, f0_bins = 2)
#' 
#' # Get 11 measurements for all formants and F0.
#' aggregatedata(path, csvs, bins = 11, f0_bins = "same")
#' }

aggregatedata <- function (path=NA, csvs=NA, bins = 5, f0_bins = 1, n_formants = NA, method = "median"){
  
  # Autofill parameters
  if (is.na(path)) path = getwd()
  if (f0_bins == "same") f0_bins = bins

  if (all(is.na(csvs))){
    csvs = readcsvs(path)
  } 
  
  f0present = FALSE
  if (sum(colnames(csvs)=="f0") > 0) f0present = TRUE
  
  # If the file exists already, read it in instead and be done
  aggregated_path = paste0(path, "/processed_data/aggregated_data.csv")
  if (file.exists(aggregated_path) & all(is.na(csvs))) {
    aggregated = utils::read.csv(aggregated_path)
    return(aggregated)
  }
  
  # Make sure the csvs object is correct
  if ((class(csvs)!="data.frame") | (attr(csvs,"object")!="csvs")) 
    stop ("Please load csvs using the readcsv function and set asone=TRUE.")
  
  # How many formants to process?
  if (is.na (n_formants)) {
    n_formants=4
    if (length (which(colnames(csvs)=="f4")) == 0) n_formants = 3
  }
  
  # What method should be used?
  if (method=="median") method = stats::median
  
  # Split csvs and get filenames
  tmp_csvs = split (csvs, csvs$file)
  files = names (tmp_csvs)
  
  # internal function to quickly calculate duration and bins
  tmp_csvs = lapply (tmp_csvs, function (x){
    tmp_time = x$time
    tmp_time = tmp_time - min(tmp_time)
    x$dur = max (tmp_time)
    tmp_time = tmp_time / max (tmp_time)
    tmp_time = 1+floor(tmp_time*(bins-.001))
    x$bin = tmp_time
    x
  })
  # rejoining data
  tmp_csvs = do.call (rbind, tmp_csvs)
  
  if (n_formants==3) tmp_agg = stats::aggregate (cbind (f1,f2,f3) ~ bin+file, tmp_csvs, method)  
  if (n_formants==4) tmp_agg = stats::aggregate (cbind (f1,f2,f3,f4) ~ bin, tmp_csvs, method)

  # aggregated bins into matrix for output
  aggregated = c(t(tmp_agg[,3:5]))
  aggregated =  data.frame(matrix (aggregated,length (files),bins*n_formants, byrow = TRUE))
  colnames (aggregated) = paste0 ("f",rep(1:n_formants,bins),if(bins>1)rep(1:bins,each=n_formants))
  rownames (aggregated) = 1:nrow(aggregated)
  
  # find duration
  duration = tapply (tmp_csvs$dur, tmp_csvs$file, max)
  
  # put parts together
  aggregated = cbind (file = files, duration, aggregated)
  
  # calculation of f0 if it applies
  if (f0_bins == 1 & f0present){
    f0 = stats::aggregate (f0 ~ file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)  
    aggregated = cbind (aggregated, f0 = f0$f0)
  }
  
  # calculation of f0 if it applies
  if ((f0_bins) > 1 & f0present){
    
    # Split csvs and get filenames
    tmp_csvs = split (csvs, csvs$file)
    files = names (tmp_csvs)
    
    # internal function to quickly calculate duration and bins
    tmp_csvs = lapply (tmp_csvs, function (x){
      tmp_time = x$time
      tmp_time = tmp_time - min(tmp_time)
      x$dur = max (tmp_time)
      tmp_time = tmp_time / max (tmp_time)
      tmp_time = 1+floor(tmp_time*(f0_bins-.001))
      x$bin = tmp_time
      x
    })
    # rejoining data
    tmp_csvs = do.call (rbind, tmp_csvs)
    
    f0 = stats::aggregate (f0 ~ bin+file, tmp_csvs, FUN = method, na.rm = TRUE, na.action = stats::na.pass)  
    f0 = data.frame (matrix (f0$f0, length(files), f0_bins, byrow = TRUE))
    colnames (f0) = paste0 ("f0",if(f0_bins>1)1:f0_bins) # only add the number if >1 bins
    
    aggregated = cbind (aggregated, f0)
  }
  
  # Add in information from the other file if it exists
  if (file.exists(paste0(path, "/file_information.csv"))){
    fileinfo = utils::read.csv(paste0(path, "/file_information.csv"))
    fileinfo$file = gsub("\\.wav", "", fileinfo$file) # strip off .wav
    aggregated = merge(fileinfo, aggregated, by="file", all.x=TRUE)
  }
  
  aggregated
}






