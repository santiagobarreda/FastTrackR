
#' Aggregate analysis data
#'
#' @param csvs --.
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param bins --.
#' @param nf --.
#' @param method --.
#' @return --.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs (asone=TRUE)
#' }

aggregatedata <- function (path, csvs, bins = 5, nf = NA, method = "median"){
  
  if (missing(path)) path = getwd()
  
  # If the file exists already, read it in and be done
  aggregated_path = paste0(path, "/processed_data/aggregated_data.csv")
  if (file.exists(aggregated_path)) {
    aggregated = utils::read.csv(aggregated_path)
    return(aggregated)
  }
  
  if((class(csvs)!="data.frame") | (attr(csvs,"object")!="csvs")) stop ("Please load csvs using the readcsv function and set asone=TRUE.")
  
  tmp_csvs = split (csvs, csvs$file)
  files = names (tmp_csvs)
  
  if (is.na (nf)){
    nf=4
    if (length (which(colnames(csvs)=="f4")) == 0) nf = 3
  }
  
  if (method=="median") method = stats::median
  
  aggregated = data.frame (matrix (0, length(tmp_csvs), bins*nf))
  duration = matrix (0, length(tmp_csvs), 1)
  colnames (aggregated) = paste0 ("f",rep(1:nf,bins),rep(1:bins,each=nf))
  
  for (i in 1:length (tmp_csvs)){
    n = nrow (tmp_csvs[[i]])
    tmp_csvs[[i]]$ntime = ceiling((1:n)/n*bins)
    
    if (nf==3) tmp_agg = stats::aggregate (cbind (f1,f2,f3) ~ ntime, tmp_csvs[[i]], method)
    if (nf==4) tmp_agg = stats::aggregate (cbind (f1,f2,f3,f4) ~ ntime, tmp_csvs[[i]], method)
    
    aggregated[i,] = round (c(t(tmp_agg[,-1])))
    duration[i] = diff(range(tmp_csvs[[i]]$time))
  }
  
  aggregated = cbind (file = files, duration = duration, aggregated)
  
  # Add in the file information 
  fileinfo = utils::read.csv(paste0(path, "/file_information.csv"))
  fileinfo$file = gsub("\\.wav", "", fileinfo$file) # strip off .wav
  aggregated = merge(fileinfo, aggregated, by="file", all.x=TRUE)
  
}