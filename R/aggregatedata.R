
#' Aggregate analysis data
#'
#' @param csvs --.
#' @param bins --.
#' @param nf --.
#' @param method --.
#' @return --.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs (asone=TRUE)
#' }

aggregatedata <- function (csvs, bins = 5, nf = NA, method = "median"){

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
  colnames (aggregated) = paste0 ("f",rep(1:4,bins),rep(1:bins,each=nf))

  for (i in 1:length (tmp_csvs)){
    n = nrow (tmp_csvs[[i]])
    tmp_csvs[[i]]$ntime = ceiling((1:n)/n*bins)

    if (nf==3) tmp_agg = stats::aggregate (cbind (f1,f2,f3) ~ ntime, tmp_csvs[[i]], method)
    if (nf==4) tmp_agg = stats::aggregate (cbind (f1,f2,f3,f4) ~ ntime, tmp_csvs[[i]], method)

    aggregated[i,] = round (c(t(tmp_agg[,-1])))
    duration[i] = diff(range(tmp_csvs[[i]]$time))
  }

  aggregated = cbind (file = files, duration = duration, aggregated)

}
