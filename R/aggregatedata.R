
#' Aggregate analysis data
#'
#'
#' @param formants a list of formant data read in with the readformants function.
#' @param order the order of the prediction model.
#' @param nf the number of formants to optimize for.
#' @param method method of selecting the winning analysis.
#' @param outputpath --.
#' @param subset a vector indicating a subset of the analyses to be considered.
#' @return A vector with the winning analysis for each file.
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

  aggregated = data.frame (matrix (0, length(tmp_csvs), bins*nf))
  duration = matrix (0, length(tmp_csvs), 1)
  colnames (aggregated) = paste0 ("f",rep(1:4,bins),rep(1:bins,each=nf))

  for (i in 1:length (tmp_csvs)){
    n = nrow (tmp_csvs[[i]])
    tmp_csvs[[i]]$ntime = ceiling((1:n)/n*bins)

    if (nf==3) tmp_agg = aggregate (cbind (f1,f2,f3) ~ ntime, tmp_csvs[[i]], method)
    if (nf==4) tmp_agg = aggregate (cbind (f1,f2,f3,f4) ~ ntime, tmp_csvs[[i]], method)

    aggregated[i,] = round (c(t(tmp_agg[,-1])))
    duration[i] = diff(range(tmp_csvs[[i]]$time))
  }

  aggregated = cbind (file = files, duration = duration, aggregated)

}
