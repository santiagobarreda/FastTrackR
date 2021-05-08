
#' Automatically select winning analyses
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
#' formants = readformants ()
#' winners = autoselectwinners (formants, outputpath="working")
#' }

autoselectwinners <- function (formants, order = 5, nf = 4, method = "classic",
                               outputpath = NA, subset = NA){

  n = length (formants)
  nsteps = length (formants[[1]])
  nf = ncol (formants[[1]][[1]])/2

  steps = 1:nsteps
  if (!is.na(subset)) steps = subset

  files = sapply (formants, attr, "filename")

  errors = matrix (0, n, nsteps)
  for (i in 1:n){
    for (j in steps){
      for (k in 1:nf){
        tmp_ff = formants[[i]][[j]][,k]
        xs = makepredictors (length (tmp_ff), order = order)
        mod = stats::lm (tmp_ff ~ 0 + xs)
        errors[i,j] = errors[i,j] + stats::sd(mod$residuals)
      }
    }
  }

  winners = apply (errors, 1, which.min)
  winners = steps[winners]

  output = data.frame (file = files, winner = winners, F1=winners,
                       F2=winners, F3=winners)
  if (nf==4) output[["F4"]] = winners

  if (!is.na (outputpath)){
    if (outputpath == "working") outputpath = getwd()
    utils::write.csv (output, outputpath %+% "/winners.csv", row.names=FALSE)
  }

  output
}


makepredictors = function (n, order){
  x = (0:(n-1)) / n
  xs = sapply (seq (0,order/2,.5), function (f) cos (f*2*pi*x))
}
