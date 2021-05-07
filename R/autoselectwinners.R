
#' Automatically select winning analyses
#'
#'
#' @param formants a list of formant data read in with the readformants function.
#' @param order the order of the prediction model.
#' @param method method of selecting the winning analysis.
#' @param subset a vector indicating a subset of the analyses to be considered.
#' @return A vector with the winning analysis for each file.
#' @export
#' @examples
#' \dontrun{
#' formants = readformants ()
#' autoselectwinners (formants)
#' }

autoselectwinners <- function (formants, order = 5, method = "classic", subset = NA){

  n = length (formants)
  nsteps = length (formants[[1]])
  nf = ncol (formants[[1]][[1]])/2

  steps = 1:nsteps
  if (!is.na(subset)) steps = subset

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

  ## output needs to be the same as in Praat output
  winners = apply (errors, 1, which.min)
  winners = steps[winners]
  winners
}


makepredictors = function (n, order){
  x = (0:(n-1)) / n
  xs = sapply (seq (0,order/2,.5), function (f) cos (f*2*pi*x))
}

