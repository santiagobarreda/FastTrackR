
#' Automatically select winning analyses
#'
#'
#' @param formants a list of formant data read in with the readformants function.
#' @param order the order of the prediction model.
#' @param method method of selecting the winning analysis.
#' @return A vector with the winning analysis for each file.
#' @export
#' @examples
#' \dontrun{
#' formants = readformants ()
#' autoselectwinners (formants)
#' }

autoselectwinners <- function (formants, order = 5, method = "classic"){

  n = length (formants)
  nsteps = length (formants[[1]])
  nf = ncol (formants[[1]][[1]])/2

  errors = matrix (0, n, nsteps)
  for (i in 1:n){
    for (j in 1:nsteps){
      for (k in 1:nf){
        tmp_ff = formants[[i]][[j]][,k]
        xs = makepredictors (length (tmp_ff), order = order)
        mod = lm (tmp_ff ~ 0 + xs)
        errors[i,j] = errors[i,j] + sd(mod$residuals)
      }
    }
  }
  winners = apply (errors, 1, which.min)

}


makepredictors = function (n, order){
  x = (0:(n-1)) / n
  xs = sapply (seq (0,order/2,.5), function (f) cos (f*2*pi*x))
}

