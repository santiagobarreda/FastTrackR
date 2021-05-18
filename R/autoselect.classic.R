
autoselect.classic <- function (formants, order = 5, nf = 4, subset = NA){
  
  n = length (formants)
  nsteps = length (formants[[1]])
  nf = ncol (formants[[1]][[1]])/2
  
  steps = 1:nsteps
  if (!is.na(subset[1])) steps = subset
  
  files = sapply (formants, attr, "filename")
  
  errors = array (0, dim = c(n, nsteps, nf))
  coefficients = array (0, dim = c(n, nsteps, nf, order+1))
  
  for (i in 1:n){
    for (j in steps){
      y = as.matrix(formants[[i]][[j]][,1:nf])
      xs = makepredictors (nrow (y), order = order)
      mod = stats::lm (y ~ 0 + xs)
      
      errors[i,j,] = errors[i,j,] + apply (mod$residuals,2,stats::sd)
      coefficients[i,j,,] = t(mod$coefficients)
    }
  }
  total_errors = apply (errors,c(1,2), sum)
  winners = apply (total_errors, 1, which.min)
  winners = steps[winners]
  
  errors = round (errors,1)
  total_errors = round (total_errors,1)
  coefficients = round (coefficients,1)
  winners_csv = data.frame (file = files, winner = winners, F1=winners,
                       F2=winners, F3=winners)
  if (nf==4) winners_csv[["F4"]] = winners
 
  output = list (winners_csv=winners_csv,
                 errors=errors,total_errors=total_errors,
                 coefficients=coefficients)
  output
}

