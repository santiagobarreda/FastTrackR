
# thoughts on organization:
# actually finds the winners given the formant file. 
# one function for each method, called by the user controlled function
# output is a list containing all the information needed in other places: 
#  the winners table, information about all errors and information about all regression coefficients. 

autoselect.classic <- function (formants, order = 5, n_formants = 4, subset = NA){
  
  n_files = length (formants)
  n_steps = length (formants[[1]])
  n_formants = ncol (formants[[1]][[1]])/2
  
  # if only a subset of analyses are going to be checked
  steps = 1:n_steps
  if (!is.na(subset[1])) steps = subset
  
  # track file names
  files = sapply (formants, attr, "filename")
  
  # empty matrix for errors
  errors = array (0, dim = c(n_files, n_steps, n_formants))
  # empty 4d array for analysis regression coefficients 
  # d1 = file, d2 = analysis, d3 = formant, d4 = coefficient
  coefficients = array (0, dim = c(n_files, n_steps, n_formants, order+1))
  
  # for each file and analysis step
  for (i in 1:n_files){
    for (j in steps){
      y = as.matrix(formants[[i]][[j]][,1:n_formants])
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
  if (n_formants==4) winners_csv[["F4"]] = winners
 
  output = list (winners_csv=winners_csv,
                 errors=errors,total_errors=total_errors,
                 coefficients=coefficients)
  output
}

