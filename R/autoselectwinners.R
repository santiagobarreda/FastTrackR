
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

  output = data.frame (file = files, winner = winners, F1=winners,
                       F2=winners, F3=winners)

  if (nf==4) output[["F4"]] = winners

  if (!is.na (outputpath) & is.na (subset[1])){
    if (outputpath == "working") outputpath = getwd()
    utils::write.csv (output, outputpath %+% "/winners.csv", row.names=FALSE)

    colnames(total_errors) = paste0("e",1:ncol(total_errors))
    colnames(coefficients) = paste0("a",1:dim(coefficients)[2])
    dir.create (outputpath %+% "/infos_aggregated/", showWarnings = FALSE)

    utils::write.csv (total_errors, outputpath %+% "/infos_aggregated/all_errors.csv", row.names=FALSE)
    utils::write.csv (coefficients[,,1,1], outputpath %+% "/infos_aggregated/all_f1s.csv", row.names=FALSE)
    utils::write.csv (coefficients[,,2,1], outputpath %+% "/infos_aggregated/all_f2s.csv", row.names=FALSE)
    utils::write.csv (coefficients[,,3,1], outputpath %+% "/infos_aggregated/all_f3s.csv", row.names=FALSE)

    # if list of winners is identical to info files, write out
    info_files = list.files (outputpath %+% "/infos/")
    if (identical (files, unlist (strsplit(info_files,split='_'))[c(T,F)])){
      for (i in 1:length (info_files)){
        tmp_info  = readLines (outputpath %+% "/infos/" %+% info_files[i])
        tmp_info[11] = winners[i]

        errors_out = t(cbind(total_errors[i,],errors[i,,1:nf]))
        errors_out = apply (errors_out, 1, paste, collapse = " ")
        tmp_info[13:(12+length(errors_out))] = errors_out

        coeffs_out = coefficients[i,winners[i],,]
        coeffs_out = apply (coeffs_out, 1, paste, collapse = " ")

        spot=which(tmp_info=="Coefficients are (row-wise by formant):")
        tmp_info = c(tmp_info[1:spot],coeffs_out)

        writeLines (tmp_info, outputpath %+% "/infos/" %+% info_files[i])
      }
    }
  }
  output
}

