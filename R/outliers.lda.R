
#' Find outliers using Linear Discriminant Analysis 
#'
#'  This is mostly a placeholder.
#'
#' @param aggregated_data --.
#' @return --.
#' @export
#' @examples
#' \dontrun{
#'  # coming soon
#' }

outliers.lda <- function (aggregated_data){
  
  colnms = colnames(aggregated_data)
  start = which(colnms=="f11")
  colnms = colnms[nchar (colnms) == 3]
  nf = substr (colnms,2,2)
  nf = suppressWarnings(nf[!is.na(as.numeric(nf))])
  nf = max (as.numeric (nf))
  nt = substr (colnms,3,3)
  nt = suppressWarnings(nt[!is.na(as.numeric(nt))])
  nt = max (as.numeric (nt))
  end = start+nf*nt-1

  ffs = aggregated_data[,which(colnames(aggregated_data)=="f11"):ncol(aggregated_data)]
  ffs = log (as.matrix (ffs))
  
  label = aggregated_data$label
  
  means = stats::aggregate (ffs ~ label, FUN = mean)[,-1]
  idx = as.numeric(factor(label))
  
  centered_ffs = as.matrix(ffs - means[idx,])
  est_cov = stats::cov(centered_ffs)
  
  distances = 
    sapply (1:nrow(ffs), function (j) 
          sapply (1:nrow (means), 
                  function (i) stats::mahalanobis(ffs[j,], unlist(means[i,]),est_cov)))
  distances= t(distances)
  
  denom = matrix (rowSums(exp(distances)),nrow(distances),ncol(distances))
  posteriors = round (exp(distances) / denom, 4)
  
  labss = sort(unique(aggregated_data$label))[apply (distances,1,which.min)]
  
  
  ldamod = MASS::lda (aggregated_data$label ~ ffs, method = 'moment')
  ldapred = stats::predict (ldamod)
  
  file = aggregated_data$file
  label = aggregated_data$label
  correct = as.numeric(ldapred$class == aggregated_data$label)
  winner = ldapred$class
  posterior = apply (ldapred$posterior,1,max)
  posterior = round (posterior, 4)

  output = data.frame (file, label,winner,correct,posterior.winner = posterior)
  if (!distance) output$posterior = round (ldapred$posterior,4)
  if (distance){
    distance = abs(log(ldapred$posterior))
    distance = round (distance, 3)
    output$distance = distance
  }
  output
}

