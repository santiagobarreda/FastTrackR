
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
  
  form = label ~ ffs
  #ldamod = MASS::lda (aggregated_data$label ~ ffs)
  ldamod = MASS::lda (aggregated_data$label ~ ffs, method = 't')
  #ldamod
  
  ldapred = stats::predict (ldamod)
  
  file = aggregated_data$file
  label = aggregated_data$label
  correct = as.numeric(ldapred$class == aggregated_data$label)
  winner = ldapred$class
  posterior = apply (ldapred$posterior,1,max)
  posterior = round (posterior, 4)
  distance = abs(log(ldapred$posterior))
  distance = round (distance, 3)
  
  output = data.frame (file, label,winner,correct,posterior)
  output$distance = distance
  output
}

