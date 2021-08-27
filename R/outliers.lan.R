
#' Find outliers using Log-Additive Normalization 
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

outliers.lan <- function (aggregated_data){
  
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
  ff = log (unlist(ffs))
  
  nr = nrow (aggregated_data)
  nc = nt*nf
  
  lp = stats::contr.poly(nt)[,1]
  qp = stats::contr.poly(nt)[,2]
  lp = rep (lp, each = nf * nr)
  qp = rep (qp, each = nf * nr)
  f = rep (factor (rep (1:nf, each = nr)), nt)
  vlab = factor(rep (aggregated_data$label, nc))
  fname = factor(rep (aggregated_data$file, nc))
  
  datt = data.frame (ff, lp,qp, vlab, f)
  #str (datt)
  
  modd = MASS::rlm (ff ~ lp*qp*vlab*f, data = datt)
  #summary(modd)
  residuals = abs(modd$residuals)
  
  tmp_agg = stats::aggregate (residuals ~ f + fname + vlab,FUN = mean) 
  
  output = cbind (tmp_agg[tmp_agg$f==1,2:4],
                  tmp_agg[tmp_agg$f==2,4],
                  tmp_agg[tmp_agg$f==3,4])
  colnames (output) = c('file','label','f1res','f2res','f3res')
  if (nf==4) output$f4res = tmp_agg[tmp_agg$f==4,3]
  
  output$totalres = rowMeans (output[,3:ncol(output)])
  output
  
}
