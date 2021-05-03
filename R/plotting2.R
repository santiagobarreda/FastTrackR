
#' Plot an aggregated file
#'
#' Set up a plot aggregated files. This function does not draw anything but is used to set up the plot for other functions.
#'
#' @param aggregated_data A dataframe containing the data from an 'aggregated_data' file produced by Fast Track.
#' @param xformant an integer indicating which formant number should be plotted on the x axis.
#' @param yformant an integer indicating which formant number should be plotted on the y axis.
#' @param revaxes if TRUE, axis ranges are inverted so that they go high > low.
#' @param logaxes if TRUE, axes are logarithmic.
#' @param xlab an optional user-specified x-axis label.
#' @param ylab an optional user-specified y-axis label.
#' @param add if FALSE, a new plot if created.
#' @param ... Additional arguments are passed to the internal call of 'plot'.
#' @export

ft.plot <- function(aggregated_data, xformant=1,yformant=2,revaxes=FALSE,
                    logaxes=FALSE, xlab, ylab, add=FALSE,...){

  n = nrow (aggregated_data)
  nf = as.numeric (substr (utils::tail(colnames(aggregated_data),1),2,2))
  ntimes = as.numeric (substr (utils::tail(colnames(aggregated_data),1),3,3))

  #expected_colnames = paste0("f", rep(1:nf,ntimes),rep(1:ntimes, each=nf))
  if (ncol(aggregated_data) < (7+nf*ntimes))
    stop ("Some formant column is missing, rows can be removed but not columns!")

  # break-up data into 3d array for easier plotting. rows are observations
  # columns are formants, matrices (3rd dim) are time slices.
  ffs = array (0, dim = c(n,nf,ntimes))
  for (i in 0:(ntimes-1)){
    ffs[,,i+1] = as.matrix (aggregated_data[,(8+(i*nf)):(8+nf-1+(i*nf))])
  }
  color = aggregated_data$color
  color[color=="Lime"] = "green2"
  color[color=="Teal"] = "turquoise2"
  color[color=="Olive"] = "olivedrab3"

  xrange = range (ffs[,xformant,])
  yrange = range (ffs[,yformant,])

  if (revaxes){ xrange = rev (xrange); yrange = rev (yrange) }
  log=""
  if (logaxes) log="xy"
  if (missing (xlab)) xlab = paste0 ("F",xformant," (Hz)")
  if (missing (ylab)) ylab = paste0 ("F",yformant," (Hz)")

  if (!add) plot (0,type='n',xlim=xrange,ylim=yrange,xlab=xlab,ylab=ylab,log=log,...)

  output = list (ffs=ffs, nf=nf, ntimes=ntimes,color=color)
  return (output)
}
