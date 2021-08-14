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
                    logaxes=FALSE, xlab=NA, ylab=NA, add=FALSE,...){

  n = nrow (aggregated_data)
  nf = suppressWarnings(max(as.numeric(substr (colnames(aggregated_data),2,2)), na.rm=TRUE))
  ntimes = suppressWarnings(max(as.numeric(substr (colnames(aggregated_data),3,3)), na.rm=TRUE))

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
  if (is.na (xlab)) xlab = paste0 ("F",xformant," (Hz)")
  if (is.na (ylab)) ylab = paste0 ("F",yformant," (Hz)")

  if (!add) plot (0,type='n',xlim=xrange,ylim=yrange,xlab=xlab,ylab=ylab,log=log,...)

  output = list (ffs=ffs, nf=nf, ntimes=ntimes,color=color)
  return (output)
}


#' Plot formant contours
#'
#' Draws lines representing formant contours using the information represented in aggregated data files. Colors are taken from the 'color' column in the data.
#'
#' @param aggregated_data A dataframe containing the data from an 'aggregated_data' file produced by Fast Track.
#' @param xformant an integer indicating which formant number should be plotted on the x axis.
#' @param yformant an integer indicating which formant number should be plotted on the y axis.
#' @param revaxes if TRUE, axis ranges are inverted so that they go high > low.
#' @param logaxes if TRUE, axes are logarithmic.
#' @param lwd an integer determining contour line width.
#' @param starttime an integer indicating which time point the contour should start at.
#' @param endtime an integer indicating which time point the contour should end at.
#' @param add if FALSE, a new plot if created.
#' @param ... Additional arguments are passed to the internal call of 'lines'.
#' @export
#' @examples
#' data(aggregated_data)
#' ft.lines (aggregated_data)
#' ft.lines (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)

ft.lines <- function(aggregated_data, xformant=1,yformant=2,revaxes=FALSE,
                     logaxes=FALSE,lwd=2,starttime=0,endtime=0,add=FALSE,...){

  output = ft.plot (aggregated_data, xformant=xformant,yformant=yformant,add=add,
                    lwd=lwd, revaxes=revaxes, logaxes=logaxes)
  ffs = output$ffs; color = output$color

  if (starttime==0) starttime=1
  if (endtime==0) endtime=dim(ffs)[3]
  for (i in 1:nrow(aggregated_data))
    graphics::lines (ffs[i,xformant,starttime:endtime],ffs[i,yformant,starttime:endtime],
           lwd=lwd,col=color[i],...)
}


#' Plot points
#'
#' Plot labels at specific points along formant trajectories. Colors and labels are taken from the 'color' column in the data.
#'
#' @param aggregated_data A dataframe containing the data from an 'aggregated_data' file produced by Fast Track.
#' @param xformant an integer indicating which formant number should be plotted on the x axis.
#' @param yformant an integer indicating which formant number should be plotted on the y axis.
#' @param revaxes if TRUE, axis ranges are inverted so that they go high > low.
#' @param logaxes if TRUE, axes are logarithmic.
#' @param cex an integer determining point size.
#' @param time an integer indicating which time point the point should be placed at.
#' @param add if FALSE, a new plot if created.
#' @param ... Additional arguments are passed to the internal call of 'text'.
#' @export
#' @examples
#' data(aggregated_data)
#' ft.lines (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)
#' ft.arrows (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)
#' ft.points (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)

ft.points <- function(aggregated_data, xformant=1,yformant=2,revaxes=FALSE,
                      logaxes=FALSE,cex=2,time=1,add=TRUE,...){

  output = ft.plot (aggregated_data, xformant=xformant,yformant=yformant,add=add,
                    revaxes=revaxes, logaxes=logaxes)
  ffs = output$ffs; color = output$color

  for (i in 1:nrow(aggregated_data)){
    graphics::text (ffs[i,xformant,time],ffs[i,yformant,time],cex=cex,
                    col=color[i], label=aggregated_data$label[i],...)
  }
}

#' Plot arrows
#'
#' Add arrows to the end of contours made by the ft.contour function. Colors are taken from the 'color' column in the data.
#'
#' @param aggregated_data A dataframe containing the data from an 'aggregated_data' file produced by Fast Track.
#' @param xformant an integer indicating which formant number should be plotted on the x axis.
#' @param yformant an integer indicating which formant number should be plotted on the y axis.
#' @param revaxes if TRUE, axis ranges are inverted so that they go high > low.
#' @param logaxes if TRUE, axes are logarithmic.
#' @param lwd an integer determining arrow line width.
#' @param length the length of the arrow lines.
#' @param add if FALSE, a new plot if created.
#' @param ... Additional arguments are passed to the internal call of 'arrows'.
#' @export
#' @examples
#' data(aggregated_data)
#' ft.lines (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)
#' ft.arrows (aggregated_data, xformant=2,yformant=1,revaxes=TRUE)

ft.arrows <- function(aggregated_data, xformant=1,yformant=2,revaxes=FALSE,
                      logaxes=FALSE,lwd=2,length=.1,add=TRUE,...){

  output = ft.plot (aggregated_data, xformant=xformant,yformant=yformant,add=add,
                    lwd=lwd, revaxes=revaxes, logaxes=logaxes,...)
  ffs = output$ffs; color = output$color; ntimes = output$ntimes

  for (i in 1:nrow(aggregated_data)){
    graphics::arrows (ffs[i,xformant,(ntimes-1)],ffs[i,yformant,(ntimes-1)],
                      ffs[i,xformant,ntimes],ffs[i,yformant,ntimes],
                      lwd=lwd,col=color[i],length=length,...)
  }
}





