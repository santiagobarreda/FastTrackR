
#' Plot an aggregated file
#'
#' Set up a plot aggregated files. This function does not draw anything but is used to set up the plot for other functions.
#'
#' @param ffs a dataframe containing formant tracks for a single file, or a list of dataframes comparing multiple analyses for a single file.
#' @param xlim an optional user-specified x-axis range.
#' @param ylim an optional user-specified x-axis range.
#' @param xlab an optional user-specified x-axis label.
#' @param ylab an optional user-specified y-axis label.
#' @param main an optional user-specified plot label.
#' @param colors an optional vector of colors to use for the formant points/lines.
#' @param cex --.
#' @param lwd --.
#' @param pch --.
#' @param add if FALSE, a new plot if created.
#' @param spect an optional spectrogram to be shown behind the tracks.
#' @param ... Additional arguments are passed to the internal call of 'plot'.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' ffs = analyze (sound)
#' spect = spectrogram (sound)
#' plotffs (ffs[[9]])
#' plotffs (ffs[[9]], spect = spect)
#' plotffs (ffs)
#' plotffs (ffs, spect = spect)
#' }

plotffs = function(ffs,xlim=NA,ylim=NA,xlab=NA,ylab=NA,
                   main=NA,colors=NA,cex=NA,lwd=NA,pch=NA,add=FALSE,spect=NA,...){

  zcolors = grDevices::colorRampPalette(c("white", "black"))
  zcolors = zcolors(40)

  if (class (ffs) == "data.frame"){
    if (!is.na(spect[1])){
      if (attr(spect, "object")=="spectrogram"){
        graphics::image (as.numeric(rownames(spect)), as.numeric(colnames(spect)),
                         spect,col = zcolors,
                         xlab = "Time (ms)", ylab = "Frequency",...)
        add = TRUE
      }
    }
    plotffs.internal(ffs,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
                       main=main,colors=colors,cex=cex,lwd=lwd,pch=pch,add=add, ...)
  }
  if (class (ffs)=="list"){
    tmp_par = graphics::par(no.readonly = TRUE)

    n = length (ffs)
    rows = floor(n/4)

    graphics::par (mfrow = c(4,rows), mar =c(1,1,2,1), oma = c(1,1,0,1))
    for (i in 1:n){
      if (!is.na(spect[1])){
        if (attr(spect, "object")=="spectrogram"){
          graphics::image (as.numeric(rownames(spect)), as.numeric(colnames(spect)),
                           spect,col = zcolors,
                           xlab = "Time (ms)", ylab = "Frequency")
          add = TRUE
        }
      }
      plotffs.internal (ffs[[i]],xaxt='n',yaxt='n', add = add, ...)
    }
    graphics::par (tmp_par)
  }
}


plotffs.internal = function(ffs,xlim=NA,ylim=NA,xlab=NA,ylab=NA,
                            main=NA,colors=NA,add=FALSE,...){

  if (is.na(colors[1]) & length(colors)==1) colors = c("darkgoldenrod2",3,4,2)
  if (is.na(xlab)) xlab="Time (ms)"
  if (is.na(ylab)) ylab = "Frequency (Hz)"
  if (is.na(main)) main = ""
  nf = 4
  if (!("f4" %in% colnames (ffs))) nf = 3

  if (!("time" %in% colnames (ffs))){
    w1 = attr (ffs,"w1")*1000
    timestep = attr (ffs,"timestep")*1000
    n = nrow (ffs)
    time = w1 + timestep*(0:(n-1))
  }
  if ("time" %in% colnames (ffs)) time = ffs$time*1000

  if (length(pch)==1) pch = rep (pch, 4)

  if (is.na(xlim)) xlim = range(time)
  if (is.na(ylim) & nf==3) ylim =c(100,max (ffs$f3)+500)
  if (is.na(ylim) & nf==4) ylim =c(100,max (ffs$f4)+500)

  if (!add) plot (0, type="n", xlim=xlim, ylim=ylim, xlab=xlab,
                  ylab=ylab,main=main, ...)

  graphics::points (time, ffs[,"f1"], pch=pch[1], col = colors[1],cex=cex)
  graphics::lines (time, ffs[,"f1"], col = colors[1],lwd=lwd)
  graphics::points (time, ffs[,"f2"], pch=pch[2], col = colors[2],cex=cex)
  graphics::lines (time, ffs[,"f2"], col = colors[2],lwd=lwd)
  graphics::points (time, ffs[,"f3"], pch=pch[3], col = colors[3],cex=cex)
  graphics::lines (time, ffs[,"f3"], col = colors[3],lwd=lwd)
  if (nf==4){
    graphics::points (time, ffs[,"f4"], pch=pch[4], col = colors[4],cex=cex)
    graphics::lines (time, ffs[,"f4"], col = colors[4],lwd=lwd)
  }
}









