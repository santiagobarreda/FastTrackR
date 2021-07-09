
#' Plot an aggregated file
#'
#' Set up a plot aggregated files. This function does not draw anything but is used to set up the plot for other functions.
#'
#' @param ffs a dataframe containing formant tracks for a single file, or a list of dataframes comparing multiple analyses for a single file.
#' @param winner an optional dataframe representing the winners file.
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

plotffs = function(ffs,winner=NA,xlim=NA,ylim=NA,xlab=NA,ylab=NA,
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

    graphics::par (mfrow = c(4,rows), mar =c(1.5,1,2,1), oma = c(1,1,0,1))
    for (i in 1:n){
      main = paste0 ("Maximum formant = ",attr(ffs[[i]], "maxformant")," Hz")
      if (!is.na(spect[1])){
        if (attr(spect, "object")=="spectrogram"){
          graphics::image (as.numeric(rownames(spect)), as.numeric(colnames(spect)),
                           spect,col = zcolors,
                           xlab = "Time (ms)", ylab = "Frequency", main=main)
          add = TRUE
        }
      }
      plotffs.internal (ffs[[i]],xaxt='n',yaxt='n', add = add, main = main,
                        colors=colors,cex=cex,lwd=lwd,pch=pch, ...)
      if(!is.na(winner)) if (winner==i) graphics::box(lwd=3, col = 2)
    }
    graphics::par (tmp_par)
  }
}






