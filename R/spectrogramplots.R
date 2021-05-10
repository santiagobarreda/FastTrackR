
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
                            main=NA,colors=NA,add=FALSE,cex=NA,lwd=NA,pch=NA,...){

  if (length(colors)==1) colors = c("darkgoldenrod2",3,4,2)
  if (is.na(xlab)) xlab="Time (ms)"
  if (is.na(ylab)) ylab = "Frequency (Hz)"
  if (is.na(main)) main = paste0 ("maximum formant = ",
                                  attr(ffs,"maxformant"), " (Hz)")
  if (is.na(cex)) cex = 1
  if (is.na(lwd)) lwd = 1
  if (!is.na(pch[1]) & length (pch)) pch = rep (pch, 4)
  if (is.na(pch[1])) pch = rep (16, 4)

  nf = 4
  if (!("f4" %in% colnames (ffs))) nf = 3

  if (!("time" %in% colnames (ffs))){
    w1 = attr (ffs,"w1")*1000
    timestep = attr (ffs,"timestep")*1000
    n = nrow (ffs)
    time = w1 + timestep*(0:(n-1))
  }
  if ("time" %in% colnames (ffs)) time = ffs$time*1000

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



#' Spectrogram
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param maxformant the maximum analysis frequency (i.e., the Nyquist/2).
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @param dynamicrange the dynamic range desired for the spectrogram, in decibels.
#' @param plot if TRUE, a plot is created.
#' @param ... Additional arguments are passed to the internal call of 'image'.
#' @return A matrix representing a spectrogram.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' spect = spectrogram (sound, maxformant = 5000)
#' ffs = analyze (sound, timestep = 2)
#' plotffs (ffs[[2]], spect = spect)
#' }

spectrogram = function (sound, maxformant = 5000, windowlength = 0.006, timestep = 0.001,
                        dynamicrange = 60, plot = TRUE, ...){

  if (class(sound)=="Wave"){
    fs = sound@samp.rate
    if (maxformant*2 < fs) sound = tuneR::downsample (sound, maxformant*2)
    tmp_snd = sound@left
  }

  tmp_snd = signal::filter (.97,1, tmp_snd)

  fs = maxformant*2
  n = length (tmp_snd)
  duration = n / (maxformant*2)
  spots = round(seq (1/fs,duration-windowlength, timestep)*fs)

  windowlength_pts <- round(windowlength * fs)
  window <- phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) tmp_snd[x:(x+windowlength_pts-1)]*window))

  nfft = 2^(ceiling(log2(windowlength_pts))) * 2
  zeros = matrix (0, nfft-windowlength_pts, ncol (snd_matrix))
  snd_matrix = rbind (snd_matrix, zeros)

  spect <- stats::mvfft(snd_matrix)
  spect = spect[1:(nrow(spect)/2),]
  spect = t(abs(spect)^2)
  spect = log10(spect)*10
  spect = spect - max(spect)
  spect[spect < -(dynamicrange)] = -dynamicrange

  times = 1000 * round(spots/fs + windowlength/2 , 4)
  rownames (spect) = times
  frequencies = seq (0, (fs/2)-(1/fs), length.out = nfft/2)
  colnames (spect) = frequencies

  #zcolors = colorRampPalette(c("dark blue", "blue", "cyan", "light green",
  #"yellow","orange", "red", "brown"))
  if (plot){
    zcolors = grDevices::colorRampPalette(c("white", "black"))
    zcolors = zcolors(40)
    graphics::image (times, frequencies, spect,col = zcolors, xlab = "Time (ms)",
                     ylab = "Frequency",...)
  }
  attr(spect, "object") = "spectrogram"

  invisible (spect)
}


