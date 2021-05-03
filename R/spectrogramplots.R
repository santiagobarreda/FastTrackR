
#' Plot an aggregated file
#'
#' Set up a plot aggregated files. This function does not draw anything but is used to set up the plot for other functions.
#'
#' @param ffs a dataframe containing formant tracks for a single file, or a list of dataframes comparing multiple analyses for a single file.
#' @param xlim an integer indicating which formant number should be plotted on the x axis.
#' @param ylim --.
#' @param xlab an optional user-specified x-axis label.
#' @param ylab an optional user-specified y-axis label.
#' @param main --.
#' @param colors --.
#' @param add if FALSE, a new plot if created.
#' @param ... Additional arguments are passed to the internal call of 'plot'.
#' @export
#' @examples
#' \dontrun{
#' sound = tuneR::readWave("yoursound.wav")
#' ffs = analyze (sound)
#' plotffs (ffs)
#' plotffs (ffs[[2]])
#' }

plotffs = function(ffs,xlim=NA,ylim=NA,xlab=NA,ylab=NA,
                   main=NA,colors=NA,add=FALSE,...){

  if (class (ffs)=="fasttrack"){
    if (attr (ffs,"object")=="ffs")
      plotffs.internal(ffs,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
                       main=main,colors=colors,add=FALSE, ...)

    if (attr (ffs,"object")=="fileffs"){
      tmp_par = graphics::par(no.readonly = TRUE)

      n = length (ffs)
      rows = floor(n/4)

      graphics::par (mfrow = c(4,rows), mar =c(.1,.1,2,.1), oma = c(1,1,0,1))
      for (i in 1:n){
        plotffs.internal (ffs[[i]],xaxt='n',yaxt='n', ...)
      }
      graphics::par (tmp_par)
    }
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
#' sound = tuneR::readWave("yoursound.wav")
#' spectrogram (sound, maxformant = 5000)
#' }

spectrogram = function (sound, maxformant = 5000, windowlength = 0.009, timestep = 0.005,
                        dynamicrange = 60, plot = TRUE, ...){

  if (class(sound)=="Wave"){
    fs = sound@samp.rate
    tmp_snd = sound@left
  }
  tmp_snd = signal::filter (.97,1, tmp_snd)
  if (maxformant*2 < fs)
    tmp_snd = tuneR::downsample (sound, maxformant*2)

  fs = maxformant*2
  n = length (tmp_snd)
  duration = n / (maxformant*2)
  spots = round(seq (1/fs,duration-windowlength, 0.002)*fs)

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
  class(spect) = "fasttrack"
  attr(spect, "object") = "spectrogram"

  invisible (spect)
}



plotffs.internal = function(ffs,xlim=NA,ylim=NA,xlab=NA,ylab=NA,
                            main=NA,colors=NA,add=FALSE,...){

  if (length(colors)==1) colors = c("darkgoldenrod2",3,4,2)
  if (is.na(xlab)) xlab="Time (ms)"
  if (is.na(ylab)) ylab = "Frequency (Hz)"
  if (is.na(main)) main = paste0 ("maximum formant = ",
                                    attr(ffs,"maxformant"), " (Hz)")

  n = nrow (ffs)
  nf = ncol (ffs)/2
  if (is.na(ylim)) ylim =c(100,max (ffs[,1:nf]))
  time = 25 + 2*(0:(n-1))
  if (is.na(xlim)) xlim = range((time))


  if (!add)
    plot (0, type="n", xlim=xlim, ylim=ylim, xlab=xlab,
          ylab=ylab,main=main, ...)

  graphics::points (time, ffs[,1], pch=16, col = colors[1])
  graphics::lines (time, ffs[,1], col = colors[1])
  graphics::points (time, ffs[,2], pch=16, col = colors[2])
  graphics::lines (time, ffs[,2], col = colors[2])
  graphics::points (time, ffs[,3], pch=16, col = colors[3])
  graphics::lines (time, ffs[,3], col = colors[3])
  if (nf==4){
    graphics::points (time, ffs[,4], pch=16, col = colors[4])
    graphics::lines (time, ffs[,4], col = colors[4])
  }
}


