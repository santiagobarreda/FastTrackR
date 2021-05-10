

#' Spectrogram
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param maxformant the maximum analysis frequency (i.e., the Nyquist/2).
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @param dynamicrange the dynamic range desired for the spectrogram, in decibels.
#' @param resolution the spectral resolution in Hertz.
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

spectrogram = function (sound, maxformant = 5000, windowlength = 0.005, timestep = 0.001,
                        dynamicrange = 60, resolution = 50, plot = TRUE, ...){

  if (!class(sound)=="Wave") stop ("Please pass a Wave object read in using the readWave2 function.")

  fs = sound@samp.rate
  tmp_snd = sound@left
  tmp_snd = phonTools::preemphasis (tmp_snd, fs = fs)

  n = length (tmp_snd)
  duration = n / (fs)
  spots = round(seq (1/fs,duration-windowlength, timestep)*fs)

  windowlength_pts <- round(windowlength * fs)
  window <- phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) tmp_snd[x:(x+windowlength_pts-1)]*window))

  resolution_pts = fs/resolution
  resolution_pts = max (resolution_pts, windowlength_pts)

  nfft = 2^(ceiling(log2(resolution_pts)))
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
  spect = spect[,frequencies <= 5000]
  frequencies = frequencies[frequencies <= 5000]

  #zcolors = colorRampPalette(c("dark blue", "blue", "cyan", "light green",
  #"yellow","orange", "red", "brown"))
  if (plot){
    zcolors = grDevices::colorRampPalette(c("white", "black"))
    zcolors = zcolors(dynamicrange)
    graphics::image (times, frequencies, spect,col = zcolors, xlab = "Time (ms)",
                     ylab = "Frequency (Hz)")
  }
  attr(spect, "object") = "spectrogram"

  invisible (spect)
}


