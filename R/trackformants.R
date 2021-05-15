
#' Track formants
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param maxformant the maximum analysis frequency (i.e., the Nyquist/2).
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @return A matrix in which each row represents a different time point. The first four columns represent the frequencies of F1-F4, and columns 5-8 represent their bandwidths.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' trackformants (sound, maxformant = 5000)
#' }

trackformants = function (sound, maxformant = 5000, windowlength = 0.05, timestep = 0.0025){

  if (!class(sound)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  fs = sound@samp.rate
  if (maxformant*2 < fs)  sound = downsample (sound, maxformant)

  sound = sound@left
  fs = maxformant*2

  n = length (sound)
  duration = n / (maxformant*2)
  spots = round(seq (1/fs,duration-windowlength, timestep)*fs)

  windowlength_pts <- round(windowlength * fs)
  window <- phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) sound[x:(x+windowlength_pts-1)]*window))

  #nfft = 2^(ceiling(log2(windowlength_pts)))
  #zeros = matrix (0, nfft-windowlength_pts, ncol (snd_matrix))
  #snd_matrix = rbind (snd_matrix, zeros)

  spect <- stats::mvfft(snd_matrix)
  spect = abs(spect)^2
  r <- Re(stats::mvfft(spect,inverse=TRUE))
  r <- r[1:(nrow(r)/2),]

  coeffs <- suppressWarnings (signal::levinson(x = r, p = 11))$a
  coeffs <- t(coeffs)

  ffs = t(apply (coeffs,2,getformants, fs=fs,nreturn=4))
  colnames (ffs) = c(paste0("f",1:4),paste0("b",1:4))

  ffs = data.frame(ffs)

  attr(ffs, "object") = "ffs"
  attr(ffs, "timestep") = timestep
  attr(ffs, "w1") = windowlength/2
  attr(ffs, "maxformant") = maxformant
  #attr(tmp, "filename") = strsplit(basename(files[1]),split="_")[[1]][1]

  ffs
}

