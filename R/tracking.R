
#' Track formants
#'
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param maxformant the maximum analysis frequency (i.e., the Nyquist/2).
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @return A matrix in which each row represents a different time point. The first four columns represent the frequencies of F1-F4, and columns 5-8 represent their bandwidths.
#' @export
#' @examples
#' \dontrun{
#' sound = tuneR::readWave("yoursound.wav")
#' snd = sound@left
#' fs = sound@samp.rate
#' tmp_snd = downsample (snd, fs, maxformant = 5000)
#' trackformants (tmp_snd, maxformant = 5000)
#' }


trackformants = function (sound, maxformant = 5000, windowlength = 0.05, timestep = 0.002){

  fs = maxformant*2
  n = length (sound)
  duration = n / (maxformant*2)
  spots = round(seq (1/fs,duration-windowlength, 0.002)*fs)

  windowlength_pts <- round(windowlength * fs)
  nfft = 2^(ceiling(log2(windowlength_pts)))

  window <- phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) sound[x:(x+windowlength_pts-1)]*window))
  zeros = matrix (0, nfft-windowlength_pts, ncol (snd_matrix))
  snd_matrix = rbind (snd_matrix, zeros)

  spect <- stats::mvfft(snd_matrix)
  spect = abs(spect)^2
  r <- Re(stats::mvfft(spect,inverse=TRUE))
  r <- r[1:(nfft/2),]

  coeffs <- suppressWarnings (signal::levinson(x = r, p = 11))$a
  coeffs <- t(coeffs)

  ffs = t(apply (coeffs,2,getformants, fs=fs,nreturn=4))
  colnames (ffs) = c(paste0("f",1:4),paste0("b",1:4))

  attr(formants, "type") = "fasttrack"
  attr(formants, "object") = "ffs"

  ffs
}


#' Downsample
#'
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param fs the sampling frequency of the sound (i.e., the Nyquist*2).
#' @param maxformant the desired maximum analysis frequency (i.e., the new Nyquist/2).
#' @param precision the number of neighbors used to interpolate.
#' @return A numeric vector representing the downsampled sound. The new sampling frequency is maxformant*2.
#' @export
#' @examples
#' \dontrun{
#' sound = tuneR::readWave("yoursound.wav")
#' tmp_snd = downsample (snd, fs, maxformant = 5000)
#' }

downsample = function (sound, fs, maxformant = 5000, precision = 50){

  ratio = (maxformant/fs)*2
  fs = maxformant*2

  if (ratio > 1) stop ("Downsampling only, sorry!")

  filter = signal::butter (5,ratio)
  sound = signal::filtfilt (filt = filter, x = sound)

  newtime = seq(1, length(sound) + 1, by = 1/ratio)
  nearest = round(newtime)
  offset = newtime - nearest
  sound = c(rep(0, precision), sound, rep(0, precision + 1))
  y = newtime * 0
  for (i in -precision:precision)
    y = y + sound[nearest + precision + i] * phonTools::sinc(offset - i, normalized = TRUE)
  y = y / max (y)

  return(y)
}


getformants = function (coeffs, fs = 1, nreturn=4){

  roots = polyroot(rev(coeffs))
  angs = atan2(Im(roots), Re(roots))
  formants = round(angs * (fs/(2 * pi)), 2)
  nums = order(formants)
  formants = formants[nums]
  bws = -(fs/pi) * log(abs(roots[nums]))
  touse = (formants > 0)
  out = c(formants[touse][1:4], bws[touse][1:4])
  return (round(out))
}




