
#' Carry out LPC for sound
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param n_formants --.
#' @param maxformant the maximum analysis frequency (i.e., the Nyquist/2).
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @param preemphasis_frequency --.
#' @param label --.
#' @param returnsound --.
#' @return A matrix in which each row represents a different time point. The first four columns represent the frequencies of F1-F4, and columns 5-8 represent their bandwidths.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' trackformants (sound, maxformant = 5300)
#' sound = readWave2("C:/Users/santi/Desktop/JSdata/melissa-R/sounds/WS005-Melissa_2725.wav")
#' sound = readWave2("C:/Users/santi/Desktop/JSdata/melissa-R/sounds/WS005-Melissa_0001_14000.wav")
#' }

findformants = function (sound, n_formants = 4, maxformant = 7000, windowlength = 0.05, 
                          timestep = 0.002, preemphasis_frequency = 50, 
                          label = NA, returnsound = FALSE){

  if (!class(sound)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  tmp_sound = sound
  filename = tmp_sound@filename
  fs = tmp_sound@samp.rate
  
  #if (maxformant*2 < fs)  tmp_sound = tuneR::downsample (tmp_sound, maxformant*2)
  if (maxformant*2 < fs)  tmp_sound = downsample (sound, maxformant)

  sound_samples = tmp_sound@left
  fs = tmp_sound@samp.rate
  
  if (!is.na(preemphasis_frequency)){
    coeff = -exp(-2 * pi * preemphasis_frequency/fs)
    sound_samples = signal::filter(c(1,coeff),1,sound_samples)
  }

  sound_samples = sound_samples / stats::sd (sound_samples)
  
  windowlength_pts = round(windowlength * fs)

  n = length (sound_samples)
  duration = n / fs
  spots = round(seq (1/fs,duration-windowlength, timestep)*fs)
  
  window = phonTools::windowfunc(windowlength_pts, "gaussian")
  snd_matrix = (sapply (spots, function (x) sound_samples[x:(x+windowlength_pts-1)]*window))
  
  #nfft = 2^(ceiling(log2(windowlength_pts)))
  #zeros = matrix (0, nfft-windowlength_pts, ncol (snd_matrix))
  #snd_matrix = rbind (snd_matrix, zeros)
  
  spect = stats::mvfft(snd_matrix)
  spect = abs(spect)^2
  r = Re(stats::mvfft(spect,inverse=TRUE))
  r = r[1:(nrow(r)/2),]

  coeffs = suppressWarnings (signal::levinson(x = r, p = 11))$a
  coeffs = t(coeffs)
  
  ffs = t(apply (coeffs,2,solvelpc, fs=fs,nreturn=n_formants))
  colnames (ffs) = c(paste0("f",1:n_formants),paste0("b",1:n_formants))

  ffs = data.frame(ffs)

  attr(ffs, "filename") = substr (filename,1,nchar(filename)-4)
  attr(ffs, "duration") = duration
  attr(ffs, "timestep") = timestep
  attr(ffs, "label") = label
  attr(ffs, "w1") = windowlength/2
  attr(ffs, "maxformant") = maxformant
  
  if(!returnsound) return (ffs)
  if(returnsound) return (list (ffs=ffs, sound=tmp_sound))
}

