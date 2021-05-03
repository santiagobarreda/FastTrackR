
#' Analyze sounds
#'
#'
#' @param sound a Wave object read in using the readWave function from the tuneR package, or a list containing a set of these to be analyzed. A list of paths to wave files on your hard drive can be provided instead using the path parameter.
#' @param from the lowest analysis frequency.
#' @param to the highest analysis frequency.
#' @param nsteps the number of steps between the lowest and highest analysis frequencies.
#' @param windowlength the windowlength specified in seconds.
#' @param timestep the analysis time step specified in seconds.
#' @param path a vector of file names to be analyzed, can be provided instead of the WAve objects directly.
#' @param showprogress if TRUE, analysis progress is shown.
#' @return A list of lists of dataframes. The 'external' list is as long as number of files that were analyzed. For each 'external' list element there are N 'internal' list elements, for N analysis steps. For example, 'formant[[32]][[3]]' contains information regarding the 3rd analysis option for the 32nd file.
#' @export
#' @examples
#' \dontrun{
#' sound = tuneR::readWave("yoursound.wav")
#' snd = sound@left
#' fs = sound@samp.rate
#' tmp_snd = downsample (snd, fs, maxformant = 5000)
#' trackformants (tmp_snd, maxformant = 5000)
#' }

analyze = function (sound, from = 4800, to = 6800, nsteps=12, windowlength = 0.05,
                    timestep = 0.002, path = NA, showprogress=TRUE){

  # if there is a single file run it once
  if (class(sound)=="Wave" | (!is.na(path) & length(path)==1)){
    if (!is.na(path)) sound = tuneR::readWave(path)

    ffs = analyze.internal (sound, from = from, to = to,
                            nsteps=nsteps, windowlength = windowlength,
                            timestep = timestep)
    class(ffs) = "fasttrack"
    attr(ffs, "object") = "fileffs"
  }

  # if there is a list of file names read them all in
  if (length(path)>1) sound = lapply (path, tuneR::readWave)

  # if there is a list of wave objects analyze them
  if (class(sound)=="list"){
    n = length(sound)
    ffs = list(rep(0, n))
    if (showprogress) cat ("Analyzing sound... \n")
    for (i in 1:n){
      if (showprogress) cat (i, " of ", n, " ...\n")
      ffs[[i]] = analyze.internal (sound[[i]], from = from, to = to,
                                   nsteps=nsteps, windowlength = windowlength,
                                   timestep = timestep)

      class(ffs[[i]]) = "fasttrack"
      attr(ffs[[i]], "object") = "fileffs"
    }

    #ffs = lapply (sound, analyze.internal, fs = fs, from = from, to = to,
    #              nsteps=nsteps, windowlength = windowlength,
    #              timestep = timestep)

    class(ffs) = "fasttrack"
    attr(ffs, "object") = "formants"
  }

  ffs
}


analyze.internal = function (tmp_snd, from = 4800, to = 6800, nsteps=12,
                    windowlength = 0.05, timestep = 0.002){

  if (!class(tmp_snd)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  ffs = list(rep(0,nsteps))
  count = nsteps
  maxformants = round(seq(from,to,length.out=nsteps))

  for (i in rev(maxformants)){
    tmp_snd = downsample (tmp_snd, maxformant = i)
    ffs[[count]] = trackformants (tmp_snd,maxformant = i)
    count = count - 1
  }
  class(ffs) = "fasttrack"
  attr(ffs, "object") = "fileffs"
  attr(ffs, "maxformants") = maxformants
  ffs
}


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
#' sound = tuneR::readWave("yoursound.wav")
#' snd = sound@left
#' fs = sound@samp.rate
#' tmp_snd = downsample (snd, fs, maxformant = 5000)
#' trackformants (tmp_snd, maxformant = 5000)
#' }

trackformants = function (sound, maxformant = 5000, windowlength = 0.05, timestep = 0.002){

  if (!class(sound)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  fs = sound@samp.rate
  if (maxformant*2 < fs)
    sound = tuneR::downsample (sound, maxformant*2)

  sound = sound@left
  fs = maxformant*2

  n = length (sound)
  duration = n / (maxformant*2)
  spots = round(seq (1/fs,duration-windowlength, 0.002)*fs)

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

  class(ffs) = "fasttrack"
  attr(ffs, "object") = "ffs"
  attr(ffs, "maxformant") = maxformant

  ffs
}


#' Downsample
#'
#'
#' @param sound a numeric vector representing the sound to be analyzed.
#' @param maxformant the desired maximum analysis frequency (i.e., the new Nyquist/2).
#' @param precision the number of neighbors used to interpolate.
#' @return A numeric vector representing the downsampled sound. The new sampling frequency is maxformant*2.
#' @export
#' @examples
#' \dontrun{
#' sound = tuneR::readWave("yoursound.wav")
#' tmp_snd = downsample (snd, fs, maxformant = 5000)
#' }

downsample = function (sound, maxformant = 5000, precision = 50){

  if (!class(sound)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  tmp_snd = sound@left
  fs = sound@samp.rate

  ratio = (maxformant/fs)*2
  fs = maxformant*2

  if (ratio > 1) stop ("Downsampling only, sorry!")

  filter = signal::butter (5,ratio)
  tmp_snd = signal::filtfilt (filt = filter, x = tmp_snd)

  newtime = seq(1, length(tmp_snd) + 1, by = 1/ratio)
  nearest = round(newtime)
  offset = newtime - nearest
  tmp_snd = c(rep(0, precision), tmp_snd, rep(0, precision + 1))
  y = newtime * 0
  for (i in -precision:precision)
    y = y + tmp_snd[nearest + precision + i] * phonTools::sinc(offset - i, normalized = TRUE)
  y = (y / max (y)) * 1000


  sound@left = tmp_snd
  sound@samp.rate = fs

  return(sound)
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

