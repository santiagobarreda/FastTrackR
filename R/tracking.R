
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
#' sound = readWave2("yoursound.wav")
#' ffs = analyze (sound, timestep = 0.002)
#' plotffs (ffs)
#' plotffs (ffs[[2]])
#' }

analyze = function (sound, from = 4800, to = 6800, nsteps=12, windowlength = 0.05,
                    timestep = 0.0025, path = NA, showprogress=TRUE){

  # if there is a list of file names read them all in
  if (length(path)>1) sound = lapply (path, readWave2)

  # if there is a single file run it once
  if (class(sound[1])=="Wave" | (length(path)==1)){
    if (!is.na(path)) sound = readWave2(path)

    ffs = analyze.internal (sound, from = from, to = to,
                            nsteps=nsteps, windowlength = windowlength,
                            timestep = timestep)

    attr(ffs, "object") = "fileffs"
  }

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

      attr(ffs[[i]], "object") = "fileffs"
    }

    #ffs = lapply (sound, analyze.internal, from = from, to = to,
    #              nsteps=nsteps, windowlength = windowlength,
    #              timestep = timestep)

    attr(ffs, "object") = "formants"
  }

  ffs
}

analyze.internal = function (tmp_snd, from = 4800, to = 6800, nsteps=12,
                    windowlength = 0.05, timestep = 0.0025){

  if (!class(tmp_snd)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  ffs = list(rep(0,nsteps))
  count = nsteps
  maxformants = round(seq(from,to,length.out=nsteps))

  for (i in rev(maxformants)){
    tmp_snd = downsample (tmp_snd, maxformant = i)
    ffs[[count]] = trackformants (tmp_snd,maxformant = i,timestep=timestep)
    count = count - 1
  }
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

