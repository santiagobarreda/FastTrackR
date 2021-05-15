
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
#' ffs1 = analyze (sound, timestep = 0.002)
#' ffs2 = trackformants (sound, timestep = 0.002)
#' plotffs (ffs2)
#' plotffs (ffs[[2]])
#' }

analyze = function (sound, from = 4800, to = 6800, nsteps=12, windowlength = 0.05,
                    timestep = 0.002, path = NA, showprogress=TRUE){

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
