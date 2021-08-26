
#' Track formants for a sound or folder of sounds
#'
#'
#' @param path a Wave object read in using the readWave function from the tuneR package, or a list containing a set of these to be analyzed. A list of paths to wave files on your hard drive can be provided instead using the path parameter.
#' @param from the lowest analysis frequency.
#' @param to the highest analysis frequency.
#' @param nsteps the number of steps between the lowest and highest analysis frequencies.
#' @param windowlength the windowlength specified in seconds.
#' @param write --.
#' @param n_formants --.
#' @param timestep the analysis time step specified in seconds.
#' @param fileinformation --.
#' @param estimateduration --.
#' @return A list of lists of dataframes. The 'external' list is as long as number of files that were analyzed. For each 'external' list element there are N 'internal' list elements, for N analysis steps. For example, 'formant[[32]][[3]]' contains information regarding the 3rd analysis option for the 32nd file.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' ffs1 = analyze (sound, timestep = 0.002, from = 4800, to = 6800, nsteps=12)
#' ffs2 = trackformants (sound, timestep = 0.002)
#' plotffs (ffs1)
#' plotffs (ffs1[[2]])
#' }

#files = list.files ("C:/Users/santi/Desktop/JSdata/melissa-R/sounds", full.names = TRUE)

trackformants = function (path=NA, from = 4800, to = 6800, nsteps=12, windowlength = 0.05, write = TRUE,
                          n_formants = 3, timestep = 0.002, fileinformation = NA, estimateduration=TRUE){

  if (is.na (path)) path =  getwd()

  # if there is a single file run it once
  if (class(path)=="Wave")
    ffs = trackformants.internal (path, from = from, to = to,n_formants = n_formants,
                            nsteps=nsteps, windowlength = windowlength,
                            timestep = timestep, label = NA)
  

  # if there is a list of wave objects analyze them
  if (class(path)=="character"){
    if (is.na(fileinformation)){
      if (file.exists (path %+% "/file_information.csv"))
        fileinformation = utils::read.csv (path %+% "/file_information.csv")
      
      if (!file.exists (path %+% "/file_information.csv")){
        cat ("No file information exists in your working directory (and none was provided).")
        cat ("A default one was generated and saved in your working directory.")
        #makefileinformation()
      }
    }
    
    files = list.files (paste0 (getwd(), "/sounds"), full.names = TRUE)
    n = length(files)
    
    labels = fileinformation$label
    names (labels) = fileinformation$file
    
    ffs = list(rep(0, n))
    cat ("Analyzing ", n, " sounds.\n")
    if (estimateduration & n > 200){
      cat ("Progress begun at",format(Sys.time(), "%H:%M:%S"),"\n")
      start = Sys.time()
    }
    for (i in 1:n){
      if (estimateduration & (n > 200) & (i %in% c(50, 200,500,1000))){
        now = Sys.time()
        pred_duration = abs(((start - now)/i) * n)
        units = attr (pred_duration, "units")
        time = round (abs (pred_duration), 2)
        cat ("Estimated total analysis duration at",i,"tokens:", time,units,"\n")
      }
      sound = readWave2 (files[i])
      
      ffs[[i]] = trackformants.internal (sound, from = from, to = to,n_formants = n_formants,
                                   nsteps=nsteps, windowlength = windowlength,
                                   timestep = timestep, label = labels[i])
    }

    ## attributes for object containing all analyses
    attr(ffs, "path") = path
    attr(ffs, "nfiles") = length (ffs)
    attr(ffs, "cutoffs") = attr(ffs[[1]], "cutoffs")
    attr(ffs, "ncutoffs") = length (attr(ffs[[1]], "cutoffs"))
    attr(ffs, "labels") = labels[i]
    attr(ffs, "class") = "formants"
  }
  if (write) saveRDS (ffs, "formants.RDS")

  ffs
}



