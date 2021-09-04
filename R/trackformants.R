
#' Track formants for a sound or folder of sounds
#'
#'
#' @param path a Wave object read in using the readWave function from the tuneR package, or a list containing a set of these to be analyzed. A list of paths to wave files on your hard drive can be provided instead using the path parameter.
#' @param from the lowest analysis frequency.
#' @param to the highest analysis frequency.
#' @param nsteps the number of steps between the lowest and highest analysis frequencies.
#' @param windowlength the windowlength specified in seconds.
#' @param write if TRUE, the result of the analysis is saved as an RDS file. 
#' @param n_formants the number of formants returned (5.5. are always tracked).
#' @param timestep the analysis time step specified in seconds.
#' @param fileinformation a dataframe representing the "file_information.csv" file used by Fast Track. If NA, it is loaded from the working directory. 
#' @param progressbar if TRUE, information about estimated analysis time is printed. 
#' @param write_infos if TRUE, analysis info text files are written out as in Praat. 
#' @param sounds a list of previously loaded sounds.. 
#' @param encoding --. 
#' @return A dataframe containing information about the formant tracks for the file.
#' @export
#' @examples
#' \dontrun{
#' sound = readWave2("yoursound.wav")
#' ffs = trackformants (sound, timestep = 0.002)
#' plotffs (ffs)
#' plotffs (ffs[[2]])
#' }

#files = list.files ("C:/Users/santi/Desktop/JSdata/melissa-R/sounds", full.names = TRUE)

trackformants = function (path=NA, from = 4800, to = 6800, nsteps=12, windowlength = 0.05, 
                          write = TRUE, n_formants = 3, timestep = 0.002, fileinformation = NA, 
                          progressbar=TRUE, write_infos = FALSE, sounds = NA, encoding = "UTF-8"){

  if (is.na (path)) path =  getwd()

  # if there is a single file run it once
  if (class(path)=="Wave")
    ffs = trackformants.internal (path, from = from, to = to,n_formants = n_formants,
                            nsteps=nsteps, windowlength = windowlength,
                            timestep = timestep, label = NA)
  
  # if path is a path to a working directory, analyze those folders
  if (class(path)=="character"){
    
    fileinformation = load_file_information (path, encoding = encoding)
    
    sounds_exist = FALSE
    if (!all(is.na(sounds))){
      sounds_exist = TRUE
      n = length(sounds)
    }
    
    if (!sounds_exist & file.exists (path %+% "sounds.RDS")){
      sounds = readRDS (path %+% "sounds.RDS")
      n = length(sounds)
      sounds_exist = TRUE
    }
    
    if (!sounds_exist){
      files = list.files (paste0 (path, "/sounds"), full.names = TRUE)
      n = length(files)
    }
    
    labels = fileinformation$label
    names (labels) = fileinformation$file
    
    ffs = list(rep(0, n))
    cat ("Analyzing ", n, " sounds.\n")
    
    start = Sys.time()
    for (i in 1:n){
      
      if (progressbar)  progressbar (i,n,start)
      
      if (!sounds_exist) sound = readWave2 (files[i])
      if (sounds_exist) sound = sounds[[i]]
      
      ffs[[i]] = trackformants.internal (sound, from = from, to = to,n_formants = n_formants,
                                   nsteps=nsteps, windowlength = windowlength,
                                   timestep = timestep, label = labels[i])
      
      if (write_infos){
        output = c(paste0(attr(ffs[[i]],"filename"),".wav"),
                   "Number of steps:",
                   length (attr(ffs[[1]], "cutoffs")),
                   "Cutoff frequencies were",
                   paste (attr(ffs[[1]], "cutoffs"), collapse = " "),
                   "Number of coefficients for prediction",
                   "5",
                   "Number of formants",
                   n_formants)
        filename = path %+% "/infos/"%+%attr(ffs[[i]],"filename")%+%"_info.txt"
        write (output, filename)  
      }
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



