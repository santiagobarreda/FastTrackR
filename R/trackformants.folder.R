
#' Track formants for a folder of sounds
#'
#'
#' @param path the path to the folder you with to analyze. If NA, the working directory is used. 
#' @param from the lowest maximum formant frequency that will be considered.
#' @param to the highest maximum formant frequency that will be considered.
#' @param nsteps the number of analysis steps between the lowest and highest analysis frequencies.
#' @param windowlength the LPC analysis window length specified in seconds.
#' @param write if TRUE, the result of the analysis is saved as an RDS file. 
#' @param n_formants the number of formants returned and optimized over (5.5. are always tracked).
#' @param timestep the analysis time step specified in seconds.
#' @param fileinformation a dataframe representing the "file_information.csv" file used by Fast Track. If NA, it is loaded from the working directory. 
#' @param progressbar if TRUE, information about estimated analysis time is printed. 
#' @param write_infos if TRUE, analysis info text files are written out as in Praat. 
#' @param sounds a list of previously loaded sounds. 
#' @param encoding if loading file_information from a local CSV file, you may need to specify the encoding or change the file to UTF-8 encoding.  
#' @return A 'formants' object containing information about all alternate formant analyses for all files. See the help for the readformants() function in this package for more information on 'formants' objects. 
#' @export
#' @examples
#' \dontrun{
#' ffs = trackformants.folder ()
#' ffs
#' plotffs (ffs)
#' plotffs (ffs[[2]])
#' }

#files = list.files ("C:/Users/santi/Desktop/JSdata/melissa-R/sounds", full.names = TRUE)

trackformants.folder = function (path=NA, from = 4800, to = 6800, nsteps=12, windowlength = 0.05, 
                          write = TRUE, n_formants = 3, timestep = 0.002, fileinformation = NA, 
                          progressbar=TRUE, write_infos = FALSE, sounds = NA, encoding = "UTF-8"){

  if (is.na (path)) path =  getwd()

  # if there is a single file run it once
  #if (class(path)=="Wave")
  #  ffs = trackformants.internal (path, from = from, to = to,n_formants = n_formants,
  #                          nsteps=nsteps, windowlength = windowlength,
  #                          timestep = timestep, label = NA)
  
  # if path is a path to a working directory, analyze those folders
  if (class(path)=="character"){
    
    fileinformation = load_file_info (path, encoding = encoding)
    
    sounds_exist = FALSE
    if (!all(is.na(sounds))){
      sounds_exist = TRUE
      n = length(sounds)
    }
    
    if (!sounds_exist & file.exists (path %+% "/sounds.RDS")){
      sounds = readRDS (path %+% "/sounds.RDS")
      n = length(sounds)
      sounds_exist = TRUE
    }
    
    if (!sounds_exist){
      files = list.files (paste0 (path, "/sounds"), full.names = TRUE)
      n = length(files)
    }
    if (nrow(fileinformation)!=n) stop ("Mismatch between file information and number of sounds in the 'sounds' folder. This will probably cause a problem later on so execution is halted.")
    
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
        dir.create(path %+% "/infos", showWarnings = FALSE)
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
    attr(ffs, "labels") = unname (labels)
    attr(ffs, "class") = "formants"
  }
  if (write) saveRDS (ffs, "formants.RDS")

  ffs
}



