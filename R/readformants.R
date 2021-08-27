
#' Load Fast Track formant objects
#'
#' This function reads in all the files contained in the "/formants" folder within a Fast Track directory. Since Fast Track exports one formant object per analysis option per token, there are usually a large number of files stored in that directory. This function makes it easy to read all that information in at once. Note that it may take a bit of time (1-2 minutes per thousand sound files) for all the data to be read in if there are many tokens that were analyzed in that directory.
#' 
#' Each individual analysis is stored in a data frame containing formant frequencies and bandwidths for the first 3-4 formants. These values are rounded, which makes display easier and makes the resulting object much smaller in memory (and on your hard drive). Every data frame representing a single Praat formant object needs to have some information associated with it, and this is done using attributes for the object. The attributes for every dataframe representing a formant object are: timestep (the analysis time step, in ms), w1 (the location of the first analysis window, in ms), maxformant (the maximum formant frequency, in Hz), and filename, containing the label used for the formant object (relating to the wav file filename). 
#' 
#' The dataframes corresponding to alternative analyses for a single sound file are stored in a list. This list has the same filename attribute as each of the dataframes contained within it. Finally, all of the lists (each of which represents a single sound) are stored within one overall list. This final list contains an attribute representing the maximum formant frequencies used for all of the analyses represented by the formants files. 
#' 
#' So, if you read in your analysis into an object called 'formants', then 'formants[[2]][[3]]' represents the dataframe containing the third analysis for the second sound file. 'formants[[2]]' is a list containing all of the dataframes for the second sound file and 'formants' is a list containing each of the list of dataframes for all the sound files. 
#' 
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param fileinformation a dataframe representing the "file_information.csv" file used by Fast Track. If NA, it is loaded from the working directory. 
#' @param progressbar if TRUE, a progress bar prints out in the console.
#' @return A list of lists of dataframes. The "external" list is as long as number of files that were analyzed. For each "external" list element there are \emph{n} "internal" list elements for \emph{n} analysis steps. For example, \code{formant[[32]][[3]]} contains information regarding the 3rd analysis option for the 32nd file.
#' @export
#' @examples
#' \dontrun{
#' formants = readformants (progressbar = TRUE)
#' }

readformants <- function (path, fileinformation = NA, progressbar = TRUE){
  
  if (missing(path)) path = getwd()
  
  if (is.na(fileinformation)){
    if (file.exists (path %+% "/file_information.csv"))
      fileinformation = utils::read.csv (path %+% "/file_information.csv")
    
    if (!file.exists (path %+% "/file_information.csv")){
      cat ("No file information exists in your working directory (and none was provided).")
      cat ("A default one was generated and saved in your working directory.")
      fileinformation = makefileinformation(path)
    }
  }
  
  labels = fileinformation$label
  names (labels) = fileinformation$file
  label_vector = rep(fileinformation$label,each=nsteps)
  
  info = readLines (list.files (paste0(path,"/infos"),full.names=TRUE)[1])
  nsteps = as.numeric (info[3])
  nf = as.numeric (info[9])
  cutoffs = as.numeric (strsplit (info[5], split=" ")[[1]])

  files = list.files (paste0(path,"/formants"),full.names=TRUE)
  n_files = length(files)

  filename_vector = rep(names (labels),each=nsteps)
    
  ord = unlist (strsplit (basename (files), "_"))
  ord = matrix(ord, n_files,length(ord)/n_files,byrow=TRUE)
  nc = ncol (ord)
  ord = ord[,(nc-2):(nc-1)]
  ord = as.numeric(ord[1:nsteps,2])

  count = 0
  tmp_formants = vector(mode = "list", length = )
  cat ("There are ", n_files, "files to read. \n")
  
  items = length (strsplit (files,"_")[[1]])
  mat = matrix(unlist (strsplit (files,"_")), length (files), items, byrow = TRUE)
  mat = mat[,(ncol(mat)-2):(ncol(mat)-1)]
  ord = order (mat[,1],as.numeric(mat[,2]))
  
  start = Sys.time()
  tmp_formants = lapply (ord, function (j){
    if (progressbar) progressbar (j,n_files,start)
    
    tmp = utils::read.csv (files[j])[,1]
    w1 = tmp[6]
    duration = tmp[3]
    timestep = tmp[5]
    tmp = tmp[-c(1:7)]
    len1 = which (nchar (tmp)==1)
    tmp = as.numeric (tmp)

    if (nf==3)
      tmp = data.frame (f1=tmp[len1+1],f2=tmp[len1+3],f3=tmp[len1+5],
                        b1=tmp[len1+2],b2=tmp[len1+4],b3=tmp[len1+6])
    
    if (nf==4)
      tmp = data.frame (f1=tmp[len1+1],f2=tmp[len1+3],f3=tmp[len1+5],f4=tmp[len1+7],
                        b1=tmp[len1+2],b2=tmp[len1+4],b3=tmp[len1+6],b4=tmp[len1+8])
    
    ## formant track attributes
    attr(tmp, "filename") = tools::file_path_sans_ext(basename(filename_vector[j]))
    attr(tmp, "duration") = as.numeric(duration)
    attr(tmp, "timestep") = as.numeric (timestep)
    attr(tmp, "label") = label_vector[j]
    attr(tmp, "w1") = as.numeric(w1)
    attr(tmp, "maxformant") = rep(cutoffs, n_files/nsteps)[j]
    round (tmp)
  })
  
  formants = vector(mode = "list", length = n_files / nsteps)
  count = 1
  for (i in seq(1,n_files,nsteps)){
    formants[count] = list(tmp_formants[i:(i+nsteps-1)])
    
    ## attributes for object comparing analyses for single file
    attr(formants[[count]], "filename") = attr(tmp_formants[[i]], "filename")
    attr(formants[[count]], "duration") = attr(tmp_formants[[i]], "duration")
    attr(formants[[count]], "timestep") = attr(tmp_formants[[i]], "timestep")
    attr(formants[[count]], "label") = labels[count]
    attr(formants[[count]], "cutoffs") = cutoffs
    attr(formants[[count]], "class") = "formants_single"
    count = count + 1
  }
  
  ## attributes for object comparing all analyses for all files
  attr(formants, "path") = path
  attr(formants, "nfiles") = length (formants)
  attr(formants, "cutoffs") = cutoffs
  attr(formants, "ncutoffs") = length (cutoffs)
  attr(formants, "labels") = unname(labels)
  attr(formants, "class") = "formants"
  
  return (formants)
}


#' @export
print.formants = function (x, ...){
  cat ("\nFormant information for data in working directory: \n")
  cat (attributes (x)$path, "\n\n")
  
  cat ("A list with", attributes (x)$nfiles, "elements (e.g., formants[[1]]).\n\n")
  
  cat ("Each list element contains a list of", length(x[[1]]), "dataframes (e.g., formants[[1]][[1]]).\n\n")

  cat ("Cutoff frequencies equal to: \n")
  cat (attributes (x)$cutoffs, "\n\n")
}



#' @export
print.formants_single = function (x, ...){
  cat ("\nFormant information for file:",attributes (x)$filename, "\n\n")

  cat ("A list of", length(x), "dataframes (e.g., formants[[1]]).\n\n")
  
  cat ("Cutoff frequencies equal to: \n")
  cat (attributes (x)$cutoffs, "\n\n")
}



