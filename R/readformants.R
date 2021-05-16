

#' Load Fast Track formant objects
#'
#' This function reads in all the files contained in the "/formants" folder within a Fast Track directory. Since Fast Track exports one formant object per analysis option per token, there are usually a large number of files stored in that directory. This function makes it easy to read all that information in at once. Note that it may take a bit of time for all the data to be read in if there are many tokens that were analyzed in that directory.
#' 
#' The data is stored in data frames containing formant frequencies and bandwidths for the first 3-4 formants. These values are rounded, which makes display easier and makes the resulting object much smaller in memory (and on your hard drive). Every data frame representing a single Praat formant object needs to have some information associated with it, and this is done using attributes for the object. The attributes for every dataframe representing a formant object are: timestep (the analysis time step, in ms), w1 (the location of the first analysis window, in ms), maxformant (the maximum formant frequency, in Hz), and filename, containing the label used for the formant object (relating to the wav file filename). 
#' 
#' The dataframes corresponding to alternative analyses for a single sound file are stored in a list. This list has the same filename attribute as each of the dataframes contained within it. Finally, all of the lists (each of which represents a single sound) are stored within one overall list. This final list contains an attribute representing the maximum formant frequencies used for all of the analyses represented by the formants files. 
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @return A list of lists of dataframes. The "external" list is as long as number of files that were analyzed. For each "external" list element there are \emph{n} "internal" list elements for \emph{n} analysis steps. For example, \code{formant[[32]][[3]]} contains information regarding the 3rd analysis option for the 32nd file.
#' @export
#' @examples
#' \dontrun{
#' formants = readformants ()
#' }

readformants <- function (path){
  if (missing(path)) path = getwd()
  info = readLines (list.files (paste0(path,"/infos"),full.names=TRUE)[1])
  nsteps = as.numeric (info[3])
  nf = as.numeric (info[9])
  cutoffs = as.numeric (strsplit (info[5], split=" ")[[1]])

  files = list.files (paste0(path,"/formants"),full.names=TRUE)
  ord = unlist (strsplit (basename (files), "_"))[c(T,T,F)]
  ord = matrix (ord, length(ord)/2, 2, byrow=TRUE)
  ord[,2] = addzeros(ord[,2])
  ord = paste0 (ord[,1],"_", ord[,2])
  ord = order (ord)

  count = 0
  formants = list()
  for (i in 1:(length(files)/nsteps)){
    formants[[i]] = list()
    for (j in 1:nsteps){
      count = count + 1
      tmp = utils::read.csv (files[ord[count]])[,1]
      w1 = tmp[6]
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

      #attr(tmp, "object") = "tracks"
      attr(tmp, "timestep") = as.numeric (timestep)
      attr(tmp, "w1") = as.numeric(w1)
      attr(tmp, "maxformant") = cutoffs[j]
      attr(tmp, "filename") = strsplit(basename(files[ord[count]]),split="_")[[1]][1]

      formants[[i]][[j]] = round (tmp)
    }
    #attr(formants[[i]], "object") = "filetracks"
    attr(formants[[i]], "filename") = strsplit(basename(files[ord[count]]),split="_")[[1]][1]
  }
  #attr(formants, "object") = "formants"
  attr(formants, "cutoffs") = cutoffs
  return (formants)
}



