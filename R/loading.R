
#' Load Fast Track csv files
#'
#' This function quickly reads in all the files contained in the "/csvs" folder within a Fast Track directory. Since Fast Track exports the acoustic measurements into separate files per token, this function is a quick way to read those all in at once.
#'
#' @param path The path to the working directory for the Fast Track project. If no path is provided, the current working directory for the current R session is used.
#' @param asone If TRUE (the default), the csv files are combined into one dataframe and filenames are indicated in a new column. If FALSE, a list of dataframes is returned and each list element is named after the file.
#' @return A dataframe or list of dataframes, as determined by the \code{asone} parameter.
#' @export
#' @examples
#' \dontrun{
#' csvs <- readcsvs()
#' }

readcsvs <- function (path, asone = TRUE){
  if (missing(path)) path = getwd()
  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  csvs = list()
  for (i in 1:length(files)){
    csvs[[i]] = utils::read.csv (files[i])
    if (!asone) names (csvs)[i] = file_names[i]
    if (asone) csvs[[i]]$file = file_names[i]
  }
  if (asone) csvs = do.call (rbind, csvs)

  attr(csvs, "object") = "csvs"

  return (csvs)
}


#' Load Fast Track formant objects
#'
#' This function quickly reads in all the files contained in the "/formants" folder within a Fast Track directory. Since Fast Track exports one formant object per analysis option per token, thre are usually a large number of files stored in that directory. This function makes it easy to read all that information in at once. Note that it may take a bit of time for all the data to be read in if there are many tokens that were analyzed in that directory.
#' 
#' 
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

      attr(tmp, "object") = "fileffs"
      attr(tmp, "timestep") = as.numeric (timestep)
      attr(tmp, "w1") = as.numeric(w1)
      attr(tmp, "maxformant") = cutoffs[j]
      attr(tmp, "filename") = strsplit(basename(files[ord[count]]),split="_")[[1]][1]

      formants[[i]][[j]] = round (tmp)
    }
    attr(formants[[i]], "object") = "fileffs"
    attr(formants[[i]], "filename") = strsplit(basename(files[ord[count]]),split="_")[[1]][1]
  }
  attr(formants, "object") = "formants"
  attr(formants, "cutoffs") = cutoffs
  return (formants)
}





