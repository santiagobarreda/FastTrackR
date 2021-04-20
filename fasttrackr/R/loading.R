
#' Load Fast Track csv files
#'
#' If no path is provided the working directory is assumed to be the Fast Track directory. This is recommended as it means that is means all reading/writing can be done without ever providing a path.
#'
#' @param path the path to the working directory for the Fast Track project.
#' @return A list of dataframes, one containing the data from each csv file. Each dataframe is named after the data filename.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs ()
#' }

readcsvs <- function (path=NA){
  if (is.na(path)) path = getwd()
  files = list.files (paste0(path,"/csvs"),full.names=TRUE)
  file_names = list.files (paste0(path,"/csvs"))
  file_names = substr (file_names,1, nchar(file_names)-4)

  csvs = list()
  for (i in 1:length(files)){
    csvs[[i]] = utils::read.csv (files[i])
    names (csvs)[i] = file_names[i]
  }

  return (csvs)
}


#' Load Fast Track formant objects
#'
#' If no path is provided the working directory is assumed to be the Fast Track directory. This is recommended as it means that is means all reading/writing can be done without ever providing a path.
#'
#' @param path the path to the working directory for the Fast Track project.
#' @return A list of lists of dataframes. The 'external' list is as long as number of files that were analyzed. For each 'external' list element there are N 'internal' list elements, for N analysis steps. For example, 'formant[[32]][[3]]' contains information regarding the 3rd analysis option for the 32nd file.
#' @export
#' @examples
#' \dontrun{
#' csvs = readformants ()
#' }

readformants <- function (path=NA){
  if (is.na(path)) path = getwd()
  info = readLines (list.files (paste0(path,"/infos"),full.names=TRUE)[1])
  nsteps = as.numeric (info[3])
  nf = as.numeric (info[9])

  files = list.files (paste0(path,"/formants"),full.names=TRUE)

  formants = list()
  for (i in 1:(length(files)/nsteps)){
    formants[[i]] = list()
    for (j in 1:nsteps){

      tmp = utils::read.csv (files[i])[,1]
      tmp = tmp[-c(1:7)]
      len1 = which (nchar (tmp)==1)
      tmp = as.numeric (tmp)

      if (nf==3)
        tmp = data.frame (f1=tmp[len1+1],f2=tmp[len1+3],f3=tmp[len1+5],
                          b1=tmp[len1+2],b2=tmp[len1+4],b3=tmp[len1+6])

      if (nf==4)
        tmp = data.frame (f1=tmp[len1+1],f2=tmp[len1+3],f3=tmp[len1+5],f4=tmp[len1+7],
                          b1=tmp[len1+2],b2=tmp[len1+4],b3=tmp[len1+6],b4=tmp[len1+8])

      formants[[i]][[j]] = tmp
    }
  }
}

