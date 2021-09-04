
#' Make a file information file
#' 
#' Make a file_information.csv file if none exists.
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @export

makefileinformation = function (path = NA){
  
  if (is.na (path)) path = getwd()
  
  files = list.files (paste0(path,"/sounds"))
  n_files = length(files)
  
  number = 1:n_files
  label = rep ("*", n_files)
  group = rep ("1", n_files)
  color = rep (c("Blue","Green","Magenta","Black",
                 "Lime","Purple","Teal","Navy","Pink",
                 "Maroon","Olive","Grey","Red"), length.out = n_files)
  
  fileinformation = data.frame (number = number, file = files,label=label, 
                                group = group, color = color)
  
  fileinformation
  
}



