
#' Make a file information file
#' 
#' Make a file_information.csv file if none exists.
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param write if TRUE, the file is written to the indicated path(or working directory).
#' @export

makefileinformation = function (path = NA, write=TRUE){
  
  if (is.na (path)) path = getwd()
  
  file = list.files (paste0(path,"/sounds"))
  n_files = length(files)
  
  number = 1:n_files
  label = rep ("*", n_files)
  group = rep ("1", n_files)
  color = rep (c("Blue","Green","Magenta","Black",
                 "Lime","Purple","Teal","Navy","Pink",
                 "Maroon","Olive","Grey","Red"), length.out = n_files)
  
  fileinformation = data.frame (number = number, file = file,label=label, 
                                group = group, color = color)
  
  if (write)
    write.csv (fileinformation, "file_information.csv", row.names = FALSE,quote=FALSE)
  
  fileinformation
  
}



