
#' Write Praat formant objects
#' 
#' This function writes out R internal formant data into text files that can be used in Praat. 
#' 
#' @param formants a list of formant data read in with the readformants function, or created using the trackformants function.
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @export

writeformants <- function (formants, path = NA){

  if (is.na (path)) path = getwd()

  header = c("File type = \"ooTextFile\"",
             "Object class = \"Formant 2\"",
             "",
             "0",
             "duration",
             0,
             "time_step",
             0.025, # first analysis
             "nf") # maximum number of formants
  
  n_analyses = attr(formants,"ncutoffs")

  for (i in 1:length(formants)){
    print (i)
    for (j in 1:n_analyses){
      tmp_formants = formants[[i]][[j]]

      filename = attr(tmp_formants,"filename")
      nf = ncol(tmp_formants)/2

      header[5] = attr(tmp_formants,"duration")
      header[6] = nrow(tmp_formants)
      header[7] = attr(tmp_formants,"timestep")
      header[9] = nf

      tmp_out = c(rbind(.2, nf, t(tmp_formants)))
      tmp_out = c(header, tmp_out)
      output_file = filename %+% "_" %+% j %+% "_" %+% ".Formant"
      
      writeLines (tmp_out, paste0 (path, "/",output_file))
    }
  }
}












