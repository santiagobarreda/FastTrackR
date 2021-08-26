
#' Write Fast Track formant objects
#' @param formants --.
#' @param path --.
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












