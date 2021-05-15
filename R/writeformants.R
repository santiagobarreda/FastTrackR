
#' Write Fast Track formant objects
#' @param formants --.
#' @param path --.
#' @export
writeformants <- function (formants, path){

  snd_files = list.files (paste0(path,"/sounds"),full.names=TRUE)
  formant_files = list.files (paste0(path,"/formants"),full.names=TRUE)
  time_step = readLines (formant_files)[7]

  tmp_formants = formants[[1]][[1]]
  nf = as.character(ncol(tmp_formants)/2)
  n_analyses = length(formants[[1]])

  file_base = unlist (strsplit (formant_files, split="_"))
  file_base = matrix (file_base, ncol = 3, byrow=TRUE)[,1]
  file_base = unique (file_base)

  header = c("File type = \"ooTextFile\"",
             "Object class = \"Formant 2\"",
             "",
             "0",
             duration=0,
             0,
             time_step,
             0.025, # first analysis
             nf) # maximum number of formants


  for (i in 1:length(snd_files)){
    tmp_snd = readWave2 (snd_files[i])
    duration = length(tmp_snd@left)/tmp_snd@samp.rate
    header[5] = duration

    for (j in 1:n_analyses){
      tmp_formants = formants[[i]][[j]]
      header[6] = nrow(tmp_formants)

      tmp_out = c(rbind(.2, nf, t(tmp_formants)))
      tmp_out = c(header, tmp_out)
      filename = file_base[i] %+% "_" %+% j %+% "_" %+% ".Formant"
      writeLines (tmp_out, filename)
    }
  }
}





















