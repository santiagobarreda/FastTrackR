
'%+%' = function (x,y) paste0 (x,y)


getnumbers = function (x){
  string = strsplit(x,split="")
  use = suppressWarnings (lapply (string, as.numeric))
  use = lapply (use, is.na)

  output = NULL
  for (i in 1:length (string))
    output[i] = paste (string[[i]][!use[[i]]], collapse="")

  as.numeric(output)
}



addzeros = function (nums){
  toadd = 4-nchar (nums)
  sapply (1:length (toadd), function (i) paste0 (paste0(rep(0,toadd[i]),collapse=""), nums[i]))
}



readWave2 = function (path){

  sound = tuneR::readWave (path)
  attr (sound, "filename") = basename (path)

  sound
}


extractWave2 = function (sound,from,to, filename){

  sound_out = tuneR::extractWave (sound,from,to,
                      xunit='time',interact=FALSE)
  attr (sound_out, "filename") = basename (filename)

  sound_out
}
