
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
