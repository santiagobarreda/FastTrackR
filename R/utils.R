
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



# #' Downsample
# #'
# #'
# #' @param sound a numeric vector representing the sound to be analyzed.
# #' @param maxformant the desired maximum analysis frequency (i.e., the new Nyquist/2).
# #' @param precision the number of neighbors used to interpolate.
# #' @return A numeric vector representing the downsampled sound. The new sampling frequency is maxformant*2.
# #' @export
# #' @examples
# #' \dontrun{
# #' sound = readWave2 ("yoursound.wav")
# #' tmp_snd = downsample (snd, maxformant = 5000)
# #' }


downsample = function (sound, maxformant = 7000, precision = 50){

  if (!class(sound)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  tmp_snd = sound@left
  fs = sound@samp.rate

  ratio = maxformant/(fs/2)
  fs = maxformant*2

  #if (ratio > 1) stop ("Downsampling only, sorry!")

  filter = signal::butter (5,ratio)
  tmp_snd = signal::filtfilt (filt = filter, x = tmp_snd)

  newtime = seq(1, length(tmp_snd) + 1, by = 1/ratio)
  nearest = round(newtime)
  offset = newtime - nearest
  tmp_snd = c(rep(0, precision), tmp_snd, rep(0, precision + 1))
  y = newtime * 0
  for (i in -precision:precision)
    y = y + tmp_snd[nearest + precision + i] * phonTools::sinc(offset - i, normalized = TRUE)
  y = (y / max (y)) * 1000

  sound@left = y
  sound@samp.rate = fs

  return(sound)
}


solvelpc = function (coeffs, fs = 1, nreturn=4){

  roots = polyroot(rev(coeffs))
  angs = atan2(Im(roots), Re(roots))
  formants = round(angs * (fs/(2 * pi)), 2)
  nums = order(formants)
  formants = formants[nums]
  bws = -(fs/pi) * log(abs(roots[nums]))
  touse = (formants > 0)
  out = c(formants[touse][1:nreturn], bws[touse][1:nreturn])
  return (round(out))
}


progressbar = function (i, n, start = NA, width=20){

  if (is.na(start)) stop ("No start time provided.")
    
  previous = round (width*((i-1)/n))
  progress = round (width*(i/n))

  if (previous!=progress){
    
    now = Sys.time()
    time_diff = abs (difftime(now, start, units='mins'))
    unit = "minutes"
    
    if (time_diff < 2){
      time_diff = abs (difftime(now, start, units='secs'))
      unit = "seconds"
    }
    if (time_diff > 120){
      time_diff = abs (difftime(now, start, units='hours'))
      unit = "hours"
    }
    
    pred_duration = (time_diff/i) * n
    pred_remaining = pred_duration * (1 - i/n)
    
    message1 = paste0 ("Progress: [", paste(rep("*", progress),collapse=""),paste(rep(" ", width-progress),collapse=""), "]")
    message2 = paste ("   Estimated time left:", round (pred_remaining,1),unit,"                           ")
    
    if (progress == width){
      time_diff = abs (difftime(now, start, units='mins'))
      message2 = paste ("   Total duration:", round (time_diff,2)," minutes.                             ")
    }
    
    cat (paste0 ("\r", message1, message2))
    utils::flush.console() 
  }
}


makepredictors = function (n, order){
  x = (0:(n-1)) / n
  xs = sapply (seq (0,order/2,.5), function (f) cos (f*2*pi*x))
}



