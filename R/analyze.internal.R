
analyze.internal = function (tmp_snd, from = 4800, to = 6800, nsteps=12,
                    windowlength = 0.05, timestep = 0.0025){

  if (!class(tmp_snd)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  ffs = list(rep(0,nsteps))
  count = nsteps
  maxformants = round(seq(from,to,length.out=nsteps))

  for (i in rev(maxformants)){
    tmp_snd = downsample (tmp_snd, maxformant = i)
    tmp_snd@left = tmp_snd@left / max(tmp_snd@left)
    ffs[[count]] = trackformants (tmp_snd,maxformant = i,timestep=timestep)
    count = count - 1
  }
  attr(ffs, "object") = "fileffs"
  attr(ffs, "maxformants") = maxformants
  ffs
}
