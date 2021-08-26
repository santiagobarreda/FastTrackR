


trackformants.internal = function (snd, n_formants = 4, from = 5300, to = 7000, nsteps=16,
                    windowlength = 0.05, timestep = 0.0025, label = NA){

  if (!class(snd)=="Wave") stop ("Sound must be a Wave object read in using the tuneR package.")

  ffs = list(rep(0,nsteps))
  count = nsteps
  maxformants = round(seq(from,to,length.out=nsteps))

  tmp_snd = snd
  for (i in count:1){
    if (i == count)
      tmp_analysis = findformants (tmp_snd,n_formants=n_formants,maxformant = maxformants[i],
                                    timestep=timestep,returnsound=TRUE, preemphasis_frequency=50, label=label)
    if (i < count)
      tmp_analysis = findformants (tmp_snd,n_formants=n_formants,maxformant = maxformants[i],
                                    timestep=timestep,returnsound=TRUE, preemphasis_frequency=NA, label=label)
    ffs[[i]] = tmp_analysis[[1]]
    ffs[[i]][is.na(ffs[[i]])] = 0
    tmp_snd = tmp_analysis[[2]]
  }
  
  attr(ffs, "filename") = substr (snd@filename,1,nchar(snd@filename)-4)
  attr(ffs, "duration") = length(tmp_snd@left)/tmp_snd@samp.rate
  attr(ffs, "timestep") = timestep
  attr(ffs, "label") = label
  attr(ffs, "cutoffs") = maxformants
  attr(ffs, "class") = "formants_single"
  
  ffs
}



