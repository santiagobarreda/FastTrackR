

extract.internal = function (tgpath, sndpath, segmenttier="phone",wordtier=NA,
                           commenttiers=NA,omittier=NA, stress=c(0,1,2), wordstoskip=NA){

  vowelstoextract = fasttrackr::vowelstoextract

  if (!missing(tgpath) & !missing(sndpath)){
    base = strsplit (basename (tgpath), split ="\\.")[[1]][1]
    dirname = dirname (tgpath)
  }
  if (!missing(tgpath) & missing(sndpath)){
    base = strsplit (basename (tgpath), split ="\\.")[[1]][1]
    dirname = dirname (tgpath)
    sndpath = dirname %+% "/" %+% base %+% ".wav"
  }
  if (missing(tgpath) & !missing(sndpath)){
    base = strsplit (basename (sndpath), split ="\\.")[[1]][1]
    dirname = dirname (sndpath)
    tgpath = dirname %+% "/" %+% base %+% ".TextGrid"
  }
  if (missing(tgpath) & missing(sndpath)) stop ("No paths provided.")

  tgdata = readtextgrid(tgpath)
  sound = readWave2 (sndpath)
  duration = length(sound@left)/sound@samp.rate

  phones = tgdata[[segmenttier]][,1]
  stresses = NA
  if (!is.na(stress[1])){
    stresses = substr( phones , nchar(phones) , nchar(phones))
    phones = gsub('.{1}$', '', phones)
  }

  use = phones %in% vowelstoextract[,2]
  if (!is.na(stress[1])) use = (phones %in% vowelstoextract[,2]) & (stresses %in% stress)

  extract = tgdata[[segmenttier]]
  extract$interval = 1:nrow(extract)
  extract = tgdata[[segmenttier]][use,]
  extract[,1] = phones[use]
  extract = extract[,c(1,4,2,3)]

  segmentlabels = c("-",tgdata[[segmenttier]]$label,"-")
  use = which (phones %in% vowelstoextract[,2])
  extract$previous_sound = segmentlabels[use]
  extract$next_sound = segmentlabels[use+2]

  if (!is.na(stresses[1])) extract$stress = stresses[use]

  extract$omit = as.numeric (extract$duration < 0.03)

  if (!is.na (wordtier)){
    midpoints = (as.numeric(extract$start)+as.numeric(extract$end))/2
    vowelwordtiers = sapply (1:length (midpoints), function (i){
      use = (midpoints[i] > tgdata[[wordtier]]$start & midpoints[i] < tgdata[[wordtier]]$end)
      which.max (use)
    })
    wordlabels = c("-",tgdata[[wordtier]]$label,"-")
    extract$word = wordlabels[vowelwordtiers+1]
    extract$word_interval = vowelwordtiers

    extract$word_start = tgdata[[wordtier]]$start[vowelwordtiers]
    extract$word_end = tgdata[[wordtier]]$end[vowelwordtiers]

    extract$previous_word = wordlabels[vowelwordtiers]
    extract$next_word = wordlabels[vowelwordtiers+2]

    if (!is.na (wordstoskip[1])) extract = extract[!(extract$word %in% wordstoskip),]
  }
  if (!is.na (omittier)){
    print (omittier)
    midpoints = (as.numeric(extract$start)+as.numeric(extract$end))/2
    vowelomittiers = sapply (1:length (midpoints), function (i){
      use = (midpoints[i] > tgdata[[omittier]]$start & midpoints[i] < tgdata[[omittier]]$end)
      which.max (use)
    })
    whichkeep = which(tgdata[[omittier]]$label=="")
    extract$omit = as.numeric(extract$omit | !(vowelomittiers %in% whichkeep))
  }
  if (!is.na (commenttiers[1])){
    midpoints = (as.numeric(extract$start)+as.numeric(extract$end))/2

    for (i in 1:length (commenttiers)){
      tier = commenttiers[i]
      vowelomittiers = sapply (1:length (midpoints), function (i){
        use = (midpoints[i] > tgdata[[tier]]$start & midpoints[i] < tgdata[[tier]]$end)
        which.max (use)
      })
      extract[[tier %+% "_comment"]] = tgdata[[tier]]$label[vowelomittiers]
    }
  }

  extract = cbind (filename="--", extract)
  extract$filename[extract$omit==0] = paste0 (base, "_", addzeros(1:sum(extract$omit==0)), ".wav")

  data_out = extract

  extract = extract[extract$omit==0,]

  times = extract[,c("start","end","omit","filename")]
  times[,1] = as.numeric(times[,1]) - 0.025
  times[,2] = as.numeric(times[,2]) + 0.025
  times[times[,1] < 0,1] = 0
  times[times[,2] > duration,2] = duration

  sounds = lapply (1:nrow (extract),
                   function (i) extractWave2(sound,times[i,1],times[i,2],times$filename[i]))
  output = list (data_out, sounds)

  output
}

