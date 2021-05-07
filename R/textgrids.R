


#' Extract Sounds
#'
#' Extracts vowels from larger sounds using information from matching TextGrids.
#'
#'
#' @param tgpath --.
#' @param sndpath --.
#' @param outputpath --.
#' @param segmenttier --.
#' @param wordtier --.
#' @param commenttiers --.
#' @param omittier --.
#' @param stress --.
#' @param wordstoskip --.
#' @return --.
#' @export
#' @examples
#' \dontrun{
#' tgpath = "yoursound2.TextGrid"
#' tmp = readtextgrid (tgpath)
#' output = extractsounds (tgpath, wordtier="word",commenttiers=c("omit","comments"))
#' }

extractsounds = function (tgpath, sndpath, outputpath="output", segmenttier="phone",wordtier=NA,
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
  sound = tuneR::readWave(sndpath)
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

  if (!missing (wordtier)){
    midpoints = (extract$start+extract$end)/2
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
  if (!missing (omittier)){
    midpoints = (extract$start+extract$end)/2
    vowelomittiers = sapply (1:length (midpoints), function (i){
      use = (midpoints[i] > tgdata[[omittier]]$start & midpoints[i] < tgdata[[omittier]]$end)
      which.max (use)
    })
    whichkeep = which(tgdata[[omittier]]$label=="")
    extract$omit = as.numeric(extract$omit | !(vowelomittiers %in% whichkeep))
  }
  if (!missing (commenttiers)){
    midpoints = (extract$start+extract$end)/2

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

  times = extract[,c("start","end","omit")]
  times[,1] = times[,1] - 0.025
  times[,2] = times[,2] + 0.025
  times[times[,1] < 0,1] = 0
  times[times[,2] > duration,2] = duration
  sounds = lapply (1:nrow (extract),
                   function (i) tuneR::extractWave (sound,
                                                    from=times[i,1],to=times[i,2],
                                                    xunit='time',interact=FALSE))
  output = list (data_out, sounds)
  output
}




#extractsounds = function (tgpath, sndpath, outputpath="output", segmenttier="phone",wordtier=NA,
#commenttiers=NA,omittier=NA, stress=c(0,1,2), wordstoskip=NA,writedata=TRUE){

#dir.create (outputpath, showWarnings = FALSE)
#dir.create (outputpath %+% "/sounds", showWarnings = FALSE)
#filenames = paste0 (outputpath, "/sounds/", base, "_", addzeros(1:nrow(extract)), ".wav")
#for (i in 1:length (filenames))  tuneR::writeWave (sounds[[i]], filenames[i])

#}




#' Load textgrid information into R
#'
#' Create a list of dataframes from a textgrid. Each list element is a dataframe representing information from a different interval tier. Each dataframe contains the interval label, start time, end time, and duration (all in milliseconds).
#'
#' @param path the path to the Praat textgrid file you want to read.
#' @return A list of dataframes, one containing the data from each interval tier in the textgrid. Each dataframe is named after the tier.
#' @export
#' @examples
#' \dontrun{
#' path = "tmp/yoursound2.TextGrid"
#' readtextgrid (path)
#' }

readtextgrid <- function (path){

  if (!file.exists(path)) stop ("File does not exist. The path is probably wrong.")
  tg = readLines(path)

  filetype = "neither"
  if (tg[1] == "File type = \"ooTextFile\"") filetype = "long"
  if (tg[1] == "File type = \"ooTextFile short\"") filetype = "short"
  if (filetype == "neither") stop ("Not a valid Praat file.")
  filetype

  tiers = grep ("IntervalTier", tg)
  tier_names = tg[tiers+1]
  if (filetype=="long"){
    tier_names = gsub ("name = ", "", tier_names)
    tier_names = gsub (" ", "", tier_names)
  }
  tier_names = gsub ("\\\"", "", tier_names)
  tier_n = getnumbers (tg[tiers+4])

  outputs = list ()
  for (i in 1:length (tiers)){
    if (filetype=="short"){
      start = (tiers[i]+5)
      end = (tiers[i]+4+tier_n[i]*3)

      output = matrix (tg[start:end],(end-start+1)/3,3,byrow=TRUE)
      output = data.frame (output[,c(3,1,2)])
      colnames(output) = c("label", "start", "end")
      output$duration = as.numeric(output[,3])-as.numeric(output[,2])

      output[,1] = gsub ("text = ", "", output[,1])
      output[,1] = gsub (" ", "", output[,1])
      output[,1] = gsub ("\\\"", "", output[,1])
    }
    if (filetype=="long"){
      start = (tiers[i]+5)
      end = (tiers[i]+4+as.numeric(tier_n[i])*4)

      output = matrix (tg[start:end],(end-start+1)/4,4,byrow=TRUE)
      output = data.frame (output[,c(4,2,3)])
      colnames(output) = c("label", "start", "end")
      output[,2] = as.numeric(gsub ("xmin = ", "", output[,2]))
      output[,3] = as.numeric(gsub ("xmax = ", "", output[,3]))
      output$duration = as.numeric(output[,3])-as.numeric(output[,2])

      output[,1] = gsub ("text = ", "", output[,1])
      output[,1] = gsub (" ", "", output[,1])
      output[,1] = gsub ("\\\"", "", output[,1])
    }
    outputs[[i]] = output
  }
  names (outputs) = tier_names
  outputs
}

