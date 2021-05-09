
#' Extract Vowels
#'
#' Extracts vowels from larger sounds using information from matching TextGrids.
#'
#'
#' @param tgpath a path or vector of paths to textgrid files.
#' @param sndpath a path or vector of paths to wav files.
#' @param outputpath a path to the folder where you want data to go. If NA, nothing is written out. If "working" it will output in a folder called "output" in your working directory.
#' @param segmenttier the name of the tier containing segmental information used for extraction.
#' @param wordtier the name of a tier containing information about words (optional).
#' @param commenttiers a vector containing names of tiers with comments you wish to extract (optional).
#' @param omittier the name of a tier indicating which segments you wish to skip (optional).
#' @param stress a vector contianing labels you may have used to mark stress.
#' @param wordstoskip a vector containing any words you do not want to extract vowels from.
#' @return --.
#' @export
#' @examples
#' \dontrun{
#' tgpath = "yoursound3.TextGrid"
#' output = extractvowels (tgpath, wordtier="word", outputpath = "working")
#' }
#'

extractvowels = function (tgpath, sndpath,outputpath=NA, segmenttier="phone",wordtier=NA,
                             commenttiers=NA,omittier=NA, stress=c(0,1,2), wordstoskip=NA){

  if (!missing(tgpath) & !missing(sndpath)){
    if (length(tgpath) != length (sndpath)) stop ("Path lengths do not match.")
  }
  if (!missing(tgpath) & missing(sndpath)){
    base = unlist (strsplit (basename (tgpath), split ="\\."))[c(T,F)]
    dirname = dirname (tgpath)
    sndpath = dirname %+% "/" %+% base %+% ".wav"
  }
  if (missing(tgpath) & !missing(sndpath)){
    base = unlist (strsplit (basename (sndpath), split ="\\."))[c(T,F)]
    dirname = dirname (sndpath)
    tgpath = dirname %+% "/" %+% base %+% ".TextGrid"
  }
  if (missing(tgpath) & missing(sndpath)) stop ("No paths provided.")

  n = length (tgpath)
  output = list()

  segmentation_info = NULL

  for (i in 1:n){
    output[[i]] = extract.internal (tgpath[i], sndpath[i], segmenttier,wordtier,
                                    commenttiers,omittier, stress, wordstoskip)

    output[[i]][[1]] = cbind(source_file = base[i] %+% ".wav", output[[i]][[1]])
    segmentation_info = rbind(segmentation_info, output[[i]][[1]])

    ## make file information in here too
  }

  if (!is.na (outputpath)){
    if (outputpath == "working") outputpath = getwd()
    dir.create (outputpath %+% "/output", showWarnings = FALSE)
    dir.create (outputpath %+% "/output/sounds", showWarnings = FALSE)

    all_filenames = NULL
    for (i in 1:n){
      filenames = "output/sounds/" %+% sapply (output[[i]][[2]], attr, "filename")
      all_filenames = c(all_filenames, filenames)

      lapply (1:length(output[[i]][[2]]),
              function(j) tuneR::writeWave (output[[i]][[2]][[j]], filenames[j]))
    }
    file_info = data.frame (number = 1:length(all_filenames), file = basename(all_filenames),
                            label = segmentation_info$label[segmentation_info$omit==0], group = 1, color = "Blue")
    file_info$group = as.numeric (factor(file_info$label))

    utils::write.csv (segmentation_info, outputpath %+% "/output/segmentation_information.csv", row.names = FALSE)
    utils::write.csv (file_info, outputpath %+% "/output/file_information.csv", row.names = FALSE)
  }

  invisible (output)
}


#' Extract Vowels
#'
#' Extracts vowels from larger sounds using information from matching TextGrids. A complete explanation of function behavior is provided in the extractvowels reference page.
#'
#' @param tgpath a path or vector of paths to textgrid files.
#' @param sndpath a path or vector of paths to wav files.
#' @param segmenttier the name of the tier containing segmental information used for extraction.
#' @param wordtier the name of a tier containing information about words (optional).
#' @param commenttiers a vector containing names of tiers with comments you wish to extract (optional).
#' @param omittier the name of a tier indicating which segments you wish to skip (optional).
#' @param stress a vector contianing labels you may have used to mark stress.
#' @param wordstoskip a vector containing any words you do not want to extract vowels from.
#'
#' @export
#' @examples
#' \dontrun{
#' tgpath = "yoursound2.TextGrid"
#' tmp = readtextgrid (tgpath)
#' output = extract.internal (tgpath, wordtier="word",commenttiers=c("omit","comments"))
#' }

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

