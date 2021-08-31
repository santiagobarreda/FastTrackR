
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
#' @param write if TRUE, 'sounds.RDS' and 'file_information.RDS' are saved to the working directory. 
#' @return --.
#' @export
#' @examples
#' \dontrun{
#' tgpath = "tuvesamisol.TextGrid"
#' tgpath = "WS005-Melissa.TextGrid"
#' 
#' output = extractvowels (tgpath, stress = NA)
#' output = extractvowels (tgpath, wordtier=NA)
#' output = extractvowels (tgpath, wordtier=2)
#' }
#'

extractvowels = function (tgpath=NA, sndpath=NA,outputpath=NA, segmenttier=1,
                          wordtier=NA,commenttiers=NA,omittier=NA, stress=c(0,1,2), 
                          wordstoskip=NA, write = TRUE){

  if (!is.na(tgpath) & !is.na(sndpath)){
    if (length(tgpath) != length (sndpath)) stop ("Path lengths do not match.")
  }
  if (!is.na(tgpath) & is.na(sndpath)){
    base = unlist (strsplit (basename (tgpath), split ="\\."))[c(T,F)]
    dirname = dirname (tgpath)
    sndpath = dirname %+% "/" %+% base %+% ".wav"
  }
  if (is.na(tgpath) & !is.na(sndpath)){
    base = unlist (strsplit (basename (sndpath), split ="\\."))[c(T,F)]
    dirname = dirname (sndpath)
    tgpath = dirname %+% "/" %+% base %+% ".TextGrid"
  }
  if (is.na(tgpath) & is.na(sndpath)) stop ("No paths provided.")

  n = length (tgpath)
  output = list()

  segmentation_info = NULL
  file_information = NULL
  sounds = NULL

  for (i in 1:n){
    output = extract.internal (tgpath[i], sndpath[i], segmenttier,wordtier,
                                    commenttiers,omittier, stress, wordstoskip)

    output[[1]] = cbind(source_file = base[i] %+% ".wav", output[[1]])
    segmentation_info = rbind(segmentation_info, output[[1]])
    
    colors = rep (c("Blue","Green","Magenta","Black",
                   "Lime","Purple","Teal","Navy","Pink",
                   "Maroon","Olive","Grey","Red"), 5)
    colors = colors[as.numeric(factor(segmentation_info$label))]

    tmp_file_information = data.frame(number = 1, 
                                  file = segmentation_info$file,
                                  label = segmentation_info$label, 
                                  group = as.numeric(factor(segmentation_info$label)),
                                  color = colors)
    file_information = rbind (file_information, tmp_file_information)
    
    sounds = c(sounds, output[[2]])
  }
  file_information$number = 1:nrow(file_information)

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

    utils::write.csv (segmentation_info, outputpath %+% "/output/segmentation_information.csv", row.names = FALSE)
    utils::write.csv (file_information, outputpath %+% "/output/file_information.csv", row.names = FALSE)
  }
  
  file_information = list (file_information = file_information, 
                           segmentation_information = segmentation_info)
  
  output = list (file_information = file_information, sounds = sounds)
  
  if (write){
    saveRDS (output[[1]], getwd() %+% "/file_information.RDS")
    saveRDS (output[[2]], getwd() %+% "/sounds.RDS")
  }
    
  invisible (output)
}











