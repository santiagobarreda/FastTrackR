
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
