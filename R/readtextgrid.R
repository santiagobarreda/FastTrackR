
#' Load textgrid information into R
#'
#' Creates a list of dataframes from a textgrid. Each list element is a dataframe representing information from a different interval tier. Each dataframe contains the interval label, start time, end time, and duration (all in milliseconds).
#'
#' @param path the path to the Praat textgrid file you want to read.
#' @return A list of dataframes, one containing the data from each interval tier in the textgrid. Each list element is named after the tier.
#' @param encoding --.
#' @export
#' @examples
#' \dontrun{
#' path = "tuvesamisol.TextGrid"
#'
#' path = "testing.txt"
#' path = "WS005-Melissa.TextGrid"
#' readtextgrid (path)
#' }

readtextgrid <- function (path, encoding = "UTF-8"){

  if (!file.exists(path)) stop ("File does not exist. The path is probably wrong.")
  
  tg = tryCatch({
    utils::read.csv(path,blank.lines.skip=FALSE, skipNul = FALSE,header=FALSE,
                    stringsAsFactors=FALSE, fileEncoding = encoding)[,1]
  }, warning = function(warning_condition){
    stop ("Problem reading in file_information. Please specify an encoding.")
  }, error = function(error_condition) {
    stop ("Problem reading in file_information. Please specify an encoding.")
  })

  # check to make sure it is a valid textgrid file
  # and check if it is in long or short format
  filetype = "neither"
  if (substr(tg[4],1,1) == "x") filetype = "long"
  if (substr(tg[4],1,1) != "x") filetype = "short"
  if (filetype == "neither") stop ("Not a valid TextGrid file.")
  filetype

  # get tier names and locations of tiers in vector
  # as well as the number of intervals in each tier
  tiers = grep ("IntervalTier", tg)
  tier_names = tg[tiers+1]
  if (filetype=="long"){
    tier_names = gsub ("name = ", "", tier_names)
    tier_names = gsub (" ", "", tier_names)
  }
  tier_names = gsub ("\\\"", "", tier_names)
  tier_n = getnumbers (tg[tiers+4])

  outputs = list ()
  # for each tier
  for (i in 1:length (tiers)){
    # short format processing
    if (filetype=="short"){
      start = (tiers[i]+5)
      end = (tiers[i]+4+tier_n[i]*3)

      # subsection of vector corresponding to each tier is changed into 
      # a matrix and cleaed up for output
      output = matrix (tg[start:end],(end-start+1)/3,3,byrow=TRUE)
      output = data.frame (output[,c(3,1,2)])
      colnames(output) = c("label", "start", "end")
      output$duration = as.numeric(output[,3])-as.numeric(output[,2])

      output[,1] = gsub ("text = ", "", output[,1])
      output[,1] = gsub (" ", "", output[,1])
      output[,1] = gsub ("\\\"", "", output[,1])
    }
    # long format processing
    if (filetype=="long"){
      start = (tiers[i]+5)
      end = (tiers[i]+4+as.numeric(tier_n[i])*4)

      # same as above but some more processing related to the extra 
      # text in the long format
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
  
  # return a list of dataframes, one for each interval tier. 
  names (outputs) = tier_names
  outputs
}

