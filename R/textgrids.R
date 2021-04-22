
#' Load textgrid information into R
#'
#' Create a list of dataframes from a textgrid. Each list element is a dataframe representing information from a different interval tier. Each dataframe contains the interval label, start time, end time, and duration (all in milliseconds).
#'
#' @param path the path to the Praat textgrid file you want to read.
#' @return A list of dataframes, one containing the data from each interval tier in the textgrid. Each dataframe is named after the tier.
#' @export
#' @examples
#' \dontrun{
#' path = "../example data/textgrids/NCF011_01-02-long.TextGrid"
#' readtextgrid (path)
#' }

readtextgrid <- function (path){

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

