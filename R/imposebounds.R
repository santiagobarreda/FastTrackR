
#' Impose boundaries on formants
#'
#' Create an updated winners dataframe or file with imposed formant boundaries.
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param boundaries a dataframe representing the boundaries for your formants for each vowel.
#' @param n_formants --.
#' @param fileinformation --.
#' @param write if TRUE, a new winners file will be written to your folder. 
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#'  # coming soon
#' }


imposebounds <- function (path, boundaries=NA, n_formants = 3, fileinformation = NA, write = FALSE){
  if (missing(path)) path = getwd()

  # or put in some default ones?
  if (is.na(boundaries)) stop ("No boundaries provided.")
  
  if (is.na(fileinformation)){
    if (!file.exists (path %+% "/file_information.csv")) stop ("No file information file available.")
    fileinformation = utils::read.csv (path %+% "/file_information.csv")
  }

  winners = data.frame (file = basename(fileinformation$file), F1=0,F2=0,F3=0)
  if (n_formants==4) winners$F4 = 0
  
  # read in intercepts and stop if they dont exist
  if (!file.exists (path %+% "/infos_aggregated/all_errors.csv") |
      !file.exists (path %+% "/infos_aggregated/all_f1.csv") |
      !file.exists (path %+% "/infos_aggregated/all_f2.csv") |
      !file.exists (path %+% "/infos_aggregated/all_f3.csv"))
    stop ("Analysis files are missing from the \'infos_aggregated\' folder. 
          Did you run the autoselectwinners step?")
  
  errors = utils::read.csv (path %+% "/infos_aggregated/all_errors.csv") 
  f1s = utils::read.csv  (path %+% "/infos_aggregated/all_f1s.csv") 
  f2s = utils::read.csv  (path %+% "/infos_aggregated/all_f2s.csv") 
  f3s = utils::read.csv  (path %+% "/infos_aggregated/all_f3s.csv")
  
  for (i in 1:nrow (winners)){
    # see if sound label is in boundary information
    spot = which (fileinformation$label[i] == boundaries$label)
    # if yes:
    if (length(spot) > 0){
      # check if each analysis falls within the bounds
      f1out = boundaries$f1lower[spot] > f1s[i,] |
              boundaries$f1upper[spot] < f1s[i,]
      f2out = boundaries$f2lower[spot] > f2s[i,] |
              boundaries$f2upper[spot] < f2s[i,]
      f3out = boundaries$f3lower[spot] > f3s[i,] |
              boundaries$f3upper[spot] < f3s[i,]
      
      # only consider those which do not defy the bounds
      use = !(f1out | f2out | f3out)

      # if there is at least one usable one:
      if (sum (use) > 0){
        # find ranking of analyses
        ord = order (errors[i,])
        # take first useable one
        winners[i,2:(2+n_formants)] = ord[use][1]
      }
      if (sum (use) == 0)
        winners[i,2:(2+n_formants)] = order (errors[i,])[1]
      
    }
    if (length(spot) == 0)
      # take lowest error
      winners[i,2:(2+n_formants)] = order (errors[i,])[1]
  }
  
  # write out if user desires
  if (write) utils::read.csv (winners, path %+% "/winners.csv")
  
  invisible (winners)
}

