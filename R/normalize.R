#' Normalize aggregated data
#'
#' Normalize formant frequencies using the log mean method outlined in Barreda and Nearey (2018). This approach is robust to unbalanced and missing data and is appropriate to use with formant contours. If you do not provide a vector of talkers, the data is assumed to be produced by a single talker.
#'
#' @param aggregated_data A dataframe representing data aggregated by Fast Track.
#' @param talker an optional vector indicating which talker produced each row in the data.
#' @return Another dataframe with normalized formant frequencies.
#' @references Barreda, S., & Nearey, T. M. (2018). A regression approach to vowel normalization for missing and unbalanced data. The Journal of the Acoustical Society of America, 144(1), 500-520.
#' @examples
#' data (aggregated_data)
#' normalize (aggregated_data)

normalize <- function(aggregated_data, talker = NA){

  normalized_data = aggregated_data

  if (is.na (talker[1])){
    ffs = aggregated_data[,-c(1:7)]
    gbar = rowMeans (log (aggregated_data[,-c(1:7)]))
    mod = stats::lm (gbar ~ label, contrasts=list(label="contr.sum"), data=aggregated_data)
    gbar = mod$coefficients[1]
    ffs = log(ffs) - gbar
    normalized_data[,-c(1:7)] = ffs
  }
  if (!is.na (talker[1])){
    ffs = aggregated_data[,-c(1:7)]
    talker_tmp = factor(as.numeric(factor(talker)))
    gbar = rowMeans (log (aggregated_data[,-c(1:7)]))
    mod = stats::lm (gbar ~ 0 + talker_tmp + label, contrasts=list(label="contr.sum"), data=aggregated_data)
    ntalkers = length(unique (talker_tmp))
    gbars = mod$coefficients[1:ntalkers]
    ffs = log(ffs) - gbars[talker_tmp]
    normalized_data[,-c(1:7)] = ffs
  }
  normalized_data
}

