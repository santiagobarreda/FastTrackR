
#' Get winning analyses
#'
#' This function will read winning analyses into R based on the content of the 'winners.csv' file. The required formant and csv data can be pre-loaded with the readformants() and readcsvs() functions provided in this package. Reading the formant data can be a bit slow so loading it once and passing it to the function will speed things up quite a bit if this function will be called repeatedly.
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param winners an optional dataframe representing the data in the 'winners.csv' file.
#' @param formants an optional list of lists representing all of the possible formant analyses.
#' @param csvs and optional list of dataframes representing all the current formant data.
#' @param asone if TRUE, the formant data is stuck together into one dataframe and filenames are indicated in a new column. If FALSE, a list of dataframes is returned and each list element is named after the file.
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#' csvs = readcsvs ()
#' }

getwinners <- function (path, winners, formants, csvs, asone = TRUE){
  if (missing(path)) path = getwd()

  # read in data
  if (missing(winners)) winners = utils::read.csv (path %+% "/winners.csv")
  if (missing(formants)) formants = readformants (path)
  if (missing(csvs)) csvs = readcsvs (path, asone = FALSE)

  # find number of formants
  nf = 3 + sum (colnames(winners)=="F4")

  for (i in 1:nrow (winners)){
    ws = unlist (winners[i,2:(2+nf)])
    allsame = (sum (diff (ws))==0)

    # if individual formants are not being selected across analyses
    if (allsame){
      tmp_formants = round(formants[[i]][[ws[1]]],1)
      tmp_csv = csvs[[i]]

      minrow = min (nrow (tmp_csv), nrow(tmp_formants))

      if (nf == 3){
        tmp_formants = tmp_formants[1:minrow, c(1,4,2,5,3,6)]
        tmp_csv[1:minrow,2:7] = tmp_formants
      }
      if (nf == 4){
        tmp_formants = tmp_formants[1:minrow, c(1,5,2,6,3,7,4,8)]
        tmp_csv[1:minrow,2:9] = tmp_formants
      }
      csvs[[i]] = tmp_csv
    }
    # if individual formants ARE being selected across analyses
    if (!allsame){
      tmp_formants1 = round(formants[[i]][[ws[2]]],1)
      tmp_formants2 = round(formants[[i]][[ws[3]]],1)
      tmp_formants3 = round(formants[[i]][[ws[4]]],1)
      if (nf == 4) tmp_formants4 = round(formants[[i]][[ws[5]]],1)
      tmp_csv = csvs[[i]]

      if (nf == 3)
        minrow = min (nrow (tmp_csv), nrow(tmp_formants1), nrow(tmp_formants2),
                      nrow(tmp_formants3))
      if (nf == 4)
        minrow = min (nrow (tmp_csv), nrow(tmp_formants1), nrow(tmp_formants2),
                      nrow(tmp_formants3), nrow(tmp_formants4))

      if (nf == 3){
        tmp_formants = cbind (tmp_formants1[1:minrow, c(1,4)],
                              tmp_formants2[1:minrow, c(1,4)+1],
                              tmp_formants3[1:minrow, c(1,4)+2])
        tmp_csv[1:minrow,2:7] = tmp_formants
      }
      if (nf == 4){
        tmp_formants = cbind (tmp_formants1[1:minrow, c(1,4)],
                              tmp_formants2[1:minrow, c(1,4)+1],
                              tmp_formants3[1:minrow, c(1,4)+2],
                              tmp_formants4[1:minrow, c(1,4)+3])
        tmp_csv[1:minrow,2:9] = tmp_formants
      }
      csvs[[i]] = tmp_csv
    }
    if (asone) csvs[[i]]$file = winners$file[i]
  }
  if (asone) csvs = do.call(rbind, csvs)
  invisible (csvs)
}

