
#' Get winning analyses
#'
#' This function will read winning analyses into R based on the content of the 'winners.csv' file. The required formant and csv data can be pre-loaded with the readformants() and readcsvs() functions provided in this package. Reading the formant data can be a bit slow so loading it once and passing it to the function will speed things up quite a bit if this function will be called repeatedly.
#' 
#' At the moment the function does not move copies of the winning formant files into a new folder (as with the Praat version) but that functionality is coming soon. 
#'
#' @param path the path to the working directory for the Fast Track project. If no path is provided this is the working directory.
#' @param winners an optional dataframe representing the data in the 'winners.csv' file.
#' @param selection_info --.
#' @param formants an optional list of lists representing all of the possible formant analyses.
#' @param asone if TRUE, the formant data is stuck together into one dataframe and filenames are indicated in a new column. If FALSE, a list of dataframes is returned and each list element is named after the file.
#' @param write_csv if TRUE, the data is written out to a CSV folder in the working directory.
#' @param move_formants --.
#' @return A dataframe or list of dataframes, as per the asone parameter.
#' @export
#' @examples
#' \dontrun{
#' getwinners ()
#' tmp = getwinners (getwd(), winners$winners, formants, write = TRUE)
#' }

getwinners <- function (path = NA, formants = NA, winners = NA, selection_info = NA, asone = TRUE, 
                        write_csv = FALSE,move_formants = FALSE){

  if (missing(path)) path = getwd()
  
  winners_exists = FALSE
  if (any(!is.na(winners))) winners_exists = TRUE
  
  if (!winners_exists & file.exists(path %+% "/winners.csv")){
    winners = utils::read.csv (path %+% "/winners.csv")
    winners_exists = TRUE
  }
  # read in data
  ## stop if files dont exist, check for both

  labels = NA
  if (all(is.na(selection_info)) & file.exists(path %+% "/selection_information.RDS")){ 
    selection_info = readRDS (path %+% "/selection_information.RDS")
    winners = selection_info$winners_csv
  }

  if (!all(is.na(selection_info))){
    winners = selection_info$winners_csv
    winners_exists = TRUE
    labels = selection_info$labels
  }

  if (all(is.na(formants))){
    if (file.exists(path %+% "/formants.RDS")) formants = readRDS (path %+% "/formants.RDS")
    if (!file.exists(path %+% "/formants.RDS")) formants = readformants (path)
  }
  
  if (!winners_exists) stop ("No winners or selection information available or provided.")

  # find number of formants
  nf = 3 + sum (colnames(winners)=="F4")

  csvs = list ()

  for (i in 1:nrow (winners)){
    ws = unlist (winners[i,2:(2+nf)])
    allsame = (sum (diff (ws))==0)

    # if individual formants are not being selected across analyses
    if (allsame){
      tmp_formants = formants[[i]][[ws[1]]]

      n = nrow(tmp_formants)-1
      time = attr (tmp_formants,"w1") + attr (tmp_formants,"timestep")*(0:n)
      time = round (time, 4)
      tmp_csv = data.frame (time = time)

      if (nf == 3){
        tmp_formants = tmp_formants[, c(1,4,2,5,3,6)]
        tmp_csv[,2:7] = tmp_formants
      }
      if (nf == 4){
        tmp_formants = tmp_formants[, c(1,5,2,6,3,7,4,8)]
        tmp_csv[,2:9] = tmp_formants
      }
      csvs[[i]] = tmp_csv
    }
    # if individual formants ARE being selected across analyses
    if (!allsame){
      tmp_formants1 = round(formants[[i]][[ws[2]]],1)
      tmp_formants2 = round(formants[[i]][[ws[3]]],1)
      tmp_formants3 = round(formants[[i]][[ws[4]]],1)
      if (nf == 4) tmp_formants4 = round(formants[[i]][[ws[5]]],1)

      n = nrow(tmp_formants1)-1
      time = attr (tmp_formants1,"w1") + attr (tmp_formants1,"timestep")*(0:n)
      time = round (time, 4)
      tmp_csv = data.frame (time = time)

      if (nf == 3)
        minrow = min (nrow(tmp_formants1), nrow(tmp_formants2), nrow(tmp_formants3))
      if (nf == 4)
        minrow = min (nrow(tmp_formants1), nrow(tmp_formants2), nrow(tmp_formants3), nrow(tmp_formants4))

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
  }
  if (move_formants){
    dir.create (path %+% "/formants_winners", showWarnings = FALSE)
    
    for (i in 1:nrow (winners)){
      file.copy (path %+% "/formants/" %+% winners$file[i] %+% "_" %+% winners$winner[i] %+% "_.Formant",
                 path %+% "/formants_winners/" %+% winners$file[i] %+% "_" %+% "winner" %+% "_.Formant")
    }
  }

  if (write_csv){
    dir.create (path %+% "/csvs", showWarnings = FALSE)
    filenames = path %+% "/csvs/" %+% winners$file %+% ".csv"
    lapply (1:length(csvs),
            function(j) utils::write.csv (csvs[[j]], filenames[j],row.names=FALSE))
  }
  if (asone){
    rows = sapply (csvs, nrow)
    filename = sapply (1:length(rows), function(i) rep (winners$file[i], rows[i]))
    filename = unlist(filename)
    csvs = do.call(rbind, csvs)
    csvs$file = filename
  }
  return (csvs)
  
}

