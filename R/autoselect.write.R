
autoselect.write <- function (outputpath, output){

  winners_csv = output$winners_csv  
  errors = output$errors  
  total_errors = output$total_errors  
  coefficients = output$coefficients  
  penalties = output$penalties  
  
  utils::write.csv (winners_csv, outputpath %+% "/winners.csv", row.names=FALSE)
  utils::write.csv (winners_csv, outputpath %+% "/winners_backup.csv", row.names=FALSE)
  utils::write.csv (penalties, outputpath %+% "/penalties.csv", row.names=FALSE)
  
  colnames(total_errors) = paste0("e",1:ncol(total_errors))
  colnames(coefficients) = paste0("a",1:dim(coefficients)[2])
  dir.create (outputpath %+% "/infos_aggregated/", showWarnings = FALSE)
  dir.create (outputpath %+% "/regression_infos/", showWarnings = FALSE)
  
  utils::write.csv (total_errors, outputpath %+% "/infos_aggregated/all_errors.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,1,1], outputpath %+% "/infos_aggregated/all_f1s.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,2,1], outputpath %+% "/infos_aggregated/all_f2s.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,3,1], outputpath %+% "/infos_aggregated/all_f3s.csv", row.names=FALSE)
  
  nf = dim(errors)[3]
  
  info_files = list.files (outputpath %+% "/infos/")
  reg_files = tools::file_path_sans_ext( list.files (outputpath %+% "/sounds/"))
  reg_files = paste0 (reg_files,".txt")
  
  for (i in 1:length (info_files)){
    tmp_info  = readLines (outputpath %+% "/infos/" %+% info_files[i])[1:9]
    
    tmp_info[10] = "Winner is:"
    tmp_info[11] = winners_csv[i,2]
    
    errors_out = t(cbind(total_errors[i,],errors[i,,1:nf]))
    errors_out = apply (errors_out, 1, paste, collapse = " ")
    if(nf==3) tmp_info[12] = "Errors (total,f1,f2,f3):"
    if(nf==4) tmp_info[12] = "Errors (total,f1,f2,f3,f4):"
    tmp_info = c(tmp_info, errors_out)
    
    coeffs_out = coefficients[i,winners_csv[i,2],,]
    coeffs_out = apply (coeffs_out, 1, paste, collapse = " ")
    
    tmp_info = c(tmp_info,"Coefficients are (row-wise by formant):")
    tmp_info = c(tmp_info,coeffs_out)
    
    writeLines (tmp_info, outputpath %+% "/infos/" %+% info_files[i])

    # writeLines (tmp_info, outputpath %+% "/regression_infos/" %+% info_files[i])
    
    #reg_files
    base = tools::file_path_sans_ext(reg_files[i])
    
    # regression information text file output 
    tmp_reg = list("Regression analysis information for: " %+% base,
               "number of formants: " %+% nf,
               "number of coefficients: " %+% tmp_info[7])
    count = 4
    for (j in 1:as.numeric(tmp_info[3])){
      tmp_reg[[count]] = paste (errors[i,j,], collapse =" ")
      count = count + 1
      tmp_reg[[count]] = t(coefficients[i,j,,])
      count = count + 1
    }
    tmp_reg[[count]] = paste ("winner is: ", winners_csv[i,2])
    
    write(tmp_reg[[1]],paste0(outputpath,"/",reg_files[i]), append=FALSE)
    lapply(2:length(tmp_reg), 
           function(x) 
             write(tmp_reg[[x]], paste0(outputpath,"/",reg_files[i]), append=TRUE,ncolumns=nrow(tmp_reg[[5]]))
          )
  }
}





