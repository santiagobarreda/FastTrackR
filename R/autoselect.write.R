
autoselect.write <- function (outputpath, output){

  winners_csv = output$winners_csv  
  errors = output$errors  
  total_errors = output$total_errors  
  coefficients = output$coefficients  
  
  utils::write.csv (winners_csv, outputpath %+% "/winners.csv", row.names=FALSE)
  
  colnames(total_errors) = paste0("e",1:ncol(total_errors))
  colnames(coefficients) = paste0("a",1:dim(coefficients)[2])
  dir.create (outputpath %+% "/infos_aggregated/", showWarnings = FALSE)
  
  utils::write.csv (total_errors, outputpath %+% "/infos_aggregated/all_errors.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,1,1], outputpath %+% "/infos_aggregated/all_f1s.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,2,1], outputpath %+% "/infos_aggregated/all_f2s.csv", row.names=FALSE)
  utils::write.csv (coefficients[,,3,1], outputpath %+% "/infos_aggregated/all_f3s.csv", row.names=FALSE)
  
  nf = dim(errors)[3]
  
  info_files = list.files (outputpath %+% "/infos/")
  for (i in 1:length (info_files)){
    tmp_info  = readLines (outputpath %+% "/infos/" %+% info_files[i])
    tmp_info[11] = winners_csv[i,2]
    
    errors_out = t(cbind(total_errors[i,],errors[i,,1:nf]))
    errors_out = apply (errors_out, 1, paste, collapse = " ")
    tmp_info[13:(12+length(errors_out))] = errors_out
    
    coeffs_out = coefficients[i,winners_csv[i,2],,]
    coeffs_out = apply (coeffs_out, 1, paste, collapse = " ")
    
    spot=which(tmp_info=="Coefficients are (row-wise by formant):")
    tmp_info = c(tmp_info[1:spot],coeffs_out)
    
    writeLines (tmp_info, outputpath %+% "/infos/" %+% info_files[i])
  }
}
