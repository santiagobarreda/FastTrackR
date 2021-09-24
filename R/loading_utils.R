

load_file_info = function (path, encoding = NA){
  
  fileinfo_exists = FALSE
  if (is.na(encoding)) encoding = ""
  
  if (file.exists (path %+% "/file_information.RDS")){
    fileinfo = readRDS(path %+% "/file_information.RDS")
    fileinfo_exists = TRUE
  }
  
  if (!fileinfo_exists & file.exists (path %+% "/file_information.csv")){
    fileinfo = tryCatch({
      utils::read.csv(path %+% "/file_information.csv",blank.lines.skip=FALSE,
                      stringsAsFactors=FALSE, encoding = encoding)
    }, warning = function(warning_condition){
      stop ("Problem with the encoding, try converting the file_information file to latin1/ANSI/ASCII or UTF-8.")
    }, error = function(error_condition) {
      stop ("Problem with the encoding, try converting the file_information file to latin1/ANSI/ASCII or UTF-8..")
    })
    fileinfo_exists = TRUE
    
    saveRDS (fileinfo, path %+% "/file_information.RDS")
  }
  if (!fileinfo_exists){
    cat ("No file information exists in your working directory (and none was provided).")
    cat ("A default one was generated and saved in your working directory.")
    fileinfo = makefileinformation(path)
    utils::write.csv (fileinfo, "file_information.csv", row.names = FALSE,quote=FALSE)
    saveRDS(fileinfo, path %+% "/file_information.RDS")
  }
  
  return (fileinfo)
}


load_segmentation_info = function (path, encoding = NA){
  
  segmentationinfo_exists = FALSE
  if (is.na(encoding)) encoding = "UTF-8"
  
  if (file.exists (path %+% "/segmentation_information.RDS")){
    segmentationinfo = readRDS(path %+% "/segmentation_information.RDS")
    segmentationinfo_exists = TRUE
  }
  
  if (!segmentationinfo_exists & file.exists (path %+% "/segmentation_information.csv")){
    segmentationinfo = tryCatch({
      utils::read.csv(path %+% "/segmentation_information.csv",blank.lines.skip=FALSE,
                      stringsAsFactors=FALSE, encoding = encoding)
    }, warning = function(warning_condition){
      stop ("Problem with the encoding, try converting the segmentation_information file to latin1/ANSI/ASCII or UTF-8.")
    }, error = function(error_condition) {
      stop ("Problem with the encoding, try converting the segmentation_information file to latin1/ANSI/ASCII or UTF-8.")
    })
    segmentationinfo_exists = TRUE
    
    saveRDS (segmentationinfo, path %+% "/segmentation_information.RDS")
  }
  
  if (segmentationinfo_exists) return (segmentationinfo)
  if (!segmentationinfo_exists) return (NA)
}



