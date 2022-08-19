open_csv_files_in_globalenv <- function(file_path){ 
  #grab file name without path or extension
  export_name <- basename(file_path) %>%
    file_path_sans_ext()
  #read csv file and save as the name1 in the global environment 
  assign(export_name, read_csv(file_path), envir = .GlobalEnv)
}