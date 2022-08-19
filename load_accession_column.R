load_accession_column <- function(file_path){ 
  #grab file name without path or extension
  export_name <- basename(file_path) %>%
    file_path_sans_ext() %>% 
    str_c("_accession")
  #View(export_name)
  accession_column <- read_delim(file_path, delim = "\t") %>% 
    select(Accession)
  #read delim file and save as the name1 in the global environment 
  assign(export_name, accession_column, envir = .GlobalEnv)
}
#Give this function a list of paths for protein.txt files from proteomic discoverer software
#This function will open all and extract the accession data only and put it into the globalenv
#to run
#lapply(list_file_paths,load_accession_column)