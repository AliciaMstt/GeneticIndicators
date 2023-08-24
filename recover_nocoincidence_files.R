recover_nocoincidence_files<-function(no_coincidence,target_dir, matching_row,
                                 myfile){
  
  ### This function moves a desired file from the no_coincidence directory into a target directory, assuming that the species
  ### of the file is present in the kobo output (matching_row).
  ### the function also makes a note of what was we done in the no_coincidence notes.
  
  
  # Arguments
  
  # no_coincidence= df with "file" (list of no coincidence files) and "comments" (blank) columns. 
  # matching_row = dataframe with matching row of the species of the target file in the kobo_clean data. Should include country and X_uuid columns. 
  # target_dir= target_dir as used in get_attached_files. Assumes a /No_coincidece directory exists withinit. 
  
###  Function
  
## comment
no_coincidence <- no_coincidence %>%
  mutate(comments = ifelse(
    file==myfile,
    paste("The species was evaluated in", matching_row$country_assessment, "with the Xuuid", matching_row$X_uuid,
           "The the Xuuid was not updated to the subfolder due to a kobo bug, the file was copied manually", comments), ""))

## copy file changing name to fit the output of get_attached_files() (c)
# path where the file is
path_no_coincidence<- paste0(target_dir,"/No_coincidence")

# get file extension
extension <- tools::file_ext(myfile)

# build new name
new_file_name  <- paste0(matching_row$country_assessment, "_", matching_row$taxon, "_",
                         matching_row$X_uuid, ".", extension)



# Move file
file.rename(from=file.path(path_no_coincidence, myfile), 
          to=file.path(target_dir, new_file_name))
}
