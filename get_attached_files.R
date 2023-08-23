### Developed by Luis Castillo


# Define a function to process files
get_attached_files <- function(root_dir, target_dir, kobo_output) {

## What this function does
#  If information of more than 25 populations was be used to collect data for Ne >500 indicator (Section 5 of the Kobo form) 
# it is possible to use a template to upload data instead of using the kobo form. 
# This function parses kobo attachments directories to look for files with population data, 
# matches them against their metadata (captured in kobo) and stores them in an target directory 
  
## Arguments:
# root_dir = path to the directory resulting from downloading the Kobo Attachments and unziping it. 
            # Normally this directory is called "attachments" and has many subdirectories with a long alphanumeric name, 
            # corresponding to the Xuuid of the record. 
  
# target_dir = path to the directory where you want to save the output of running this function 
# kobo_output = a data frame result with the raw (hundred of columns) Kobo output as downloaded from Kobo 

## Needed libraries:
  # library(dplyr)
  # library(readr)
  # library(stringr)
  # library(tools)
  
  
  ## Create target directory overwriting previous content
  # Remove existing target directory if it exists
  if (dir.exists(target_dir)) {
    # Remove all files and subdirectories within the target directory
    files_to_remove <- list.files(target_dir, full.names = TRUE, recursive = TRUE)
    file.remove(files_to_remove)
    
    # Remove the target directory itself
    unlink(target_dir, recursive = TRUE)
  }
  # Recreate the target directory
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  kobo_output <- kobo_output
  

  ###  Get the attachments files that need processing
  # Search for directories and create objects for outputs
  subfolders <- list.dirs(path = root_dir, recursive = TRUE) # List all subfolders
  output_txt <- file.path(target_dir, 'file_names.txt') # Output file for logging
  output_conn <- file(output_txt, open = "wt") # Open the output file in write mode
  
  # Iterate through subfolders and process files
  for(subfolder in subfolders){
    # List all text and CSV files in the subfolder
    files <- list.files(path = subfolder, pattern = "\\.(txt|csv)$", full.names = TRUE)
    if(length(files) > 0){
      for(file in files){
        # Extract details like subfolder name, file extension, and new file name
        subfolder_name <- basename(subfolder) # this equals the Xuuid in Kobo
        file_name_without_extension <- subfolder_name
        extension <- tools::file_ext(file)
        
        # Look for a match in kobo_output
        matching_row <- kobo_output %>%
          filter(X_uuid == file_name_without_extension)
  
        
        ## Move to corresponding directory depending on match
        
        if (nrow(matching_row) > 0) {
          # If match found, copy the file to target directory and log the match
          cat("\n ########        Processing file:", "\n", file) # this creates a nice space between files to facilitate reading the log
          print(paste("the uuid of the file was found in the kobo metadata and the file was copied to", target_dir))
          
          # new file name
          new_file_name <- paste0(matching_row$country_assessment, "_", matching_row$taxon, "_",
                                  subfolder_name, ".", extension)
          # copy file
          file.copy(from = file, to = file.path(target_dir, new_file_name), overwrite = TRUE)
          cat(paste("match +", file_name_without_extension), file = output_conn, sep = "\n")
          
        } else {
          # If no match was found, copy the file to 'No_coincidence' folder and log the non-match
          print(paste("Processing file:", file))
          print(paste0("the uuid of the file was NOT found in the kobo metadata and the file was copied to ", target_dir, '/No_coincidence', ". This likely means that the species assessment was corrected in kobo and the system does not delete the old file. But check if you are unsure"))
          no_coincidence_folder <- file.path(target_dir, 'No_coincidence')
          
          # new file name
          new_file_name <- paste0(subfolder_name, ".", extension)
          
          # copy to no_coincidence
          dir.create(no_coincidence_folder, showWarnings = FALSE)
          file.copy(from = file, to = file.path(no_coincidence_folder, new_file_name), overwrite = TRUE)
          cat(file_name_without_extension, file = output_conn, sep = "\n")
        }
      }
    }
  }
  
  close(output_conn) # Close the output file connection
  
}