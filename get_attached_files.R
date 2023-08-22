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
  
  ### 1) Separate data in kobo_output as in get_indicator1 function
  
    # create a variable with the full taxon name if this variable doesn't exist already
    # (raw kobo output doesn't include it, but it may exists in a "clean" version of the 
    # output if ran through the quality check pipeline)
    
if("taxon" %in% colnames(kobo_output)){
       print("the kobo_output data already contained a taxon column, that was used instead of creating a new one")
        
    }else {
    kobo_output<-kobo_output %>% 
      mutate(taxon=(utile.tools::paste(genus, species, subspecies_variety, na.rm=TRUE))) %>%
        # remove white space at the end of the name
        mutate(taxon=str_trim(taxon, "right"))
    } 
  
   ## Add a variable to the metadata stating if the taxon was assessed multiple times or only a single time
  # object with duplicated taxa within a single country
  # duplicated() is run twice, the second time with  fromLast = TRUE so that 
  # the first occurrence is also accounted for, i.e. we can subset all records with the same taxon for a given country
  kobo_output_duplicates <- kobo_output[which(duplicated(kobo_output[c('taxon', 'country_assessment')]) | duplicated(kobo_output[c('taxon', 'country_assessment')], fromLast = TRUE)), ]
  
  # if it is a duplicate then tag it as multi_assessment, if it is not duplicated within the country then single
  kobo_output <- kobo_output %>% 
    mutate(multiassessment= if_else(
      X_uuid %in% kobo_output_duplicates$X_uuid, "multiassessment", "single_assessment"))
  
  ## Process data already including taxon column and multiassessment
  kobo_output <- kobo_output %>% 
  
  # create variable with year in which assessment was done (based on date the form was completed)
  mutate(year_assesment=substr(end,1,4)) %>%
    
    # make sure some variables that seem numbers are actually character,
    # because there may be character and integer values depending on how data was written)
    # for example in IntroductionYear, NeYear and NcYear...
    mutate(across(starts_with("IntroductionYear"), as.character)) %>%
    mutate(across(starts_with("NeYear"), as.character)) %>%
    mutate(across(starts_with("NcYear"), as.character)) %>%
    mutate(across(starts_with("NcRangeDetails"), as.character))
  
  #### 2) Inner functions that would be used to process the files 
  
  ## Inner function to extract maximum numeric value from a string
  max_value_from_string <- function(value) {
    if (is.na(value) || value == "") return(NA)  # Return NA if value is NA or empty
    numbers <- as.numeric(unlist(str_extract_all(value, "\\d+"))) # Extract all numbers
    if (any(is.na(numbers))) return(NA) # Return NA if any conversion error
    return(max(numbers, na.rm = TRUE)) # Return the maximum value
  }

  # Inner function to detect delimiter used in a file
  detect_delimiter <- function(file_path) {
    line <- readLines(file_path, n = 1) # Read the first line of the file
    delimiters <- c(',', '\t', ';') # Possible delimiters
    # Count occurrences of each delimiter
    counts <- sapply(delimiters, function(d) sum(nchar(gsub(paste0('[^', d, ']'), '', line))))
    return(delimiters[which.max(counts)]) # Return the delimiter with the maximum count
  }
  
  # Inner function to convert a file's delimiter to tab
  convert_delimiter <- function(file_path, original_delimiter) {
    tryCatch({
      # Read file with original delimiter and write with tab delimiter
      data <- read_delim(file_path, delim = original_delimiter, guess_max = 1000, quote = "", show_col_types = FALSE)
      write_delim(data, file_path, delim = '\t')
      return(TRUE)
    }, error = function(e) {
      return(FALSE) # Return FALSE if any error
    })
  }
  
  ### 3) Get the attachments files that need processing
  print("###            Processing subdirectories looking for .txt and .csv files with population data     ###")
  
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
          print(paste("Processing file:", file))
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