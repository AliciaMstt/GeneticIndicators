### Developed by Luis Castillo


# Define a function to process files
get_attached_data <- function(root_dir, target_dir, kobo_output) {

## What this function does
#  If information of more than 25 populations was be used to collect data for Ne >500 indicator (Section 5 of the Kobo form) 
# it is possible to use a template to upload data instead of using the kobo form. 
# This functions processes the data stored in this files, and formats it as the output of running the function get_indicator1_data.R  

## Arguments:
# root_dir = path to the directory resulting from downloading the Kobo Attachments and unziping it. 
            # Normally this directory is called "attachments" and has many subdirectories with a long alphanumeric name, 
            # corresponding to the Xuuid of the record. 
  
# target_dir = path to the directory where you want to save the output of running this function 
# kobo_output = a data frame result with the raw (hundred of columns) Kobo output as downloaded from Kobo 

## Needed libraries:
#  library(dplyr)
#  library(readr)
#  library(stringr)
#  library(tools)
  
  
  # Define target directory and create it if it doesn't exist
  if(!dir.exists(target_dir)) {
    dir.create(target_dir)
  }
  
  # Inner function to extract maximum numeric value from a string
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
      data <- read_delim(file_path, delim = original_delimiter, guess_max = 1000, quote = "")
      write_delim(data, file_path, delim = '\t')
      return(TRUE)
    }, error = function(e) {
      return(FALSE) # Return FALSE if any error
    })
  }
  
  # Search for matching files and process them
  subfolders <- list.dirs(path = root_dir, recursive = FALSE) # List all subfolders
  output_txt <- file.path(target_dir, 'file_names.txt') # Output file for logging
  output_conn <- file(output_txt, open = "wt") # Open the output file in write mode
  
  # Iterate through subfolders and process files
  for(subfolder in subfolders){
    # List all text and CSV files in the subfolder
    files <- list.files(path = subfolder, pattern = "\\.(txt|csv)$", full.names = TRUE)
    if(length(files) > 0){
      for(file in files){
        # Process each file in the subfolder
        # Extract details like subfolder name, file extension, and new file name
        subfolder_name <- basename(subfolder)
        file_name_without_extension <- subfolder_name
        extension <- tools::file_ext(file)
        new_file_name <- paste0(subfolder_name, ".", extension)
        
        # Look for a match in kobo_output
        matching_row <- kobo_output %>%
          filter(X_uuid == file_name_without_extension)
        
        if (nrow(matching_row) > 0) {
          # If match found, copy the file to target directory and log the match
          file.copy(from = file, to = file.path(target_dir, new_file_name), overwrite = TRUE)
          cat(paste("match +", file_name_without_extension), file = output_conn, sep = "\n")
        } else {
          # If no match found, copy the file to 'No_coincidence' folder and log the non-match
          no_coincidence_folder <- file.path(target_dir, 'No_coincidence')
          dir.create(no_coincidence_folder, showWarnings = FALSE)
          file.copy(from = file, to = file.path(no_coincidence_folder, new_file_name), overwrite = TRUE)
          cat(file_name_without_extension, file = output_conn, sep = "\n")
        }
      }
    }
  }
  
  close(output_conn) # Close the output file connection
  
  # Further process files in the target directory
  result_files <- list.files(path = target_dir, pattern = "\\.(txt|csv)$", full.names = TRUE) # List all text and CSV files
  for(file_path in result_files) {
    # Process each result file
    # Convert delimiter to tab and read the file
    new_file_path <- file_path
    file_name_without_extension <- tools::file_path_sans_ext(basename(new_file_path))
    delimiter <- detect_delimiter(new_file_path)
    convert_delimiter(new_file_path, delimiter)
    df <- read_delim(new_file_path, delim = '\t')
    
    # Extract maximum values for specific columns
    NcYear_max <- sapply(df$NcYear, max_value_from_string)
    NeYear_max <- sapply(df$NeYear, max_value_from_string)
    if(length(NcYear_max) == 0) NcYear_max <- rep(NA, nrow(df))
    if(length(NeYear_max) == 0) NeYear_max <- rep(NA, nrow(df))
    df$NcYear_max <- NcYear_max
    df$NeYear_max <- NeYear_max
    
    # Find matches in "kobo_output" and merge matching rows
    matching_row <- kobo_output %>%
      filter(X_uuid == file_name_without_extension) %>%
      select(country_assessment, taxonomic_group, scientific_authority, name_assessor, email_assessor, kobo_tabular, genus, species, X_validation_status, X_uuid)
    
    if(nrow(matching_row) > 0) {
      df$genus <- matching_row$genus[1]
      df$species <- matching_row$species[1]
      matching_row <- matching_row %>%
        select(-genus, -species)
      df <- bind_cols(matching_row, df)
    }
    
    # Write the modified DataFrame to new file paths (both tab and comma delimited)
    copy_file_path <- sub("\\.txt$", "_copy.csv", new_file_path)
    write_delim(df, new_file_path, delim = '\t')
    write_delim(df, copy_file_path, delim = ',')
  }
}
