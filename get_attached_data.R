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
  print("###           FIRST STEP: processing subdirectories looking for .txt and .csv files with population data")
  
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
        new_file_name <- paste0(subfolder_name, ".", extension)
        
        # Look for a match in kobo_output
        matching_row <- kobo_output %>%
          filter(X_uuid == file_name_without_extension)
        
        if (nrow(matching_row) > 0) {
          # If match found, copy the file to target directory and log the match
          print(paste("Processing file:", file))
          print(paste("the uuid of the file was found in the kobo metadata and the file was copied to", target_dir))
          file.copy(from = file, to = file.path(target_dir, new_file_name), overwrite = TRUE)
          cat(paste("match +", file_name_without_extension), file = output_conn, sep = "\n")
          
        } else {
          # If no match was found, copy the file to 'No_coincidence' folder and log the non-match
          print(paste("Processing file:", file))
          print(paste0("the uuid of the file was NOT found in the kobo metadata and the file was copied to ", target_dir, '/No_coincidence', ". This likely means that the species assessment was corrected in kobo and the system does not delete the old file. But check if you are unsure"))
          no_coincidence_folder <- file.path(target_dir, 'No_coincidence')
          dir.create(no_coincidence_folder, showWarnings = FALSE)
          file.copy(from = file, to = file.path(no_coincidence_folder, new_file_name), overwrite = TRUE)
          cat(file_name_without_extension, file = output_conn, sep = "\n")
        }
      }
    }
  }
  
  close(output_conn) # Close the output file connection
  
  ### 4) Further process files in the target directory to make output look like the output of get_indicator1_data()
  print("###           SECOND STEP:: processing text files to check their format is correct")
  
  # columns with population data that should exist
  required_pop_columns <- c("populationID", "PopulationName", "Origin", "IntroductionYear", 
                        "Ne", "NeLower", "NeUpper", "NeYear", "GeneticMarkers", "GeneticMarkersOther", 
                        "MethodNe", "SourceNe", "NcType", "NcYear", "NcMethod", "NcRange", "NcRangeDetails", 
                        "NcPoint", "NcLower", "NcUpper", "SourceNc", "Comments")
  
  # get files in the target directory
  attached_df <- data.frame()
  result_files <- list.files(path = target_dir, pattern = "\\.(txt|csv)$", full.names = TRUE) # List all text and CSV files
  
  for(file_path in result_files) {
    # Process each result file
    # Convert delimiter to tab and read the file
    print(paste("Checking  file:", file_path))
    # Detect delimiter and read the file
    delimiter <- detect_delimiter(file_path)
    convert_delimiter(file_path, delimiter)
    temp_df <- read_delim(file_path, delim = '\t', col_names = TRUE, show_col_types = FALSE)
    
    ### 4.1) Check if all required columns are present, if missing, create them
    if(all(required_pop_columns %in% names(temp_df))) {
      print("all requiered population columns are present in the file :)")
      # create working df
      df <- temp_df 
      
         } else {
            ## tell the user and create missing columns as empty
            # create workig df
            df <- temp_df
              
            # check name of missing columns
             missing_columns<-required_pop_columns[!(required_pop_columns %in% names(df))]
             
             # tell the user which columns are missing 
             print(paste("the following column is missing in the file and it will be created as an empity variable:",  missing_columns))
         
             # create empty variable for each missing column
             # Loop through the missing_variables vector and add each column
             for (col_name in missing_columns) {
               df <- df %>%
                 mutate(!!col_name := NA)  # !! operator is used to "unquote" and interpret the col_name as a column name
             }
         }
    
    
    ### 4.2)  Join population data and metadata (the file metadata will be replaced from the metadata captured in kobo to assure it is correct)
    # Keep only population data columns and add X_uuid columm
    df <- df %>%
      select(all_of(required_pop_columns)) %>%
      mutate(X_uuid = file_name_without_extension)
    
    # Find matches in "kobo_output" to then merge metadata matching rows with population data
    matching_row <- kobo_output %>%
      filter(X_uuid == file_name_without_extension) %>%
      select(country_assessment, taxonomic_group, time_populations, taxon, multiassessment, 
             scientific_authority, name_assessor, email_assessor, kobo_tabular, genus, species, subspecies_variety,
             X_validation_status, X_uuid, year_assesment, GBIF_taxonID, NCBI_taxonID, 
             national_taxonID)
    
    # Join kobo_out metadata and file population data
    df<-left_join(df, matching_row, by = "X_uuid")
    
    ## Further clean population data
    ## Make sure numeric columns are numbers
    for(x in c("Ne", "NeLower", "NeUpper", "NcPoint", "NcLower", "NcUpper")){
      if(class(df[[x]])=="character"){
        print(paste("column", x, "is stored as character, so we will convert the decimal separator from ',' to '.' and then use as.numeric()"))
        ## numeric variables appear as character if the decimal separator "," was used in the original file instead of "."
        # change "," for "."
        df[[x]]<-gsub(pattern=",", replacement=".", df[[x]])
        # transform to numeric
        df[[x]]<-as.numeric(df[[x]])
      } else {
        df[[x]]<-as.numeric(df[[x]])}
    }
    
    ### 4.3) Rename and fill columns that should not be empty, if needed
    
    # Rename populationId and name column to match desired ind1_data names if they exits
    if ("populationID" %in% colnames(df) && "PopulationName" %in% colnames(df)) {
      df <- df %>%
        rename(population = populationID, Name = PopulationName)
         }
   
      # if population id was not filled, add it.
      df <- df %>%
      mutate(population = ifelse(is.na(population), paste0("pop", row_number()), population)) %>%
      
      # if NcPoint data was provided then NcType should exist
      mutate(NcType = ifelse(is.na(NcPoint), NcType, "Nc_point")) %>%
    
      # if NcRange data was provided then NcType should exist 
      mutate(NcType = ifelse(is.na(NcRange), NcRange, "Nc_range"))
    
    
  
  # # Alicia: With the changes I made above (processing kobofile), I think this is not needed anymore        
  #     if(nrow(matching_row) > 0) { # what is this for?
  #       year_from_kobo <- substr(matching_row$end[1], 1, 4)
  #       df$genus <- matching_row$genus[1]
  #       df$species <- matching_row$species[1]
  #       matching_row <- matching_row %>%
  #         select(-genus, -species, -end)  
  #       df <- bind_cols(matching_row, df)
  #     } else {
  #       year_from_kobo <- 2022  ## What is this for??
  #     }
  #     
  #     # Check if required population columns exist, if one doesn't exist, create it empty.
  #     df <- df %>%
  #       rename(population = populationID, Name = PopulationName) %>% 
  # #     mutate(year_assesment = year_from_kobo) %>% # this is not needed becasue it is extracted form kobo_out
  #       mutate(across(starts_with("IntroductionYear"), as.character)) %>%
  #       mutate(across(starts_with("NeYear"), as.numeric)) %>%
  #       mutate(across(starts_with("NcYear"), as.numeric)) %>%
  #       mutate(across(starts_with("NcRangeDetails"), as.numeric))
  #     
   
  ### 4.4) Change columns to desired order  
       desired_order <- c(
        "country_assessment", "taxonomic_group", "taxon", "scientific_authority", 
        "genus", "year_assesment", "name_assessor", "email_assessor", "kobo_tabular", 
        "time_populations", "X_validation_status", "X_uuid", "multiassessment", "population", 
        "Name", "Origin", "IntroductionYear", "Ne", "NeLower", "NeUpper", 
        "NeYear", "GeneticMarkers", "GeneticMarkersOther", "MethodNe", "SourceNe", 
        "NcType", "NcYear", "NcMethod", "NcRange", "NcRangeDetails", "NcPoint", 
        "NcLower", "NcUpper", "SourceNc", "Comments")
      
      df <- df %>% select(desired_order)
      

      
  ### 5) Save data
      
      # Write the modified DataFrame to new file paths (both tab and comma delimited)
      copy_file_path <- sub("\\.txt$", "_copy.csv", file_path) 
      write_delim(df, file_path, delim = '\t')
      write_delim(df, copy_file_path, delim = ',')
    
      # add to object df to return at end of loop  
      attached_df <- rbind(attached_df, df)
      
  }
  
  return(attached_df)
}

