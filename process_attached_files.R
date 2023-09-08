### Developed by Luis Castillo and Alicia Mastretta-Yanes


# Define a function to process files
process_attached_files <- function(file_path, kobo_output, delim, skip=0){

## What this function does
#  If information of more than 25 populations was be used to collect data for Ne >500 indicator (Section 5 of the Kobo form) 
# it is possible to use a template to upload data instead of using the kobo form. 
# This functions processes the data stored in a one of those files, and formats it as the output of running the function get_indicator1_data.R  


## Arguments:
# target_file = path to a file that needs to be processed 
              # Normaly the path to this file would be the "original_files_dir", the result of get_attached_files.R creating the directory and moving the files there
              # The file MUST include the Xuuid in the file name  (as resulting from running get_attached_files.R)
# kobo_output = a data frame result with the raw (hundreds of columns) Kobo output as downloaded from Kobo 
# delim = delimiter of the file ("," ";", "\t", etc) to be passed to read_delim()
# skip = Number of lines to skip before reading data. To be pased to read_delim(). Default to 0.

## Needed libraries:
#  library(dplyr)
#  library(readr)
#  library(stringr)
#  library(tools)

  ### Read file
  skip=skip
  delim = delim
  temp_df <- read_delim(file_path, delim = delim, col_names = TRUE, show_col_types = FALSE, skip=skip)
  
  ### Get Xuud from file name
  # get the Xuud ie the characters after the first "__" and the second "__".
  # [1,1] is used because we want the character result
  Xuuid_filename<-str_match(file_path, "(?<=__).*?(?=__)")[1,1] 
  
  
  #######                 Separate data in kobo_output as in get_indicator1 function
  
  # create a variable with the full taxon name if this variable doesn't exist already
  # (raw kobo output doesn't include it, but it may exists in a "clean" version of the 
  # output if ran through the quality check pipeline)
  
  if("taxon" %in% colnames(kobo_output)){
    # the kobo_output data already contained a taxon column, that will be used instead of creating a new one
    
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
  
    
  ######                              Process attachment file
  
  # columns with population data that should exist
  required_pop_columns <- c("populationID", "PopulationName", "Origin", "IntroductionYear", 
                            "Ne", "NeLower", "NeUpper", "NeYear", "GeneticMarkers", "GeneticMarkersOther", 
                            "MethodNe", "SourceNe", "NcType", "NcYear", "NcMethod", "NcRange", "NcRangeDetails", 
                            "NcPoint", "NcLower", "NcUpper", "SourceNc", "Comments")
  
    ### 1) Check if all required columns are present, if missing, create them
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
    
    
    ### 2)  Join population data and metadata (the metadata variables in the attachment file will be replaced from the metadata captured in kobo to assure it is correct)
    # Keep only population data columns and add X_uuid columm
    df <- df %>%
      select(all_of(required_pop_columns)) %>%
      mutate(X_uuid = Xuuid_filename)
    
    # Find matches in "kobo_output" to then merge metadata matching rows with population data
    matching_row <- kobo_output %>%
      filter(X_uuid == Xuuid_filename) %>%
      select(country_assessment, taxonomic_group, time_populations, taxon, 
             scientific_authority, name_assessor, email_assessor, kobo_tabular, genus, species, subspecies_variety,
             X_validation_status, X_uuid, year_assesment, GBIF_taxonID, NCBI_taxonID, multiassessment,
             national_taxonID, defined_populations)
    
    # Join kobo_out metadata and file population data
    df<-left_join(df, matching_row, by = "X_uuid")
    
    ## Further clean population data:
    
    ## Make sure numeric columns are numbers
    for(x in c("Ne", "NeLower", "NeUpper", "NcPoint", "NcLower", "NcUpper")){
      if(class(df[[x]])=="character"){
        print(cat("varible", x, "is stored as character and should be numeric, so this function will: \n
                  1) check if there are (), for instance `86 (95% CI)`, and remove them keeping only the value outside ().\n 
                  2) convert ',' to '' IF more than 3 digits followed the ',' (ie we assumed ',' is separating thousands), OR /n
                     convert ',' to '.' IF 2 digits followed the ',' (ie we assumed ',' is separating decimal points) ; and \n
                  3) use as.numeric(). \n
                  You should check the original data to make sure the transformation was correct"))
        ## 1) remove () if they are:
        df[[x]]<-gsub(pattern="\\s*\\([^\\)]+\\)", replacement="", df[[x]])
        
        ## 2) numeric variables appear as character if  "," was used in the original file 
        
        # Count characters after the first comma 
        characters_after_comma <- nchar(sub("^[^,]*,", "", df[[x]]))
        
        # Change "," for "" if characters_after_comma >= 3, else replace with "."
        df[[x]] <- ifelse(characters_after_comma >= 3, sub(",", "", df[[x]]), sub(",", ".", df[[x]]))
        
        ## transform to numeric
        df[[x]]<-as.numeric(df[[x]])
        
      } else {
        df[[x]]<-as.numeric(df[[x]])}
    }
    
      
  ### 3) NcRange and NcType should have only the values specified in the template. Any other value would be changed to NA.
      
      ## NcRange
      excpected_categories<-c("less_5000", "less_5000_bymuch", "more_5000", "more_5000_bymuch", "range_includes_5000", NA)
      
      condition<-df$NcRange %in% excpected_categories
      
      # message and change data
      if(any(!condition)){ # check if there is at least one FALSE
             print("NcRange values could only be 'less_5000', 'less_5000_bymuch', 'more_5000', 'more_5000_bymuch', 'range_includes_5000'. Other values were found and were changed to NA")
        }
     
       df<- df %>%
        mutate(NcRange = ifelse(condition, NcRange, NA))
      
      ## NcType
      excpected_categories<-c("Nc_point", "Nc_range", NA)
      
      condition<-df$NcType %in% excpected_categories
      
      # message and change data
      if(any(!condition)){ # check if there is at least one FALSE
             print("NcType values could only be 'Nc_point', 'Nc_range'. Other values were found and were changed to NA")
         }
      
      df<- df %>%
        mutate(NcType = ifelse(condition, NcType, NA))
      
      
  ### 4) Rename and fill columns that should not be empty, if needed
      
      # Rename populationId and name column to match desired ind1_data names if they exits
      if ("populationID" %in% colnames(df) && "PopulationName" %in% colnames(df)) {
        df <- df %>%
          rename(population = populationID, Name = PopulationName)
      }
      
      
      ## Population ids should be pop1, pop2.... Since people could have written all sort of things, change them all to pop1, pop2, format
      df <- df %>%
        mutate(population = paste0("pop", row_number()))
      
      
      ## Fix problematic conditionals columns
      
      # if NcPoint data was provided then NcType should exist
      condition<-!is.na(df$NcPoint) & is.na(df$NcType)
      ifelse(condition, print("NcPoint data was provided so NcType should exist but was not provided, setting NcType = `Nc_point` for relevant pops"), "")
      df <- df %>%
        mutate(NcType = ifelse(condition, "Nc_point", NcType))
      
      # if NcRange data was provided then NcType should exist 
      condition<-!is.na(df$NcRange) & is.na(df$NcType)
      ifelse(condition, print("NcRange data was provided so NcType should exist but was not provided, setting NcType = `Nc_range` for relevant pops"), "")
      df <- df %>%
        mutate(NcType = ifelse(condition, "Nc_range", NcType))
      
      # If there is no Nc data, NcMethod and NcType should be NA
      condition<-is.na(df$NcRange) & is.na(df$NcRangeDetails) & is.na(df$NcPoint)
      ifelse(condition, print("If there is no Nc data, NcMethod and NcType should be NA, this was not the case so NA were introduced to replace the value. Check original data to make sure it is correct"), "")
      df <- df %>%
        mutate(NcMethod = ifelse(condition, NA , NcMethod),
               NcType = ifelse(condition, NA , NcType))
      
      
      ## change all "" (empty) cells to NA
      df <- df %>%
        mutate_all(list(~na_if(.,"")))
      
      
    
  ### 5) Change columns to desired order  
       desired_order <- c(
        "country_assessment", "taxonomic_group", "taxon", "scientific_authority", 
        "genus", "year_assesment", "name_assessor", "email_assessor", "kobo_tabular", "defined_populations",
        "time_populations", "X_validation_status", "X_uuid", "multiassessment", "population", 
        "Name", "Origin", "IntroductionYear" , "Ne", "NeLower", "NeUpper", 
        "NeYear", "GeneticMarkers", "GeneticMarkersOther", "MethodNe", "SourceNe", 
        "NcType", "NcYear", "NcMethod", "NcRange", "NcRangeDetails", "NcPoint", 
        "NcLower", "NcUpper", "SourceNc", "Comments")
      
      df <- df %>% select(desired_order)
      

      
  ### 6) Return data
      df   
  }
  
 


