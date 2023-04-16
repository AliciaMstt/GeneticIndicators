###
### This R function takes as input the output of the Kobo form "International Genetic Indicator testing" 
### and reformat its in order to have the data in a dataframe useful for estimating 
### Genetic Diversity Indicator 1 (the proportion of populations within species with 
###                                a genetically effective size, Ne, greater than 500.)
### 

### If you use this script, please cite XXXXXXXX

get_indicator1_data<-function(file=file){
  ###
  ### Arguments:
  ###
  
  # file = path to the .csv file with kobo output. This input data should had been exported 
  # from Kobotoolbox and saved as .csv
  # as detailed in the README at: XXX UPDATE URL TO FINAL GITHUB REPO HERE".
  
  ### Needed libraries:  
  
  #  library(tidyr)
  #  library(dplyr)
  #  library(utile.tools)
  
  ###
  ### Function  
  ### 
  
  ### Read data
  kobo_output<-read.csv(file, sep=";", header=TRUE)
  
  ### Separate data 
  
  indicator1_data<-kobo_output %>%
    # create a variable with the full taxon name
    mutate(taxon=(utile.tools::paste(genus, species, subspecies_variety, na.rm=TRUE))) %>%
    
    # create variable with year in which assessment was done (based on date the form was completed)
    mutate(year_assesment=substr(end,1,4)) %>%
    
    # make sure IntroductionYear, NeYear and NcYear are character (there may be character and integer values depending on how data was written)
    mutate(across(starts_with("IntroductionYear"), as.character)) %>%
    mutate(across(starts_with("NeYear"), as.character)) %>%
    mutate(across(starts_with("NcYear"), as.character)) %>%
    
    ## select relevant columns 
    # taxon and assessment info
    dplyr::select(country_assessment, taxonomic_group, taxon, scientific_authority,
                  genus, taxon, year_assesment, name_assessor, email_assessor,
                  # indicator 1 data               
                  time_populations, Name_pop1:Comments_pop25,
                  
                  # kobo validation status
                  X_validation_status) %>% 
                  
  ### Get population data as single variables               
    pivot_longer(cols = matches("_pop[0-9]"),
                 names_to=c(".value", "population"),
                 names_sep = "_",
                 values_drop_na = TRUE)

  
  # End of function
}

