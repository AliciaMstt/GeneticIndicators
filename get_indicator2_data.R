###
### This R function takes as input the output of the Kobo form "International Genetic Indicator testing" 
### and reformat its in order to have the data in a dataframe useful for estimating 
### Genetic Diversity Indicator 2 (the proportion of populations within species which are maintained)
### 

### If you use this script, please cite XXXXXXXX

###
### Libraries
###

get_indicator2_data<-function(file=file){

### Arguments:

# file = path to the .csv file with kobo output. This input data should had been exported 
# from Kobotoolbox and saved as .csv
# as detailed in the README at: XXX UPDATE URL TO FINAL GITHUB REPO HERE".
  
### Needed libraries:  
  
#  library(tidyr)
#  library(dplyr)
#  library(utile.tools)

### Function  
# Read data
kobo_output<-read.csv(file, sep=";", header=TRUE)

####
#### Separate data 
####

indicator2_data<-kobo_output %>%
                 # create a variable with the full taxon name
                 mutate(taxon=(utile.tools::paste(genus, species, subspecies_variety, na.rm=TRUE))) %>%
  
                 # create variable with year in which assessment was done (based on date the form was compleated)
                 mutate(year_assesment=substr(end,1,4)) %>%
  
                 ## select relevant columns 
                 # taxon and assessment info
                 dplyr::select(country_assessment, taxonomic_group, taxon, scientific_authority,
                               genus, taxon, year_assesment, name_assessor, email_assessor,
                 # indicator 2 data               
                               n_extant_populations, n_extint_populations, other_populations, time_populations) %>%
                 # change -999 to Na
                         mutate(n_extint_populations=na_if(n_extint_populations, -999), 
                                n_extant_populations=na_if(n_extant_populations, -999))
                # print data
                print(indicator2_data)
                
                # End of function
                           }
                


