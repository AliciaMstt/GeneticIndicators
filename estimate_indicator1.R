estimate_indicator1<- function(ind1_data){
### This functions estimates the Ne 500 indicator, ie for each assessment of a taxon it calculates the proportion of populations within it which are above Ne 500. Notice it uses the assessment id `X_uuid` (unique record of a taxon), because a single taxon could be assessed by different countries
##  or more than once with different parameters. The output is a new dataframe with a row per assessment, metadata, new columns used to estimate the indicator (number of pops) and the indicator value.
  
## Arguments
## ind1_data: population size data as produced by get_indicator1_data() and after running transform_to_Ne()
  

### Function
  
# Estimate indicator 1 by X_uuid (unique record of a taxon, because a single taxon could be assessed by different countries
# or more than once with different parameters)  
indicator1<-ind1_data %>%
  group_by(X_uuid, ) %>%
  summarise(n_pops=n(),
            n_pops_Ne_data=sum(!is.na(Ne_combined)),
            n_pops_more_500=sum(Ne_combined>500, na.rm=TRUE),
            indicator1=n_pops_more_500/n_pops_Ne_data) %>%

# join with metadata
left_join(metadata)  

print(indicator1)
}