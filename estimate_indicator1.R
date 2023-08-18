estimate_indicator1<- function(ind1_data){
## Arguments
## ind1_data: raw data as produced by get_indicator1_data() and after running transform_to_Ne()
  

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