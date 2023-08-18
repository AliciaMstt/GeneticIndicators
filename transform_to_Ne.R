transform_to_Ne<-function(ind1_data){
## This functions gets the Nc data from point or range estimates and transforms it to Ne 
## multiplying for 0.1 (fixed)
 
## Argumetns
## ind1_data as produced by get_indicator1_data()
  
## output
## Original ind1_data df with two more columns:
# Nc_from_range (conversion of "more than..." to numbers)
# Ne_from_Nc: Ne estimated from NcRange or NcPoint  
# Ne_combined: Ne estimated from Ne if Ne is available, otherwise, from Nc
  
  
  ### Function
  ind1_data = ind1_data
  
  ind1_data<-ind1_data %>% 
    mutate(Nc_from_range = case_when(
          NcRange == "more_5000_bymuch" ~ 5001,
          NcRange == "more_5000" ~ 5001,
          NcRange == "less_5000_bymuch" ~ 100,
          NcRange == "less_5000" ~ 100,
          NcRange == "range_includes_5000" ~ 5001)) %>% 
    
    mutate(Ne_from_Nc = case_when(
                !is.na(NcPoint) ~ NcPoint*0.1,
                !is.na(Nc_from_range) ~ Nc_from_range * 0.1)) %>% 
    
    mutate(Ne_combined = if_else(is.na(Ne), 
                                 Ne_from_Nc,
                                 Ne))
    
  print(ind1_data)
}
