transform_to_Ne<-function(ind1_data, ratio=0.1){
## This functions gets the Nc data from point or range estimates and transforms it to Ne 
## multiplying for a ratio Nc:Ne (defaults to 0.1 if none provided)
 
## Argumetns
## ind1_data as produced by get_indicator1_data()
## desired Nc:Ne ratio. Should range 0-1. Defaults to 0.1
  
## output
## Original ind1_data df with two more columns:
# Nc_from_range (conversion of "more than..." to numbers)
# Ne_from_Nc: Ne estimated from NcRange or NcPoint  
# Ne_combined: Ne estimated from Ne if Ne is available, otherwise, from Nc
  
  
  ### Function
  
## check ratio: 
ratio=ratio

if (!is.numeric(ratio) || ratio < 0 || ratio > 1) {
  stop("Invalid argument. Please provide a number within the range 0 to 1, using `.` to delimit decimals.")
} else {
      
    ## process data:
      ind1_data = ind1_data
      
      ind1_data<-ind1_data %>% 
        mutate(Nc_from_range = case_when(
              NcRange == "more_5000_bymuch" ~ 10000,
              NcRange == "more_5000" ~ 5050,
              NcRange == "less_5000_bymuch" ~ 500,
              NcRange == "less_5000" ~ 4050,
              NcRange == "range_includes_5000" ~ 5001)) %>% 
        
        mutate(Ne_from_Nc = case_when(
                    !is.na(NcPoint) ~ NcPoint*ratio,
                    !is.na(Nc_from_range) ~ Nc_from_range * ratio)) %>% 
        
        mutate(Ne_combined = if_else(is.na(Ne), 
                                     Ne_from_Nc,
                                     Ne))
        
      print(ind1_data)
    }
}
