#####!!  Caution: this script does not include data cleaning process  !!####
#####!!           please do clean up the data before applying this script !!####
#
# Preliminary R scripts for genetic indicator testing projects
# to analyze relationship between genetic indicators and 
# species characteristics in species selection matrix (SSM)
#
# Generalized linear model (GLM) is used for the analysis
# (other types of models will be provided in other scripts)
#
# I provide three methods to quantify variable importances;
# 1. statistical significance in GLM
# 2. model selection by stepAIC (the variables selected by this process is meaningful to explain the diffirences in indicator values)
# 3. randomization by using package biomod2 (see https://rdrr.io/rforge/biomod2/man/variables_importance.html)
#
# required future improvements;
# need codes to cleanup data especially in SSM
# consider to include interaction terms between explanatory variables in the model 
#
#2023.03.17 
#Fumiko Ishihama; ishihama@nies.go.jp

if(!require(usdm)){install.packages(usdm)} # for calculating VIF to check colinearity among explanatory 
if(!require(MASS)){install.packages(MASS)} # use stepAIC function for model (variable) selection
if(!require(biomod2)){install.packages(biomod2)} # for calculating variable contributions; it may take some time to install

filename_SSM <- "filename_of_SSM.csv"
filename_indicator <- "filename_of_indicator_values.csv"
# be sure to save the files in csv format with encoding=utf8,comma delimited,
# and headers are only in the first line
# default headers of SSM format has too long names;
# I recommend to shorten and to remove all "space" and parentheses from them beforehand
# both files should include a column "Scientific.Name" to identify species

## read the data
SSM <- read.csv(filename_SSM, header=T,stringsAsFactors = T, fileEncoding="utf8")
#　note that all columns with strings are read as "factor"
# because even quantitative characteristics in SSM (e.g. longevity, fecundity)
# include strings in the data (e.g. "less than 0.01"), 
# they are treated as categorical varialbes ignoring the order (i.e."0.01-0.1"<"0.1-1"<"1-10") among categories
# to use these quantitative characteristics as continuous variable, 
# we need conversion such as "0.01-0.1" -> 0.01 (as.numeric).

# check the data
print(head(SSM))
summary(SSM) 
#continuous variables are usually summarized by Min, Median,etc., 
#but now longevity etc. are summarized as categorical variables 
#(shows number of records in each category)  

indicator <- read.csv(filename_indicator, header=T,stringsAsFactors = T, fileEncoding="utf8")
head(indicator)
summary(indicator) #please check if the indicator values are correctly read as continuous variables

# bind indicator and SSM; 
# please confirm both have "Scientific.Name" or other colum to use as a key for binding
data_stat <- merge(indicator, SSM, by="Scientific.Name", all.x=T)

## define variables to use in the GLM analysis
# explanatory variables
var_exp <- c("IUCN.Habitat.Classification", "Regional.National.Red.List.category", "Longevity..years.")  
# please choose from column names of SSM

## check colinearity
# now I comment out the codes because now we have only categorical variables and they do not word,
# but I prepare it for future use with continuous variables
# cor(data_stat[,var_exp]) #should be less than 0.9
# vif(data_stat[,var_exp]) #should be less than 10

## define the dependent variable
# please give the column name for the indicator value in "data_stat"
var_dep <- "indicator_column_name"

na.omit(data_stat[,c(var_dep,var_exp)]) -> data_stat_naomit
#remove records with NAs for the variables to use in glm

## define fomula to use in GLM (now I don't use interaction terms)
formula.glm <- paste(var_dep, paste(var_exp, collapse="+"), sep="~")
print(formula.glm)

## GLM analysis
glm_indicator_SSM <- glm(formula=formula.glm, data=data_stat_naomit)

summary(glm_indicator_SSM)
# you can check significance of each explanatory variables


## model (varible) selection 
# instead of statistical significance, 
# you can analyse meaningfulness of each variable 
# to explain the differences in the indicator values 
# by selecting variables to include in the model based on AIC (Akaike Information Criteria)

glm_indicator_SSM_step <- stepAIC(glm_indicator_SSM)


## quantify variable importances by randomizing data
## by using a function in biomod2 pacakge (a package for ensemble modeling)
## it may require some time for iterating randomization

var_imp_glm_step <- variables_importance(model=glm_indicator_SSM_step, data=data_stat_naomit, method="full_rand", nb_rand=10)
var_imp_mean <- apply(var_imp_glm_step[[1]],1,mean)
barplot(var_imp_mean)



