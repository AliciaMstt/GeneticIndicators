# README

In 2020, three genetic diversity indicators were proposed and discussed[1-4]:

* **Indicator 1:** the proportion of populations within species with a genetically effective size, Ne, greater than 500.

* **Indicator 2:** the proportion of populations within species which are maintained.

* **Indicator 3:** the number of species and populations being genetically monitored within a country


To facilitate and standardize data collection across different groups and countries, we have created an online data collection form using [Kobotoolbox](https://www.kobotoolbox.org/), a guidance document and processing scripts to estimate the indicators based on the Kobo form output. These are available here:

### Koboform: 

Kobotoolbox is a free and open source tool for data collection. It allows to easily develop digital data collection forms that work on both mobile devices and web browsers. Data can be collected from different devices and people, and is accessible through the KoboToolbox interface. This data can then be downloaded into multiple formats for use in applications such as Excel, R, Phyton or GIS software.

The file [kobo_form.xlsx](https://github.com/AliciaMstt/GeneticIndicators/raw/main/kobo_form.xlsx) is the .xlsx version of the Kobo form we built for collecting the needed raw data to estimate de Genetic Diversity Indicators mentioned above, as well as species taxonomic information and assesor's and country ifnromation. 

If you want to use this form to collect data for your country or desired species, the form can be deployed in Kobotoolbox as follows:

1. Download the file [kobo_form.xlsx](https://github.com/AliciaMstt/GeneticIndicators/raw/main/kobo_form.xlsx) from this repository.
2. Import it to Kobotoolbox following [these instructions](https://support.kobotoolbox.org/new_form.html).

Check [Kobotoolbox documentation](https://support.kobotoolbox.org/welcome.html) for further details on how to deploy and use it. You can also use our scripts (see below) to process the output in order to estimate the indicators.

You can see a **dummy example** of how the online form looks once it is deployed in Kobo here: XXXXXX. **Notice that this form is just an example and it should NOT be used to collect real data** 


### Population information template:

If information of more than 20 populations will be used to collect data for indicator 2 (Section 5 of the Kobo form) it is possible to use the following template to upload data instead of using the kobo form. **This is only encouraged in cases when data is extracted programatically from extant databases,** otherwise we recommend using the Kobo form to avoid mistakes. The form allows to manually fill the information of up to 100 populations, but you can add as many populations as needed in a text file following this template.

**Template:** [populations_data_template.txt](populations_data_template.txt). Notice that the first lines starting with `#` are comments to guide you in how the data of each variable (column) should be formatted. You can keep or delete these lines in your data file, but if you keep them do not delete the `#` at te beginning of each line.



### Guidance document:



### Scripts to process the kobo output and estimate the indicators:

#### Extract the data for each indicator from the kobo output:

The following R functions take as input the output of the Kobo form "International Genetic Indicator testing" and reformat it in order to have the data in a data frame useful for estimating each of the Genetic Diversity Indicators.
 
* [`get_indicator1_data.R`](get_indicator1_data.R): outputs a data frame with the data needed to estimate indicator 1. 

* [`get_indicator2_data.R`](get_indicator2_data.R): outputs a data frame with the data needed to estimate indicator 2. 


##### Usage:

* Input for the 3 functions is the same `.csv` file resulting from exporting the Kobotoolbox data from the form "International Genetic Indicator testing" with the following settings:

![export_instructions.png](export_instructions.png)

* Arguments:

`file` = path to the .csv file with kobo output. 

* Example:

```
# Needed libraries
library(tidyr)
library(dplyr)
library(utile.tools)


# load functions
source("get_indicator1_data.R")
source("get_indicator2_data.R")

# Get data for each indicator:
ind1_data<-get_indicator1_data(file="kobo_output.csv")
ind2_data<-get_indicator2_data(file="kobo_output.csv")

```

##### Dependencies:

Functions were developed and tested using:

* R version 4.2.1 
* utile.tools_0.2.7 
* dplyr_1.0.9 
* tidyr_1.2.0   


## References

1. Frankham, R. Evaluation of proposed genetic goals and targets for the Convention on Biological Diversity. Conserv. Genet. 23, 865–870 (2022) doi:10.1007/s10592-022-01459-1.

2. Fady, B. & Bozzano, M. Effective population size does not make a practical indicator of genetic diversity in forest trees. Biol. Conserv. 253, 108904 (2021).

3. Hoban, S. et al. Effective population size remains a suitable, pragmatic indicator of genetic diversity for all species, including forest trees. Biol. Conserv. 253, 108906 (2021).

4. Laikre, L. et al. Authors’ Reply to Letter to the Editor: Continued improvement to genetic diversity indicator for CBD. Conserv. Genet. 22, 533–536 (2021).
