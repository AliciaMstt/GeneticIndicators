# Resources to estimate Genetic Diversity Indicators

In 2020, three genetic diversity indicators were proposed and discussed[1-4]:

* **Indicator 1:** the proportion of populations within species with a genetically effective size, Ne, greater than 500.

* **Indicator 2:** the proportion of populations within species which are maintained.

* **Indicator 3:** the number of species and populations being genetically monitored within a country


To facilitate and standardize data collection across different groups and countries, we have created: 1) guidance documents to prepare the project and answer the form; 2) an online data collection form using [Kobotoolbox](https://www.kobotoolbox.org/),  and 3) processing scripts to estimate the indicators based on the Kobo form output. In this repository you can find the files, documentation and relevant links of each of the points mentioned above.

#### How to cite

If you use any of the materials listed below, please cite [this pre-print](https://www.authorea.com/users/514063/articles/591073-monitoring-status-and-trends-in-genetic-diversity-for-the-convention-on-biological-diversity-an-ongoing-assessment-of-genetic-indicators-in-nine-countries) (this will change to paper reference when it becomes accepted, and we may include some materials at Zenodo once the final version is ready, so that they have their own link):

Sean Hoban, Jess da Silva, Alicia Mastretta-Yanes, Catherine Grueber, Myriam Heuertz, Maggie Hunter, Joachim Mergeay, Ivan Paz-Vinas, Keiichi Fukaya, Fumiko Ishihama, Rebecca Jordan, María Camilla Latorre, 
Anna J. MacDonald, Victor Rincon-Parra, Per Sjögren-Gulve, Naoki Tani, Henrik Thurfjell, Linda Laikre. Monitoring status and trends in genetic diversity for the Convention on Biological Diversity: an ongoing assessment of genetic indicators in nine countries. Authorea. October 20, 2022. DOI: 10.22541/au.166627692.27077414/v1


#### Get in touch and more help

If you have a question that is not answered in the guidance documents available below, please post it on the [Genetic Indicators Google Group](https://groups.google.com/g/genetic-indicators-project), and a member will try to answer you as soon as posible.


### 1. Guidance documents:

The following guides provide detailed advice on how to undertake  the genetic monitoring at the country level for a set of species, as well as how to answer each question of the Kobo form.

* **Overall Project Guidance document**  [available here](https://docs.google.com/document/d/1BAFHnqEA1poTh0XFUx7AKTp31Y-zQW0hSesz00OIO1U/edit?usp=sharing). This document includes detailed definitions on key terms (e.g. what is a population), as well as information on how to select a species list and what types of data sources could be used.

* **Genetic Diversity Indicator Testing Kobo Manual**
 [available here](https://docs.google.com/document/d/12eJ7_aW3s1EgAC3zdFUW46XU1huumUaLONsINJcGCLA/edit?usp=sharing
). This manual provides detailed advice on how to answer each question, with examples if necessary. 


### 2. Koboform: 

Kobotoolbox is a free and open source tool for data collection. It allows to easily develop digital data collection forms that work on both mobile devices and web browsers. Data can be collected from different devices and people, and is accessible through the KoboToolbox interface. This data can then be downloaded into multiple formats for use in applications such as Excel, R, Phyton or GIS software. We built a Kobo form for collecting the needed raw data to estimate de Genetic Diversity Indicators mentioned above, as well as species taxonomic information and assessor's and country information. 

You can see a **dummy example** of how the online form looks once it is deployed in Kobo here: [https://ee.kobotoolbox.org/preview/2KDHEWrb](https://ee.kobotoolbox.org/preview/2KDHEWrb). **Notice that this form is just an example and it can NOT be used to collect real data.** 

If you want to use this form to collect data for your country or desired species, you can contact Alicia Mastretta-Yanes (amastretta@conabio.gob.mx) to get access to the data-collection form where other teams are collecting data. Alternatively **you can deploy your own version of the form** in Kobotoolbox as follows:

1. Download the file [kobo_form.xlsx](https://github.com/AliciaMstt/GeneticIndicators/raw/main/kobo_form.xlsx) from this repository, which is the .xlsx version of the Kobo form.
2. Import it to Kobotoolbox following [these instructions](https://support.kobotoolbox.org/new_form.html).

Check [Kobotoolbox documentation](https://support.kobotoolbox.org/welcome.html) for further details on how to deploy and use it. You can also use our scripts (see below) to process the output in order to estimate the indicators.



#### Population information template:

If information of more than 20 populations will be used to collect data for indicator 2 (Section 5 of the Kobo form) it is possible to use the following template to upload data instead of using the kobo form. **This is only encouraged in cases when data is extracted programatically from extant databases,** otherwise we recommend using the Kobo form to avoid mistakes. The form allows to manually fill the information of up to 100 populations, but you can add as many populations as needed in a text file following this template.

**Template text version:** [populations\_data_template.txt](populations_data_template.txt). Notice that the first lines starting with `#` are comments to guide you in how the data of each variable (column) should be formatted. You can keep or delete these lines in your data file, but if you keep them do not delete the `#` at te beginning of each line.

**Template Excel version:** [populations\_data_template.xlsx](populations_data_template.xlsx). This is an Excel spreadsheet that contains validation for text variables where controlled vocabularies are available. The tab “Input” is where the data should be entered. Cells that are YELLOW contain validation. For multiple rows, please copy the first data entry row (row 2) so that the validation is applied. The tab “Validation” are the validation options. Do not change these options. The tab “Guide” is a copy of the formating instructions available at [populations\_data_template.txt](populations_data_template.txt).

Regardless of which template you used, save your data as .txt tab delimited file in UTF-8 encoding.


### 3. Scripts to process the kobo output and estimate the indicators:

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
