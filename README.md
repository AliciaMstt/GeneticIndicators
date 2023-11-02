# Resources for the Multinational evaluation of genetic diversity indicators for the Kunming-Montreal Global Biodiversity Monitoring framework

In 2020, three genetic diversity indicators were proposed and discussed[1-4]:

* **Ne 500 indicator:** the proportion of populations within species with an effective population size (Ne) greater than 500. 

* **PM indicator:** the proportion of maintained populations within species.

* **genetic monitoring  indicator:** number of species in which genetic diversity has been or is being monitored using DNA-based methods

In December 2022, the United Nations Convention of Biological Diversity (CBD) Kunming-Montreal Global Biodiversity Framework (GBF) was adopted by the 196 Parties. The Ne 500 and PM indicators were adopted in the GBF (Annex 1 of CBD/COP/DEC/15/5), which means that parties will be using these indicators to report on their progress over the next decade. 

To facilitate and standardize data collection across different groups and countries, we have created: 

**1)** Guidance documents to prepare the project and answer the form. 

**2)** An online data collection form using [Kobotoolbox](https://www.kobotoolbox.org/),  

**3)** Processing scripts to estimate the indicators based on the Kobo form output. 

**4)** A first multinational evaluation of genetic diversity indicators using the former guidelines, collection tool and informatics pipeline.

In this repository you can find the files, documentation and relevant links of each of the points mentioned above.

#### How to cite

If you use any of the materials listed below, please cite: 

* Hoban, S., da Silva, J. M., Mastretta-Yanes, A., Grueber, C. E., Heuertz, M., Hunter, M. E., Mergeay, J., Paz-Vinas, I., Fukaya, K., Ishihama, F., Jordan, R., Köppä, V., Latorre-Cárdenas, M. C., MacDonald, A. J., Rincon-Parra, V., Sjögren-Gulve, P., Tani, N., Thurfjell, H., & Laikre, L. (2023). **Monitoring status and trends in genetic diversity for the Convention on Biological Diversity: An ongoing assessment of genetic indicators in nine countries**. *Conservation Letters*, 16(3), e12953. [https://doi.org/10.1111/conl.12953](https://doi.org/10.1111/conl.12953)

* Mastretta-Yanes\*, A., da Silva\*, J., Grueber, C. E., ... Laikre, L. & Hoban, S. (2023). **Multinational evaluation of genetic diversity indicators for the Kunming-Montreal Global Biodiversity Monitoring framework**. *EcoEvoRxiv* (Pre-Print). https://ecoevorxiv.org/repository/view/6104/. DOI: [https://doi.org/10.32942/X2WK6T](https://doi.org/10.32942/X2WK6T)



#### Get in touch and more help

If you have a question that is not answered in the guidance documents available below, please post it on the [Genetic Indicators Google Group](https://groups.google.com/g/genetic-indicators-project), and a member will try to answer you as soon as posible.


### 1. Guidance documents:

The following guides were used to provide detailed advice on how to undertake  the genetic monitoring at the country level for a set of species, as well as how to answer each question of the Kobo form.

* **Overall Project Guidance document**  [available as pdf here](./Indicator_testing_project_proposed_detailed_guidance.pdfg). This document includes detailed definitions on key terms (e.g. what is a population), as well as information on how to select a species list and what types of data sources could be used.

* **Genetic Diversity Indicator Testing Kobo Manual**
 [available as pdf as pdf here](./Genetic Diversity Indicator Testing Kobo v4.0 Manual.pdf). This manual provides detailed advice on how to answer each question, with examples if necessary. 


### 2. Online data collection form using Kobotoolbox

Kobotoolbox is a free and open source tool for data collection. It allows to easily develop digital data collection forms that work on both mobile devices and web browsers. Data can be collected from different devices and people, and is accessible through the KoboToolbox interface. This data can then be downloaded into multiple formats for use in applications such as Excel, R, Phyton or GIS software.

#### 2.1 Koboform: 

We built a Kobo form for collecting the needed raw data to estimate de Genetic Diversity Indicators mentioned above, as well as species taxonomic information and assessor's and country information. 

You can see a **dummy example** of how the online form looks once it is deployed in Kobo here: [https://ee.kobotoolbox.org/preview/2KDHEWrb](https://ee.kobotoolbox.org/preview/2KDHEWrb). **Notice that this form is just an example and it can NOT be used to collect real data.** 

If you want to use this form to collect data for your country or desired species, you can contact Alicia Mastretta-Yanes (amastretta@conabio.gob.mx) to get access to the data-collection form where other teams are collecting data. Alternatively **you can deploy your own version of the form** in Kobotoolbox as follows:

1. Download the file [kobo_form.xlsx](https://github.com/AliciaMstt/GeneticIndicators/raw/main/kobo_form.xlsx) from this repository, which is the .xlsx version of the Kobo form.
2. Import it to Kobotoolbox following [these instructions](https://support.kobotoolbox.org/new_form.html).

Check [Kobotoolbox documentation](https://support.kobotoolbox.org/welcome.html) for further details on how to deploy and use it. You can also use our scripts (see below) to process the output in order to estimate the indicators.


#### 2.2. Population information template:

If information of more than 25 populations will be used to collect data for indicator 2 (Section 5 of the Kobo form) it is possible to use the following template to upload data instead of using the kobo form. **This is only encouraged in cases when data is extracted programatically from extant databases,** otherwise we **strongly recommend** using the Kobo form to avoid mistakes. The form allows to manually fill the information of up to 25 populations, but you can add as many populations as needed in a text file following this template.

**Template text version:** [populations\_data_template.txt](populations_data_template.txt). Notice that the first lines starting with `#` are comments to guide you in how the data of each variable (column) should be formatted. You can keep or delete these lines in your data file, but if you keep them do not delete the `#` at te beginning of each line.

**Template Excel version:** [populations\_data_template.xlsx](populations_data_template.xlsx). This is an Excel spreadsheet that contains validation for text variables where controlled vocabularies are available. The tab “Input” is where the data should be entered. Cells that are YELLOW contain validation. For multiple rows, please copy the first data entry row (row 2) so that the validation is applied. The tab “Validation” are the validation options. Do not change these options. The tab “Guide” is a copy of the formating instructions available at [populations\_data_template.txt](populations_data_template.txt).

Regardless of which template you used, save your data as .txt tab delimited file in UTF-8 encoding.

#### 2.3. Downloading kobo data

Once the form has been answered in kobo, you can download the data in .csv or .xlsx or other formats to analyse it in R, Excel or other software.

You can either analyse the data directly, or first run a quality check. 

The R processing scripts and functions described below assume that the data was downloaded from Kobotoolbox using the following settings:

![export_instructions.png](export_instructions.png)

The variables names in the exported data match the "name" column in the   [kobo_form.xlsx](https://github.com/AliciaMstt/GeneticIndicators/raw/main/kobo_form.xlsx) survey tab. In the same file, you can check the question of the form it refers too in the "label" tab. 

### 3. Scripts to process the kobo output and estimate the indicators:


#### 3.1. Functions to extract the data for each indicator from the kobo output:

The following R functions take as input a data frame with the data downloaded from the Kobo form "International Genetic Indicator testing" and **extract and format the data** in order to estimate each of the Genetic Diversity Indicators. 


Functions:
 
* [`get_indicator1_data.R`](get_indicator1_data.R): extracts and formats the  data needed to estimate the Ne 500 indicator (the proportion of populations within species with an effective population size Ne greater than 500) . **In the kobo output, population data is in different columns, this function transforms it so that population data is in rows.** This is needed for downstream analyses. Notice that if the [Population information template](https://github.com/AliciaMstt/GeneticIndicators#population-information-template) was used (species with more than 25 populations) you will need to run an additional step before analysing the data, see below (Getting the population data if the template was used).

* [`get_indicator2_data()`](get_indicator2_data.R): outputs a data frame with the data needed to estimate the PM indicator (the proportion of maintaiened populations within species). 

* [`get_indicator3_data()`](get_indicator3_data.R): outputs a data frame with the data needed to estimate the genetic monitoring indicator (number of species in which genetic diversity has been or is being monitored using DNA-based methods).

* [`get_metadata()`](get_metadata.R): extracts the metadata for taxa and indicators, in some cases creating new useful variables, like taxon name (joining Genus, species, etc) and if the taxon was assessed only a single time or multiple times.

Arguments:

`kobo_output` = a data frame object read into R from the `.csv` file 
resulting from exporting the Kobotoolbox data as explained above.



### 3.2. Getting the population data if the template was used

If information of more than 25 populations was available for the Ne >500 indicator, it was collected in the [template to upload data instead of using the kobo form](https://github.com/AliciaMstt/GeneticIndicators#population-information-template). In that case, the data gets stored a separate attachment per each taxa, which you need to download and merge with the rest of the data.

The url to the attachment file is available in the kobo output data under the variable `pop_tabular_file_URL`. This url could be used to download each of the attachments with the function: 

* [download\_kobo\_attachment()](download_kobo_attachment.R): downloads an attachment of a kobo survey.

 Arguments:
 
 `url` = url to the file to download. For example for pop data downloaded using the template, the url is stored in the column pop_tabular_file_URL of kobo_clean
 
 `local_file_path`  = local path (and file name, including extension) where to save file
 
 `username` = your kobo username, should have permissions to download this data
 
 `password` = your kobo password

Then, the function: 

* [process\_attached\_files()](process_attached_files.R) processes the original attachment files to make them compatible with how population data is expected to estimate the Ne >500 indicator. 

Importantly, it checks the columns that **should be numbers**, but they may have been stored as characters because "," "-" "(" or other characters may have been typed. The function will try to correct this, which **may change the original data**. Please manually check any file flagged as transformed in the log produced by the function to ensure everything is correct.

The output from `process\_attached\_files()` is a data frame that can be joined to the data frame output from `get_indicator1_data()` to estimate the indicator values.

### 3.3. Estimate the Ne 500 indicator

UNDER CONSTRUCTION

transform_to_Ne()

estimate_indicator1()

### 3.4. Estimate the PM indicator

UNDER CONSTRUCTION

estimate_indicator2() 

### 3.5. Estimate the genetic monitoring indicator 

UNDER CONSTRUCTION


### 3.6. Dependencies

Functions were developed and tested using:

* R version 4.2.1 
* utile.tools_0.2.7 
* dplyr_1.0.9 
* tidyr_1.2.0   


## 4. Pipeline used in the multinational assessment 
In the multinational evaluation of genetic diversity indicators (Mastretta-Yanes, da Silva et al. 2023) the raw kobo output form the form "International Genetic Indicator testing" was downloaded from R as described above.

UNDER CONSTRUCTION

## References

1. Frankham, R. Evaluation of proposed genetic goals and targets for the Convention on Biological Diversity. Conserv. Genet. 23, 865–870 (2022) doi:10.1007/s10592-022-01459-1.

2. Fady, B. & Bozzano, M. Effective population size does not make a practical indicator of genetic diversity in forest trees. Biol. Conserv. 253, 108904 (2021).

3. Hoban, S. et al. Effective population size remains a suitable, pragmatic indicator of genetic diversity for all species, including forest trees. Biol. Conserv. 253, 108906 (2021).

4. Laikre, L. et al. Authors’ Reply to Letter to the Editor: Continued improvement to genetic diversity indicator for CBD. Conserv. Genet. 22, 533–536 (2021).
