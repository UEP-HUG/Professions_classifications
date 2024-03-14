# Professions_classifications  
**Objective**: Use job sector and profession data to classify participants according to whether or not they can be considered healthcare workers or essential workers. Additionally include a variable with a "harmonized" job sector classification at each available timepoint (WORK, Santé Travail 2022, Santé Travail 2023)

# Summary  
In order to classify the workers, we used free-text entries for the profession questions that were posed in the Work serosurvey questionnaire of 2020, in the Kids’ inclusion questionnaire (filled out by the parents), and in the Santé-Travail questionnaires of 2022 and 2023. The Inclusion questionnaire has some information about job characteristics, but the response options are a bit limited in scope for this kind of classification (e.g. “Cadre ou profession de formation universitaire” or “Employé-e qualifié-e manuel-le”).

Among participants who completed an inclusion questionnaire and at least one more questionnaire beyond that, for the moment we have 10,938 timepoints of profession entries that come from 7,036 individual participants who responded to at least one of the 4 questionnaires that had free-text profession information. We then fuzzy matched these profession entries to a database of professions that is maintained by the federal government in Switzerland (https://www.i14y.admin.ch/fr/catalog/datasets/HCL_CH_ISCO_19_PROF_1_2/content), and assigned "confidence" ratings according to the strength of the match. High confidence matches were assigned the corresponding ISCO-08 occupation code from the Swiss database, and low confidence matches were manually assigned ISCO-08 codes. 

For the moment 10,587 (96.8%) have an ISCO-08 classification, and this is still an ongoing project to assign codes to the remaining timepoints and also then run through the whole list to clean up inaccurate matches.

Finally, for the essential workers classifications I used a recent ILO paper (Berg et al., 2023) that generally defined “key” (or “frontline”) workers as those working in key sectors (defined from various government lists during the pandemic) who did not have the option of telework. I’ve classified workers as “key” or not using their definitions (matched to the ISCO codes), but we can discuss and develop other definitions to use instead. I also used a WHO classification to define "health workers" based on the ISCO-08 codes (https://www.who.int/publications/m/item/classifying-health-workers).

Berg, J., Ananian, S., Lieppmann, H., Mieres, F., Soares, S., Duman, A., Horne, R., Shroff, T., Sobeck, K., Song, R., & International Labour Organization. Research Department. (2023). World employment and social outlook 2023: The value of essential work (1st ed.). ILO. https://doi.org/10.54394/OQVF7543

**A note:** The final dataset is in a long format, with one row per available timepoint per participant. In case anyone would like to have only one entry per participant, they can filter for the data from their desired questionnaire using the relevant "source", or can use the "date_soumission" variable to filter for the latest or earliest timepoint available for a participant. E.g. to keep only the latest available timepoint:
`dat |> group_by(participant_id) |> slice_max(order_by = date_soumission, n = 1)`

# Data dictionary  
A data dictionary to explain the variables in the final generated file ("...ISCO_fuzzy_recoded_occupations") is included in the Share folder:
P:\ODS\DMCPRU\UEPDATA\Specchio-COVID19\99_data\Base_de_données\classification_jobs_anup_jan_2024

# Walkthrough of code  
The numbered code files run sequentially, but each one "sources" the preceding file, so for example you should be able to directly run the last file ("05_define_essential_healthcare_workers.R") and it will itself source the preceding four files. You should not have to manually move around any datasets, as they're all in the Share folder, and I've put the non-sensitive ones (e.g. with standard ISCO definitions / indices) directly in GitHub, so they should arrive in your local folders when you start the local repository. **The first time you run the code, it will take ~30 mins, because of the fuzzy matching.** Once that fuzzy-matched database is created for the first time, re-running the code should be fairly quick.

You should also **install the `pacman` package** (package management tool) for automatic installing / loading of all packages used in these analyses.

-  01_read_in_datasets.R
    -  Read in relevant WORK, Specchio, and KIDS datasets
-  02_combine_datasets.R
    -  This appends a short identifier to the variable name from each dataset, so that after merging we can easily see the source dataset
    -  This also makes a master dataset by merging the datasets after selecting relevant variables, to create a "dat_master_specchio" file, but for the moment this doesn't get used anywhere else.
-  03_long_participant with profession data.R
    -  In addition to the Inclusion dataset, I've kept only the datasets that have some free-text information on professions:
        -  WORK dataset
        -  Inclusion - KIDS dataset (using the parent1 profession free-text)
        -  Santé-Travail 2022 and 2023 datasets
    -  I merged the files together, keeping the inclusion dataset as the default and then left_joining the others
    -  Created a single master_profession column that fills from the other datasets
        -  Some participants have profession entries from multiple timepoints
-  04a_long_make_fuzzy_classifications.R
    -  I made the text lower-case and removed accents and extra spaces, to make text searches and matches easier
        -  I did the same for the Swiss professions list, which also contains a corresponding ISCO-08 code for each profession
    -  I removed common "stopwords" (e.g. "le", "je", "suis", etc.) from our profession entries as well as from the Swiss professions list
    -  I used the Jaro-Winkler fuzzy matching algorithm to match professions that had identical or near identical matches to the Swiss profession list
    -  For the remaining professions I used the Jaccard fuzzy matching that uses "qgrams" (3-letter combinations of the text) to match to qgrams of the Swiss profession list
    -  I then merged this master_occupations file with an ISCO definitions file (downloaded from the ILO website), to assign the occupation labels at different code levels (at the "full", 3-, 2-, and 1-digit code levels)
-  04b_long_clean_fuzzy_classifications.R --> **This is the main file to work on for improving the ISCO classifications**
    -  This file is for manually classifying professions to the most relevant ISCO-08 codes
-  05_define_essential_healthcare_workers.R
    - This file takes the indices from the Berg et al. (2023) ILO paper, and merges with the ISCO classified file to add columns indicating "key" workers
    - Adds column to also classify health workers from ISCO-08 codes: https://www.who.int/publications/m/item/classifying-health-workers
