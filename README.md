# Professions_classifications  
**Objective**: Use job sector and profession data to classify participants according to whether or not they can be considered healthcare workers or essential workers

# Summary  
In order to classify the workers, I used free-text entries for the profession questions that were posed in the Work serosurvey questionnaire of 2020, in the Kids’ inclusion questionnaire (filled out by the parents), and in the Santé-Travail questionnaires of 2022 and 2023. The Inclusion questionnaire has some information about job characteristics, but the response options are a bit limited in scope for this kind of classification (e.g. “Cadre ou profession de formation universitaire” or “Employé-e qualifié-e manuel-le”).
Among participants who completed an inclusion questionnaire, for the moment we have 7,782 participants who responded to at least one of the 4 questionnaires that had free-text profession information. I then manually assigned ISCO occupation codes (using a script adapted from the classifications I made for the burnout article), and for the moment there are 6,376 participants classified with an ISCO code. While these classifications are mostly accurate, there are still many incorrect ones, and I think that getting a clean assignment of ISCO codes is really the main challenge of this project.

Finally, for the essential workers classifications I used a recent ILO paper (Berg et al., 2023) that generally defined “key” (or “frontline”) workers as those working in key sectors (defined from various government lists during the pandemic) who did not have the option of telework. I’ve classified workers as “key” or not using their definitions (matched to the ISCO codes), but we can discuss and develop other definitions to use instead.

**A note:** in cases where people responded to more than one questionnaire, I used the earliest profession information we had for the classification, as for the moment I assumed that we wanted the profession closest to the first lockdown period. But, we can quite flexibly change this logic or could make a separate classification based on each timepoint.

# Data dictionary  
I've also included a data dictionary in the output folder, to explain the variables in the final generated file ("...ISCO_recoded_essential_plus_health_workers")

Berg, J., Ananian, S., Lieppmann, H., Mieres, F., Soares, S., Duman, A., Horne, R., Shroff, T., Sobeck, K., Song, R., & International Labour Organization. Research Department. (2023). World employment and social outlook 2023: The value of essential work (1st ed.). ILO. https://doi.org/10.54394/OQVF7543

I put the preliminary datasets with occupation information into this folder in the Share (in rds and csv formats):
P:\ODS\DMCPRU\UEPDATA\Specchio-COVID19\99_data\Base_de_données\classification_jobs_anup_jan_2024

# Walkthrough of code  
The numbered code files run sequentially, but each one "sources" the preceding file, so for example you should be able to directly run the last file ("05_define_essential_healthcare_workers.R") and it will itself source the preceding four files. You should not have to manually move around any datasets, as they're all in the Share folder, and I've put the non-sensitive ones (e.g. with standard ISCO definitions / indices) directly in GitHub, so they should arrive in your local folders when you start the local repository.

You should also **install the `pacman` package** (package management tool) for automatic installing / loading of all packages used in these analyses.

-  01_read_in_datasets.R
    -  Read in relevant WORK, Specchio, and KIDS datasets
-  02_combine_datasets.R
    -  This appends a short identifier to the variable name from each dataset, so that after merging we can easily see the source dataset
    -  This also makes a master dataset by merging the datasets after selecting relevant variables, to create a "dat_master_specchio" file, but for the moment this doesn't get used anywhere else.
-  03_participant with profession data.R
    -  In addition to the Inclusion dataset, I've kept only the datasets that have some free-text information on professions:
        -  WORK dataset
        -  Inclusion - KIDS dataset (using the parent1 profession free-text)
        -  Santé-Travail 2022 and 2023 datasets
    -  I merged the files together, keeping the inclusion dataset as the default and then left_joining the others
    -  Created a single profession column that fills from the other datasets
        -  First I created an empty master_profession column, and then filled it up progressively from the datasets, going forward in time
            - For example, I first filled from the WORK dataset, but if there was no profession entry then I filled from the inclusion dataset, then from the KIDS dataset, etc.
-  04_classify_occupations.R --> **This is the main file to work on for improving the ISCO classifications**
    -  I made the text lower-case and removed accents, to make text searches and matches easier
    -  I created an empty ISCO variable, to be filled by each line of definitions, using the master_profession column, and a mix of a few others
        -  This largely relies on str_detect() to find matches on lines that don't yet have an ISCO code defined
        -  A downside of this approach is that the order of the definitions impacts the final output (e.g. if str_detect(variable, "assistant") is used to define Administrative Assistants, then a subsequent use of str_detect(variable, "assistant medicale") will miss Medical Assistants, as they will already have been coded into "Administrative Assistants"). We can get around these kinds of things by adding in a few other criteria for the definitions, but it's important to be mindful of this issue.
    -  I then merged this master_occupations file with an ISCO definitions file (downloaded from the ILO website), to assign the occupation labels at different code levels (e.g. at the 3-, 2-, and 1-digit codes)
-  05_define_essential_healthcare_workers.R
    - This file takes the indices from the Berg et al. (2023) ILO paper, and merges with the ISCO classified file to add columns indicating "key" workers as well as "healthcare" workers
