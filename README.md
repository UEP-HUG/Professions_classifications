  # Professions_classifications
**Objective**: Use job sector and profession data to classify participants according to whether or not they can be considered healthcare workers or essential workers


# Walkthrough of code
The numbered code files run sequentially, but each one "sources" the preceding file, so for example you should be able to directly run the last file ("05_define_essential_healthcare_workers.R") and it will itself source the preceding four files. You should not have to manually move around any datasets, as they're all in the Share folder, and I've put the non-sensitive ones (e.g. with standard ISCO definitions / indices) directly in GitHub, so they should arrive in your local folders when you start the local repository.

You should also **install the `pacman` package** (package management tool) for automatic installing / loading of all packages used in these analyses.

-  01_read_in_datasets.R
    -  Read in relevant WORK, Specchio, and KIDS datasets
    -  For the Inclusion dataset, I applied the following variable to filter the dataset for participants we actually use in the studies:
        - serocov_pop | pop_pilote | serocov_schools | serocov_kids | serocov_work | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022
-  02_combine_datasets.R
    -  This appends a short identifier to the variable name from each dataset, so that after merging we can easily see the source dataset
    -  This also makes a master dataset by merging the datasets after selecting relevant variables, to create a "dat_master_specchio" file, but for the moment this doesn't get used anywhere else.
-  03_participant with profession data.R
    -  In addition to the Inclusion dataset, I've kept only the datasets that have some free-text information on professions:
        -  WORK dataset
        -  Inclusion - KIDS dataset (using the parent1 profession free-text)
        -  Santé-Travail 2022 and 2023 datasets
    -  I merged the files together, keeping the inclusion dataset as the default and then left_joining the others
    -  I then filtered the dataset to include only participants who submitted responses to at least one of the four questionnaires above
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