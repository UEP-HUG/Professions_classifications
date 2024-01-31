pacman::p_load(
  tidyverse,
  data.table,
  here,
  purrr)

# Save the usual working directory
a <- getwd()

# Data dictionaries
## Set the the data dictionaries folder as the WD
setwd("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Internal_sharing_files/sugar_mapping_dictionaries_from_Julien/")
# setwd("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/")

b <- fread("ql_dict_SCS_kids_inclusion_04.02.21_20221027.csv")

## Load in all of the csv files
temp = list.files(pattern="\\.csv$")
myfiles = lapply(temp, fread)

## Load in all of the rds files
# temp = list.files(pattern="\\.rds$")
# myfiles = lapply(temp, readRDS)

# d1 <- fread("ql_dict_SC19_health_behaviour_05_2022_20230922.csv")
varlist <- c("secteur")
tlist = tibble("outcome" = NA, "tempid" = 1:length(myfiles)) # empty dataframe
# fill the dataframe with list elements TRUE/FALSE for the string searched below (can use | separator for OR terms)
for (i in 1:length(myfiles)) {
  tlist$outcome[i] <- any(myfiles[[i]] %like% "sector")
}
# Print the names of the files that have the searched string
tlist <- tlist %>% filter(outcome);fhfh <- tlist$tempid;temp[fhfh]

## Reset to original working directory
# setwd(a)