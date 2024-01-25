pacman::p_load(
  tidyverse,
  here
)

# Read in datasets ####

## Parents inclusion ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## General Health ####
### 2022 ####
gh_22 = readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-02-27-1537_Export_general_health-202206301210_ALL.rds")

### 2023 ####
gh_23 = readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1151_general_health_v2_03_2023-202309201254_ALL.rds")

## Santé Travail ####
### 2022 ####
st_22 = readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1145_SanteTravail_ALLparticipants.rds")

### 2023 ####
# st_23 = readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/.rds") # TBD