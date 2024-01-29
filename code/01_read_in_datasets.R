pacman::p_load(
  tidyverse,
  here
)

# Read in datasets ####

## Inclusion ####
### Parents ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  filter(Origin != "V5_inclusion") |>
  filter(!testeur) |>  # remove data produced by testers - I think Nick already did this
  filter(!str_starts(codbar, "T"))  |>  # Remove any people with codbar beginning with T (also testers)
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### KIDS inclusion ####
incl_kids <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/2024-01-24_KIDS_inclusion_nov21_202401231106.rds") 
incl_kids <- incl_kids |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### KIDS - Parents' inclusion ####
incl_parents <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/2023-11-20_Parents_inclusion_202310240901.rds")|> 
  mutate(
    parent1_date_soumission = as_date(parent1_date_soumission),
    parent1_age = time_length(parent1_date_soumission - parent1_birthdate, "years"))


### Birthdates variable to merge with others ####
birthdates <- inclusion |> select(codbar, birthdate)

## General Health ####
### 2022 ####
gh_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-02-27-1537_Export_general_health-202206301210_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2023 ####
gh_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1151_general_health_v2_03_2023-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Sommeil ####
### 2023 ####
sommeil_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1120_SC_sleep_health_06_23-202309201254_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Health Behavior: 2022 ####
hb_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2022-08-29-1248_export_comportement_santé-202206301206_ALL.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

## Santé Travail ####
### 2022 ####
st_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1145_SanteTravail_ALLparticipants.rds")|> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2023 (TBD) ####
st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
  left_join(birthdates) |>
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))