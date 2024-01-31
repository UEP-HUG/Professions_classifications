pacman::p_load(
  tidyverse,
  here
)

# Read in datasets ####

## Inclusion ####
### All ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  filter(Origin != "V5_inclusion") |>
  filter(!testeur) |>  # remove data produced by testers - I think Nick already did this
  filter(!str_starts(codbar, "T"))  |>  # Remove any people with codbar beginning with T (also testers)
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### KIDS inclusion ####
inc_kids <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/2024-01-24_KIDS_inclusion_nov21_202401231106.rds") 
inc_kids <- inc_kids |> 
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
### 2021 ####
gh_21 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1217_SC19_general_health_31.05.2021-202309201254_ALL_cleaned_sym.rds") |> 
  left_join(birthdates) |> 
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

### 2022 ####
gh_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1208_SC19_general_health_v1_02_2022-202309201254_ALL.rds") |> 
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

## Health Behavior ####
### 2022 ####
hb_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1126_SC19_health_behaviour_05_2022-202309201254_ALL.rds")|> 
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
    age = time_length(date_soumission - birthdate, "years"),
    profession = case_when(profession == "Techni^cien informatique" ~ "Technicien informatique",.default = profession)
    )

### 2023 ####
st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
  left_join(birthdates) |>
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years"))

# ## Monthly questionnaires ####
# monthly <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2021-05-19-1212_readable_dat_2021.05.19_monthly_V3.rds") |> 
#   mutate(date_soumission = as_date(date_sub)) |> 
#   arrange(date_soumission)
# am <- monthly |> group_by(codbar) |> filter(n()>1) |> 
#   arrange(codbar, date_sub) |> 
#   relocate(date_sub, .after = codbar_foyer)
# 
# length(unique(monthly$codbar))