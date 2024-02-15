pacman::p_load(
  tidyverse,
  here
  )

# Read in datasets ####

## Inclusion ####
### All ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  # Remove the V5_inclusion people, as these are bus_santé people and there are some duplicates
  filter(Origin != "V5_inclusion") |>
  # Filter for only participants in one of the relevant studies
  # filter(serocov_pop | pop_pilote | serocov_schools | serocov_kids | serocov_work | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |> 
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
    age = time_length(date_soumission - birthdate, "years")) |> 
  group_by(participant_id) |> filter(n() < 2) |> ungroup() # remove duplicate entries

sector_st_23 <- st_23 |> select(participant_id, starts_with("job_sector")) |> 
  select(-job_sector_other_text) |>
  mutate(
    across(
      # job_sector_commerce:job_sector_99,
      starts_with("job_sector"),
      ~na_if(., FALSE)
    ))
wc <- droplevels(col(sector_st_23, as.factor=TRUE)[which(sector_st_23 == "TRUE")])
sector_st_23[levels(wc)] <- Map(factor, sector_st_23[levels(wc)], labels = levels(wc))
sector_st_23 <- sector_st_23 |> 
  unite("job_sector", job_sector_commerce:job_sector_99, na.rm = TRUE, sep = " ; ") |> 
  mutate(job_sector = str_replace_all(job_sector, "job_sector_",""),
         job_sector = str_replace(job_sector, "99","Other")
  )
st_23 <- left_join(st_23, sector_st_23)
rm(sector_st_23, wc)

## WORK 2020 ####
### Data dictionary ####
work_dict_poste <- read_xlsx("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/Data_dict_WORK_final_anonym.xlsx", 
                  sheet = "Feuil3", skip = 1) |> 
  mutate(poste = row_number()-1) |> select(-N) |> 
  add_row(Occupation = "Other", `Description of jobs` = "Other", poste = 99) # Add row of Other

work_dict_sector <- read_xlsx("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/Data_dict_WORK_final_anonym.xlsx", 
                             sheet = "Feuil4", skip = 1) |> 
  mutate(sect_activite = row_number()) |> select(-`Participating institutions/facilities`) |> 
  add_row(Sector = "Other", Description = "Other", sect_activite = 99) # Add row of other
  
### Dataset ####
work <- read_csv("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/SEROCoV-WORK_WP1_database_metiers_mzab4anup_matched_20240202.csv"
                 # , locale = readr::locale(encoding = "latin1") # uncomment in case accents don't appear normally
                 ) |> 
  mutate(
    date_soumission = as_date(mdy(date_du_rdv)),
    Hug_Date_Derniere_Soumission_C = as_date(Hug_Date_Derniere_Soumission_C),
    codbar = as.character(clean_main_codbar), # update codbar from clean codbar provided by Julien
    poste = case_when(is.na(poste_2) ~ poste_v2, .default = poste_2)
    ) |> 
  left_join(work_dict_poste) |> 
  left_join(work_dict_sector) |> 
  arrange(profession)

rm(work_dict_poste, work_dict_sector) # remove these intermediate files

# Last submission date ####
date_last_submission <- read_csv("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/rapport_sugar_date_dernière_soumission_all18+_mzab_2024.02.15.csv") |> 
  rename(participant_id = `Participant ID`,date_inclusion = `Date de réponse au questionnaire inclusion`,
         date_last_submission = `Date de dernière soumission (hors résultat)`) |> 
  mutate(date_inclusion = dmy(date_inclusion),
         date_last_submission = dmy(date_last_submission),
         beyond_inclusion = date_last_submission > date_inclusion
         )

# ## Monthly questionnaires ####
# monthly <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2021-05-19-1212_readable_dat_2021.05.19_monthly_V3.rds") |> 
#   mutate(date_soumission = as_date(date_sub)) |> 
#   arrange(date_soumission)
# am <- monthly |> group_by(codbar) |> filter(n()>1) |> 
#   arrange(codbar, date_sub) |> 
#   relocate(date_sub, .after = codbar_foyer)
# 
# length(unique(monthly$codbar))