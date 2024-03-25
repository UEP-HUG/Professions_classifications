pacman::p_load(
  here,
  tidyverse,
  flextable,
  readxl
)

# Input data ####
## Read in the fuzzy matches dataset that needs to be cleaned ####
if (file.exists(here("output", "cleaned_fuzzy_classified_occupations.rds"))) {
  occup_final_cleaned <- readRDS(here("output", "cleaned_fuzzy_classified_occupations.rds"))
} else {
  source(here("code", "04b_long_clean_fuzzy_classifications.R"))
}

## Inclusion ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |>
  filter(Origin != "V5_inclusion") |>
  select(participant_id, codbar, serocov_work, serocov_pop, sp2_novdec_2020, sp3_juin_2021, sp4_avril_2022, pop_pilote, work_situation) |> 
  rename(work_situation.inc = work_situation)

## Inclusion - KIDS ####
inc_kids <- readRDS("P:/ODS/DMCPRU/UEPDATA/SEROCoV-KIDS/99_data/6_database/00_cleaned_augmented_database/2024-01-24_KIDS_inclusion_nov21_202401231106.rds") |> 
  filter(!is.na(parent1_profession)) |>
  select(parent1_codbar, parent1_work_situation) |> 
  rename(codbar = parent1_codbar, work_situation.inc_kids = parent1_work_situation) |>
  arrange(codbar) |> 
  distinct() |> 
  group_by(codbar) |> filter(n() == 1) |> ungroup()

## Work ####
### Sector index ####
work_dict_sector <- read_xlsx("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/Data_dict_WORK_final_anonym.xlsx", 
                              sheet = "Feuil4", skip = 1) |> 
  mutate(sect_activite = row_number()) |> select(-`Participating institutions/facilities`) |> 
  add_row(Sector = "Other", Description = "Other", sect_activite = 99) # Add row of other
### Work dataset ####
work <- read_csv("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/SEROCoV-WORK_WP1_database_metiers_mzab4anup_2024.03.07.csv"
                 , locale = readr::locale(encoding = "latin1") # uncomment in case accents don't appear normally
)|> 
  mutate(codbar = as.character(clean_main_codbar)) |>  # update codbar from clean codbar provided by Julien
  select(codbar, Secteur_group, poste_final, sect_activite) |> 
  left_join(work_dict_sector) |> select(-Description) |> 
  mutate(
    sector_self_reported.work = case_when(
      !is.na(Sector) ~ TRUE,
      .default = FALSE),
    job_sector.work = case_when(
      sector_self_reported.work ~ Sector,
      .default = Secteur_group
    )
  )

## Sante-travail 2022 ####
st_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1145_SanteTravail_ALLparticipants.rds") |> 
  select(participant_id, job_sector, years_of_service, work_situation) |> 
  rename_with(~ paste(., "st_22", sep = "."), !matches(c("codbar", "participant_id"))) |> 
  mutate(sector_self_reported.st_22 = TRUE)

## Sante-travail 2023 ####
st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
  group_by(participant_id) |> filter(n() < 2) |> ungroup()  # remove duplicate entries

### Unite job status ####
status_st_23 <- st_23 |> select(participant_id, starts_with("status")) |> 
  select(-c(status_other_text, status_change)) |>
  mutate(across(starts_with("status"), ~na_if(., FALSE)))

status_wc <- droplevels(col(status_st_23, as.factor=TRUE)[which(status_st_23 == "TRUE")])
status_st_23[levels(status_wc)] <- Map(factor, status_st_23[levels(status_wc)], labels = levels(status_wc))
status_st_23 <- status_st_23 |> 
  unite("job_status", status_employed:status_99, na.rm = TRUE, sep = " ; ") |> 
  mutate(job_status = str_replace_all(job_status, "status_",""),
         job_status = str_replace(job_status, "99","Other")
  )
st_23 <- left_join(st_23, status_st_23)
rm(status_st_23, status_wc)


### Unite job sectors ####
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
st_23 <- left_join(st_23, sector_st_23) |> 
  select(participant_id, job_sector, years_of_service, job_status) |> 
  rename_with(~ paste(., "st_23", sep = "."), !matches(c("codbar", "participant_id"))) |> 
  mutate(sector_self_reported.st_23 = TRUE)

rm(sector_st_23, wc)

# Key occupations indices from ILO paper (Berg et al., 2023) ####
key_occupations <- read_xlsx(here("data", "indices_key_HCW.xlsx"), sheet = "key_occupations_ILO_ISCO2")
HCW <- read_xlsx(here("data", "indices_key_HCW.xlsx"), sheet = "HCW_WHO")


# Merge the classified occupations with the indices for frontline ("key") occupations
merged <- left_join(occup_final_cleaned, key_occupations, by = join_by("isco_2" == "ISCO_2")) |>
  # Merge with indices for "healthcare worker"
  left_join(HCW, by = join_by("isco_full" == "ISCO")) |> 
  # Merge with inclusion for serocov_work variable
  left_join(inclusion) |> # comment out for bus santé
  left_join(inc_kids) |> 
  left_join(work) |> 
  left_join(st_22) |> 
  left_join(st_23) |> 
  mutate(
    years_of_service = case_when(
      source == "st_22" ~ years_of_service.st_22,
      source == "st_23" ~ years_of_service.st_23,
      .default = NA
    ),
    work_situation = case_when(
      source == "Work" ~ "Employed",
      source == "st_22" ~ work_situation.st_22,
      source == "st_23" ~ job_status.st_23,
      source == "Inclusion" ~ work_situation.inc,
      source == "inc_kids" ~ work_situation.inc_kids,
      .default = NA
    ),
    key_occupation = case_when(is.na(isco_full) ~ NA,
                               key_occupation == "TRUE" ~ TRUE, .default = FALSE), # define essential / key worker
    # serocov_work = case_when(serocov_work ~ "Yes", .default = "No"), # recode as Yes / No
    # define health workers
    health_workers = case_when(is.na(isco_full) ~ NA,
                               HCW == "Yes" ~ TRUE, .default = FALSE),
    job_sector = case_when(
      source == "Work" ~ job_sector.work,
      source == "st_22" ~ job_sector.st_22,
      source == "st_23" ~ job_sector.st_23,
      .default = NA),
    sector_self_reported = case_when(
      source == "Work" ~ sector_self_reported.work,
      source == "st_22" ~ sector_self_reported.st_22,
      source == "st_23" ~ sector_self_reported.st_23,
      .default = NA)
  ) |> 
  # filter(isco_full != -999) |> # remove unclassified people
  select(-c(HCW, label, occupational_grouping, Name_fr, job_sector.st_22, job_sector.st_23, 
            starts_with("years_of_service."), starts_with("work_situation."), starts_with("job_status."))) |>
  relocate(codbar, .after = participant_id) |> 
  # Bus santé dataset-specific
  # mutate(Code = str_split_i(participant_id, "_", 1)) |> relocate(Code, .after = participant_id)
  
  # Harmonize the job sectors across WORK and the Santé Travail questionnaires
  # ?? where should I put these :
  # ?? Agroalimentaire --> Commerce
  # ?? Entreprises privées --> Other
  # ?? Food industry --> ??
  mutate(
    job_sector_harmonized = case_when(
      job_sector %in% c("health_social", "health_social ; public_admin", "Action social",
                        "EMS", "Pharmacie", "Santé", "Healthcare",
                        "Nursing homes", "Social work", "Pharmacy") ~ "Santé, social, médico-social",
      job_sector %in% c("public_admin", "Administration publique", "Public administration") ~ "Administration publique",
      job_sector %in% c("education_research", "public_admin ; education_research") ~ "Enseignement, recherche",
      job_sector %in% c("banking_insurance", "Activité financière", "Financial services") ~ "Banques, assurances",
      job_sector %in% c("Other", "Entreprises privées", "Food industry") ~ "Autre",
      job_sector %in% c("security", "Securité publique", "Public security") ~ "Sécurité, secours (police, gendarmerie, pompiers, ambulances)",
      job_sector %in% c("legal_accounting", "legal_accounting ; public_admin") ~ "Activités juridiques, comptabilité, secrétariat",
      job_sector %in% c("transportation", "Transport", "Transportation") ~ "Secteur des transports et de l’entreposage",
      job_sector %in% c("commerce") ~ "Commerce",
      job_sector %in% c("manufacturing") ~ "Secteur de l’industrie, de la fabrication de biens (aliments, machines ou d’autres articles)",
      job_sector %in% c("embassy", "Organisation internationale", "International organizations") ~ "Ambassade, organisation internationale",
      job_sector %in% c("it_comms", "Media") ~ "Information et communication (publicité, marketing, journalisme, réseaux sociaux)",
      job_sector %in% c("childcare", "Crèche", "Early childhood education") ~ "Petite enfance (maman de jour, éducateur-trice de jeunes enfants, etc.)",
      job_sector %in% c("r_and_d") ~ "Bureaux d’études, recherche et développement (R&D), architecture",
      job_sector %in% c("art_library") ~ "Arts, spectacle, musée, bibliothèque",
      job_sector %in% c("real_estate") ~ "Immobilier, agences (location, voyages, placement)",
      job_sector %in% c("construction", "Construction") ~ "Secteur de la construction ou de la rénovation (métiers du bâtiment, peinture, génie civil, etc.)",
      job_sector %in% c("agriculture", "Agriculture") ~ "Secteur de l’agriculture, sylviculture, horticulture, entretien des espaces verts, élevage, pêche, etc.",
      job_sector %in% c("consulting") ~ "Services de conseils (conseil en gestion de patrimoine, coaching, etc.)",
      job_sector %in% c("utilities") ~ "Secteur de la production ou de la distribution (électricité, gaz, air conditionné, eau, gestion des déchets, etc.)",
      job_sector %in% c("accomodation_food") ~ "Hébergement et restauration",
      job_sector %in% c("domestic", "Soins à domicile", "Homecare") ~ "Services domestiques (aide-ménagère, aide à domicile)",
      job_sector %in% c("extraction") ~ "Secteur de l’extraction de matières premières (houille, sel, etc.)",
      job_sector %in% c("personal_service") ~ "Services à la personne (coiffeur-se, esthéticien-ne, etc.)",
      source %in% c("st_23", "Work") & !is.na(job_sector) ~ "Autre",
      .default = job_sector
    )) |> 
  relocate(job_sector_harmonized, .after = job_sector) |> 
  select(-c(Secteur_group, poste_final, contains("sector_self_reported."), job_sector.work, sect_activite, Sector))


# Save dataset ####
## Local ####
# RDS
saveRDS(merged, file = here("output", "ISCO_fuzzy_recoded_occupations.rds"))
# saveRDS(merged, file = here("output", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),
#                                              "ISCO_fuzzy_recoded_occupations.rds")))

# ## In the Share drive ####
# # RDS
# saveRDS(merged, file = paste0("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/",
#                               format(Sys.time(), "%Y-%m-%d-%H%M_"),
#                               "ISCO_fuzzy_recoded_occupations.rds"))
# # CSV
# write_csv(merged, file = paste0("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/",
#                                 format(Sys.time(), "%Y-%m-%d-%H%M_"),
#                                 "ISCO_fuzzy_recoded_occupations.csv"))

# Visually check that health workers are properly classified 
health_w_check <- merged |> filter(health_workers == TRUE)
