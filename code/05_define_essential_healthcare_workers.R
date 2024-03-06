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
  select(participant_id, codbar, serocov_work, serocov_pop, sp2_novdec_2020, sp3_juin_2021, sp4_avril_2022, pop_pilote)

## Sante-travail 2022 ####
st_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1145_SanteTravail_ALLparticipants.rds") |> select(participant_id, job_sector) |> 
  rename(job_sector.st_22 = job_sector)

## Sante-travail 2023 ####
st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Pour_user/Pour_Anshu/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
  group_by(participant_id) |> filter(n() < 2) |> ungroup()  # remove duplicate entries

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
  select(participant_id, job_sector) |> rename(job_sector.st_23 = job_sector)
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
  left_join(st_22) |> 
  left_join(st_23) |> 
  mutate(
    key_occupation = case_when(is.na(isco_full) ~ NA,
                               key_occupation == "TRUE" ~ TRUE, .default = FALSE), # define essential / key worker
    # serocov_work = case_when(serocov_work ~ "Yes", .default = "No"), # recode as Yes / No
    # define health workers
    health_workers = case_when(is.na(isco_full) ~ NA,
                               HCW == "Yes" ~ TRUE, .default = FALSE),
    job_sector = case_when(
      source == "st_22" ~ job_sector.st_22,
      source == "st_23" ~ job_sector.st_23,
      .default = NA
    )
  ) |> 
  # filter(isco_full != -999) |> # remove unclassified people
  select(-c(HCW, label, occupational_grouping, Name_fr, job_sector.st_22, job_sector.st_23)) |>
  relocate(codbar, .after = participant_id)
  # Bus santé dataset-specific
  # mutate(Code = str_split_i(participant_id, "_", 1)) |> relocate(Code, .after = participant_id)
  

# Save dataset ####
## Local ####
# RDS
saveRDS(merged, file = here("output", "ISCO_fuzzy_recoded_occupations.rds"))
saveRDS(merged, file = here("output", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),
                                            "ISCO_fuzzy_recoded_occupations.rds")))

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
