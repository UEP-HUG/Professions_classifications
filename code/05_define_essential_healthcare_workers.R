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

inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |>
  filter(Origin != "V5_inclusion") |>
  select(participant_id, codbar, serocov_work)

# Key occupations indices from ILO paper (Berg et al., 2023)
key_occupations <- read_xlsx(here("data", "indices_key_HCW.xlsx"), sheet = "key_occupations_ILO_ISCO2")
HCW <- read_xlsx(here("data", "indices_key_HCW.xlsx"), sheet = "HCW_WHO")


# Merge the classified occupations with the indices for frontline ("key") occupations
merged <- left_join(occup_final_cleaned, key_occupations, by = join_by("isco_2" == "ISCO_2")) |>
  # Merge with indices for "healthcare worker"
  left_join(HCW, by = join_by("isco_full" == "ISCO")) |> 
  # Merge with inclusion for serocov_work variable
  left_join(inclusion) |> 
  mutate(
    key_occupation = case_when(key_occupation == "TRUE" ~ TRUE, .default = FALSE), # define essential / key worker
    serocov_work = case_when(serocov_work ~ "Yes", .default = "No"), # recode as Yes / No
    health_workers = case_when(HCW == "Yes" ~ TRUE, .default = FALSE) # define health workers
  ) |> 
  # filter(isco_full != -999) |> # remove unclassified people
  select(-c(HCW, label, occupational_grouping)) |>
  relocate(codbar, .after = participant_id)

# Save dataset ####
## Local ####
# # RDS
# saveRDS(merged, file = here("output", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),
#                                             "ISCO_fuzzy_recoded_occupations.rds")))

## In the Share drive ####
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

# Summaries ####
# define standard myflextable function 
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", digits = 2, na_str = "NA")
  return(x)
}

## Key workers ####
by_overall_key <- merged |> 
  count(key_occupation) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |> 
  mutate(serocov_work = "Overall") |> relocate(serocov_work)

by_work_key <- merged |> 
  group_by(serocov_work) |>
  count(key_occupation) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%"))
# Bind them and output a summary table
rbind(by_work_key, by_overall_key) |> myflextable()

## Health workers ####
by_overall_health <- merged |> 
  count(health_workers) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |> 
  mutate(serocov_work = "Overall") |> relocate(serocov_work)

by_work_health <- merged |> 
  group_by(serocov_work) |>
  count(health_workers) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%"))
# Bind them and output a summary table
rbind(by_work_health, by_overall_health) |> myflextable()
