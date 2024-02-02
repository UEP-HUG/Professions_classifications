pacman::p_load(
  here,
  tidyverse,
  flextable
)

# Input data ####
source(here("code","04_classify_occupations.R")) # can source this file if dataset not already created
# occup_ISCO_final <- readRDS(here("data", "Classified_occupations.rds"))

# Key occupations indices from ILO paper (Berg et al., 2023)
key_occupations <- read_csv2(here("data", "key_occupations_ILO_ISCO2.csv"))

# Merge the classified occupations with the indices for frontline ("key") occupations
merged <- left_join(occup_ISCO_final, key_occupations, by = join_by(
  "isco_2" == "ISCO_2")
  ) |> 
  mutate(key_occupation = case_when(key_occupation ~ TRUE, .default = FALSE), # define essential / key worker
         serocov_work.inc = case_when(serocov_work.inc ~ "Yes", .default = "No"), # recode as Yes / No
         health_workers = case_when(occupational_grouping == "Health workers" ~ TRUE, .default = FALSE) # define health workers
  ) |> 
  filter(isco_full != -999) # remove unclassified people

# Save dataset
saveRDS(merged, file = paste0("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/",
                              format(Sys.time(), "%Y-%m-%d-%H%M_"),
                              "ISCO_recoded_essential_plus_health_workers.rds"))
write_csv(merged, file = paste0("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/",
                              format(Sys.time(), "%Y-%m-%d-%H%M_"),
                              "ISCO_recoded_essential_plus_health_workers.csv"))

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
  mutate(serocov_work.inc = "Overall") |> relocate(serocov_work.inc)

by_work_key <- merged |> 
  group_by(serocov_work.inc) |>
  count(key_occupation) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%"))
# Bind them and output a summary table
rbind(by_work_key, by_overall_key) |> myflextable()
  
## Health workers ####
by_overall_health <- merged |> 
  count(health_workers) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |> 
  mutate(serocov_work.inc = "Overall") |> relocate(serocov_work.inc)

by_work_health <- merged |> 
  group_by(serocov_work.inc) |>
  count(health_workers) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%"))
# Bind them and output a summary table
rbind(by_work_health, by_overall_health) |> myflextable()

