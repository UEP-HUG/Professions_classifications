pacman::p_load(tidyverse, here, flextable)

# define standard myflextable function 
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", 
                       digits = 2,
                       na_str = "NA")
  return(x)
}

# Read in datasets ####
merged <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Base_de_données/classification_jobs_anup_jan_2024/2024-02-29-1553_ISCO_fuzzy_recoded_occupations.rds")
merged <- readRDS(here("output", "ISCO_fuzzy_recoded_occupations.rds"))

# Sante-travail 2022
st_22 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-09-21-1145_SanteTravail_ALLparticipants.rds") |> select(participant_id, job_sector)

# Sante-travail 2023
st_23 <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/Pour_user/Pour_Anshu/santé_travail_11_2023-202401221247.rds")|> # preliminary version from Sergeui
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
  select(participant_id, job_sector)
rm(sector_st_23, wc)
  

# Occupation summaries ####
summary_overall <- merged |> 
  filter(serocov_pop | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |> 
  summarise(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")
  ) |> 
  mutate(ISCO_label = "Overall", ISCO = "-") |> 
  relocate(c(ISCO_label, ISCO), .before = N)
  
summary_level_1 <- merged |> 
  filter(serocov_pop | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |> 
  group_by(ISCO_label_1, isco_1) |> 
  # count(ISCO_label_1, isco_1,health_workers)
  summarise(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")
            ) |> 
  rename(ISCO_label = ISCO_label_1, ISCO = isco_1) |> 
  mutate(ISCO = as.integer(ISCO)) |> 
  arrange(ISCO)

summary_level_2 <- merged |> 
  filter(serocov_pop | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |> 
  group_by(ISCO_label_2, isco_2) |> 
  # count(ISCO_label_1, isco_1,health_workers)
  summarise(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")
  ) |> 
  rename(ISCO_label = ISCO_label_2, ISCO = isco_2) |> 
  mutate(ISCO = as.integer(ISCO)) |> 
  arrange(ISCO)

summary_1 <- rbind(summary_overall, summary_level_1) 
summary_1 |> myflextable() |> 
  width(j = 1, 3.8, unit = "in") |> 
  bold(i = 1)


summary_2 <- rbind(summary_overall, summary_level_2) 
summary_2 |> myflextable() |> 
  width(j = 1, 3.8, unit = "in") |> 
  bold(i = 1)

# Key workers ####
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

# Health workers ####
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

# Job sector summaries: st_22 ####
merged_st_22 <- left_join(merged, st_22) |> filter(!is.na(job_sector) & source == "st_22")
merged_st_22_overall <- merged_st_22 |> 
  summarise(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")
  ) |> 
  mutate(job_sector = "Overall") |> 
  relocate(job_sector, .before = N)

merged_st_22 <- merged_st_22 |> 
  group_by(job_sector) |> 
  summarize(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%"))

rbind(merged_st_22_overall, merged_st_22) |> 
  myflextable() |> 
  width(j = 1, 5.5, unit = "in") |> 
  bold(i = 1)

# Job sector summaries: st_23 ####
merged_st_23 <- left_join(merged, st_23) |>   filter(!is.na(job_sector) & source == "st_23")
merged_st_23_overall <- merged_st_23 |> 
  summarise(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")
  ) |> 
  mutate(job_sector = "Overall") |> 
  relocate(job_sector, .before = N)

merged_st_23 <- merged_st_23 |> 
  mutate(
    job_sector_lumped = job_sector,
    job_sector_lumped = forcats::fct_lump_min(job_sector_lumped,5,other_level = "Other")
    ) |> 
  group_by(job_sector_lumped) |> 
  summarize(N = n(),
            HCW_n = sum(health_workers, na.rm = TRUE),
            HCW_percent = paste0(round(HCW_n / N, 3)*100, "%")) |> 
  rename(job_sector = job_sector_lumped) |> 
  arrange(-N)

rbind(merged_st_23_overall, merged_st_23) |> 
  myflextable() |> 
  width(j = 1, 4, unit = "in") |> 
  bold(i = 1)
