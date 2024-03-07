pacman::p_load(here)
source(here("code","02_combine_datasets.R"))

# Datasets with some free-text on occupation ####
exp_work <- dat_work |> select(codbar, date_soumission.WORK) |> mutate(source = "Work") |> rename(date_soumission = date_soumission.WORK)
exp_st22 <- dat_st_22 |> select(codbar, date_soumission.st_22) |> mutate(source = "st_22") |> rename(date_soumission = date_soumission.st_22)
exp_st23 <- dat_st_23 |> select(codbar, date_soumission.st_23) |> mutate(source = "st_23") |> rename(date_soumission = date_soumission.st_23)
exp_inc_kids_pre <- dat_inc_kids |>
  filter(!is.na(parent1_profession.inc_kids)) |>
  arrange(parent1_codbar) |> 
  distinct() |> 
  group_by(parent1_codbar) |> filter(n() == 1) |> ungroup()
exp_inc_kids <- exp_inc_kids_pre |> 
  select(parent1_codbar, date_soumission.inc_kids) |> mutate(source = "inc_kids") |> rename(codbar = parent1_codbar, date_soumission = date_soumission.inc_kids)

# Inclusion dataset
exp_inc <- dat_inclusion |> select(participant_id, codbar, date_soumission.inc)


# Combine the datasets ####
master_dataset <- rbind(exp_work, exp_inc_kids, exp_st22, exp_st23) |> 
  right_join(exp_inc) |> 
  mutate(source = case_when(is.na(source) ~ "Inclusion", .default = source),
         date_soumission = case_when(source == "Inclusion" ~ date_soumission.inc, .default = date_soumission)) |> 
  # Join with the file that indicates last submission date for questionnaires
  left_join(date_last_submission |> select(participant_id, beyond_inclusion)) |>
  filter(beyond_inclusion) |> select(-beyond_inclusion) |> # filter out people that never participated beyond inclusion questionnaire
  left_join(dat_work) |> 
  left_join(exp_inc_kids_pre, by = join_by(codbar == parent1_codbar)) |> 
  left_join(dat_st_22) |> 
  left_join(dat_st_23) |> 
  left_join(dat_inclusion |> select(codbar, date_soumission.inc, profession_other.inc, work_situation_rec_en.inc, profession.inc))

dat_master_professions_long <- master_dataset |> 
  mutate(
    master_profession = case_when(
      source == "Work" ~ profession.WORK,
      source == "inc_kids" ~ parent1_profession.inc_kids,
      source == "st_22" ~ profession.st_22,
      source == "st_23" ~ job.st_23,
      source == "Inclusion" ~ profession_other.inc,
      .default = NA
    ),
    complementary_info = case_when(
      source == "Work" ~ if_else(is.na(poste_2_autre.WORK), Secteur_group.WORK,
                                 paste(poste_2_autre.WORK, Secteur_group.WORK, sep = " | ")),
      source == "inc_kids" ~ if_else(is.na(parent1_occupation_other.inc_kids), parent1_occupation.inc_kids,
                                     paste(parent1_occupation.inc_kids, parent1_occupation_other.inc_kids, sep = " | ")),
      source == "st_22" ~ if_else(is.na(job_sector_other.st_22), job_sector.st_22,
                                  paste(job_sector.st_22, job_sector_other.st_22, sep = " | ")),
      source == "st_23" ~ if_else(is.na(job_sector_other_text.st_23), job_sector.st_23,
                                  paste(job_sector.st_23, job_sector_other_text.st_23, sep = " | ")),
      source == "Inclusion" ~ if_else(is.na(profession.inc), work_situation_rec_en.inc,
                                      paste(work_situation_rec_en.inc, profession.inc, sep = " | ")),
      .default = NA
    ),
    management = case_when(
      source == "st_22" ~ supervision.st_22,
      source == "st_23" ~ supervision.st_23,
      .default = NA
    ),
    management = case_when(
      str_detect(management, "Oui, et") ~ "Management (main duty)",
      str_detect(management, "Oui, mais") ~ "Some management",
      str_detect(management, "Non") ~ "No management",
      .default = NA
    )
  ) |> 
  relocate(c(master_profession,complementary_info, management), .after = source) |> 
  select(participant_id, codbar, source, master_profession, complementary_info, management, date_soumission) |>
  mutate(
    master_profession = case_when(
      codbar == "1104061" & source == "st_22" ~ "Administration et Ressources humaines",
      codbar == "1710291" & source == "st_22" ~ "Manager",
      .default = master_profession)) |> 
  filter(!is.na(master_profession))

### ### ###
# Keep only Master Dataset ####
### ### ###
rm(list=setdiff(ls(), c("dat_master_professions_long")))
saveRDS(dat_master_professions_long, file = here("output", "dat_master_professions_long.rds"))
