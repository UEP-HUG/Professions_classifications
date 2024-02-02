# Generate a master dataset that merges all Specchio datasets

pacman::p_load(here)

# Read in the datasets file
source(here("code","01_read_in_datasets.R"))

### ### ###
# Select only variables of interest, to make minimal datasets ####
### ### ###
## Inclusion - All ####
dat_inclusion <- inclusion |> 
  select(participant_id, codbar, age, serocov_work, work_pilote,  work_situation_rec_en, profession, profession_other,
         occupation_cat_en, work_rata,  education, education_other, education_rec_en) |> 
  rename_with(~ paste(., "inc", sep = "."), !matches(c("codbar", "participant_id")))

## Inclusion - KIDS ####  --> Still need to combine this into the master_specchio dataset
dat_inc_kids <- inc_kids |> 
  select(parent1_codbar, parent1_profession, parent1_occupation, parent1_occupation_other, parent1_occupation_cat) |> 
  rename_with(~ paste(., "inc_kids", sep = "."), !matches(c("codbar")))

## General Health ####
### 2022 ####
dat_gh_22 <- gh_22 |> 
  select(codbar, age, events_job_loss, events_burn_out, events_other_text) |> 
  rename_with(~ paste(., "gh_22", sep = "."), !matches(c("codbar")))

### 2023 ####
dat_gh_23 <- gh_23 |> 
  select(codbar, age, events_job_loss, events_retirement,events_moving, events_other_text, d_psycho_professional_burnout, d_psycho_personnal_burnout, ) |> 
  rename_with(~ paste(., "gh_23", sep = "."), !matches(c("codbar")))

## Sommeil ####
### 2023 ####
dat_sommeil_23 <- sommeil_23 |> 
  select(codbar, age, situation_night_work, situation_shift_work, address, screen_time_work) |> 
  rename_with(~ paste(., "sommeil_23", sep = "."), !matches(c("codbar")))

## Health Behavior: 2022 ####
dat_hb_22 <- hb_22 |> 
  select(codbar, age, job_phys_activity, job_phys_activity_other) |> 
  rename_with(~ paste(., "hb_22", sep = "."), !matches(c("codbar")))

## Santé Travail ####
### 2022 ####
dat_st_22 <- st_22 |> 
  select(codbar, age, burn_out, employed:not_employed_comment,
         work_situation, work_situation_other, profession, job_sector, job_sector_other,
         commute_work_from_home:sedentary_work_other, protective_equipment:protection_other,
         burn_out:burn_out_result_other, pandemic_change:impact_pandemic_comment
        ) |> 
  rename_with(~ paste(., "st_22", sep = "."), !matches(c("codbar")))

### 2022 ####
dat_st_23 <- st_23 |> 
  select(codbar, age, worked:workplace_size,years_of_service:move_work_other_text, 
  ) |> 
  rename_with(~ paste(., "st_23", sep = "."), !matches(c("codbar")))


## WORK 2020 ####
dat_work <- work |> 
  select(ID, codbar, Hug_Date_Derniere_Soumission_C, sexe, annee_naissance, profession, date_soumission, Occupation, Sector) |> 
  rename_with(~ paste(., "WORK", sep = "."), !matches(c("codbar")))

### ### ###
# Combine the timepoints into a Master dataset ####
### ### ###

## Join Inclusion - Parents with General Health 2022 ####
dat_master_specchio <- left_join(dat_inclusion, dat_gh_22, 
                                 by = "codbar")

## Join with General Health 2023 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_gh_23, 
                                 by = "codbar")

## Join with Sommeil 2023 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_sommeil_23, 
                                 by = "codbar")

## Join with Health Behavior 2022 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_hb_22, 
                                 by = "codbar")


## Join with Sante Travail 2022 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_st_22, 
                                 by = "codbar")

## Join with WORK 2020 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_work, 
                                 by = "codbar")

### ### ###
# Keep only Master Dataset ####
### ### ###
# rm(list=setdiff(ls(), c("dat_master_specchio")))