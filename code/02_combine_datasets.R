# Generate a master dataset that merges all Specchio datasets

pacman::p_load(here)

# Read in the datasets file
source(here("code","01_read_in_datasets.R"))

### ### ###
# Select only variables of interest, to make minimal datasets ####
### ### ###
## Inclusion - Parents ####
dat_inclusion <- inclusion |> 
  select(codbar, work_situation_rec_en, profession) |> 
  rename_with(~ paste(., "incl", sep = "."), !matches(c("codbar")))

## General Health ####
### 2022 ####
dat_gh_22 <- gh_22 |> 
  select(codbar,events_job_loss, events_burn_out, events_other_text) |> 
  rename_with(~ paste(., "gh_22", sep = "."), !matches(c("codbar")))

### 2023 ####
dat_gh_23 <- gh_23 |> 
  select(codbar,events_job_loss, events_retirement, events_other_text) |> 
  rename_with(~ paste(., "gh_23", sep = "."), !matches(c("codbar")))

## Santé Travail ####
### 2022 ####
dat_st_22 <- st_22 |> 
  select(codbar, burn_out, ) |> 
  rename_with(~ paste(., "st_22", sep = "."), !matches(c("codbar")))

### ### ###
# Combine the timepoints into a Master dataset ####
### ### ###

## Join Inclusion - Parents with General Health 2022 ####
dat_master_specchio <- left_join(dat_incl_parents, dat_gh_22, 
                                 by = join_by("parent1_codbar" == "codbar"))

## Join with General Health 2023 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_gh_23, 
                                 by = join_by("parent1_codbar" == "codbar"))

## Join with Sante Travail 2022 ####
dat_master_specchio <- left_join(dat_master_specchio, dat_st_22, 
                                 by = join_by("parent1_codbar" == "codbar"))

### ### ###
# Keep only Master Dataset ####
### ### ###
rm(list=setdiff(ls(), c("dat_master_specchio", "dat_master_KIDS")))