pacman::p_load(here)
source(here("code","02_combine_datasets.R"))

exp_work <- dat_work
exp_st22 <- dat_st_22 |> select(codbar, profession.st_22, job_sector.st_22, job_sector_other.st_22, supervision.st_22, years_of_service.st_22)
exp_st23 <- dat_st_23 |> select(codbar, ew_professsion.st_23,job.st_23,years_of_service.st_23)
exp_inc_kids <- dat_inc_kids |> select(parent1_codbar, parent1_profession.inc_kids, 
                                         parent1_occupation.inc_kids, parent1_occupation_other.inc_kids, parent1_occupation_cat.inc_kids) |> 
  filter(!is.na(parent1_profession.inc_kids)) |>
  mutate(filled_inc_kids = TRUE) |> 
  arrange(parent1_codbar) |> 
  distinct() |> 
  group_by(parent1_codbar) |> filter(n() == 1) |> ungroup()

exp_inc <- dat_inclusion |> select(participant_id, codbar, serocov_work.inc, profession.inc, profession_other.inc)

# Combine to show which participants completed EITHER the st_22 or st_23 questionnaires
dat_st_both <- full_join(exp_st22, exp_st23) |> mutate(filled_st = TRUE)


# Merge st_both with inclusion 
dat_master_professions <- left_join(exp_inc, dat_st_both)
# Merge with incl_kids
dat_master_professions <- left_join(dat_master_professions, exp_inc_kids, by = join_by("codbar" == "parent1_codbar"))
# Merge with WORK
dat_master_professions <- left_join(dat_master_professions, exp_work)

# Add a variable to show if participant completed any st questionnaire OR participated in Sero-Cov-WORK OR completed KIDS inclusion
dat_master_professions <- dat_master_professions |> 
  mutate(filled_st = case_when(is.na(filled_st) ~ FALSE,
                               .default = filled_st),
         filled_inc_kids = case_when(is.na(filled_inc_kids) ~ FALSE,
                                     .default = filled_inc_kids),
         work_OR_st_OR_incKIDS =  serocov_work.inc | filled_st |filled_inc_kids | !is.na(date_soumission.WORK))


# Merge checks with other datasets
# inc_gh_merge <- inner_join(dat_master_professions, dat_gh_22)
# gh_merge <- inner_join(dat_gh_22, dat_gh_23)
# both_gh_master_merge <- inner_join(gh_merge, dat_master_professions)

# Fill in the master_profession variable with profession data from different datasets, prioritizing the first occupation entry (closer to the pandemic)
dat_master_professions_2 <- dat_master_professions |> 
  mutate(
    master_profession = NA, # Initialize an empty variable that will be filled up
    master_profession = case_when(is.na(master_profession) & !is.na(profession.WORK) # the !is.na here is to only use non-empty fields
                                  ~ paste0(profession.WORK,"^Work"), .default = master_profession), #^Work here allows later ID of source dataset
    master_profession = case_when(is.na(master_profession) & !is.na(profession_other.inc) 
                                  ~ paste0(profession_other.inc,"^inc"), .default = master_profession),
    master_profession = case_when(is.na(master_profession) & !is.na(parent1_profession.inc_kids) 
                                  ~ paste0(parent1_profession.inc_kids,"^inc_kids"), .default = master_profession),
    master_profession = case_when(is.na(master_profession) & !is.na(parent1_occupation_other.inc_kids) 
                                  ~ paste0(parent1_occupation_other.inc_kids,"^inc_kids"), .default = master_profession),
    master_profession = case_when(is.na(master_profession) & !is.na(profession.st_22) 
                                  & !profession.st_22 %in% c("-", "--", "---", ".", "...", "/", "///", "::::", "?") # No information, skip to next variable
                                  ~ paste0(profession.st_22,"^st_22"), .default = master_profession),
    master_profession = case_when(is.na(master_profession) & !is.na(job.st_23) 
                                  ~ paste0(job.st_23,"^st_23"), .default = master_profession),
    master_profession = case_when(is.na(master_profession) & !is.na(ew_professsion.st_23) 
                                  ~ paste0(ew_professsion.st_23,"^st_23"), .default = master_profession)
  ) |> 
  # Use the ^ symbol to separate into profession and dataset source columns
  separate_wider_delim(master_profession, delim = "^", names = c("master_profession", "profession_source")) |> 
  # arrange the variables according to which ones will be referred to first
  relocate(c(master_profession,profession_source, profession.WORK, profession_other.inc, parent1_profession.inc_kids, parent1_occupation_other.inc_kids, 
             profession.st_22,job.st_23, ew_professsion.st_23), .after = serocov_work.inc) |> 
  # Remove the entries that still have NA in the master_profession field
  filter(!is.na(master_profession))

### ### ###
# Keep only Master Dataset ####
### ### ###
rm(list=setdiff(ls(), c("dat_master_specchio", "dat_master_professions", "dat_master_professions_2")))
