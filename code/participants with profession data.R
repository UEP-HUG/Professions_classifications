pacman::p_load(here)
source(here("code","02_combine_datasets.R"))

exp_st22 <- dat_st_22 |> select(codbar, profession.st_22, job_sector.st_22, job_sector_other.st_22, supervision.st_22, years_of_service.st_22)
exp_st23 <- dat_st_23 |> select(codbar, ew_professsion.st_23,job.st_23,years_of_service.st_23)
exp_incl_kids <- incl_kids |> select(parent1_codbar, parent1_profession, parent1_occupation) |> 
  filter(!is.na(parent1_profession)) |>
  mutate(filled_inc_kids = TRUE) |> 
  arrange(parent1_codbar) |> 
  distinct() |> 
  group_by(parent1_codbar) |> filter(n() == 1) |> ungroup()

exp_inc <- dat_inclusion |> select(codbar, work_pilote.inc, serocov_work.inc, profession.inc, profession_other.inc)

# Combine to show which participants completed EITHER the st_22 or st_23 questionnaires
dat_st_both <- full_join(exp_st22, exp_st23) |> mutate(filled_st = TRUE)


# Merge st_both with inclusion 
dat_master_professions <- left_join(exp_inc, dat_st_both)
# Merge with incl_kids
dat_master_professions <- left_join(dat_master_professions, exp_incl_kids, by = join_by("codbar" == "parent1_codbar"))

# Add a variable to show if participant completed any st questionnaire OR participated in Sero-Cov-WORK OR completed KIDS inclusion
dat_master_professions <- dat_master_professions |> 
  mutate(filled_st = case_when(is.na(filled_st) ~ FALSE,
                               .default = filled_st),
         filled_inc_kids = case_when(is.na(filled_inc_kids) ~ FALSE,
                                     .default = filled_inc_kids),
         work_OR_st_OR_incKIDS = work_pilote.inc | serocov_work.inc | filled_st |filled_inc_kids) |> 
  filter(work_OR_st_OR_incKIDS == TRUE) # 7,156 participants with free-text profession information


# Merge checks with other datasets
# inc_gh_merge <- inner_join(dat_master_professions, dat_gh_22)
# gh_merge <- inner_join(dat_gh_22, dat_gh_23)
# both_gh_master_merge <- inner_join(gh_merge, dat_master_professions)

### ### ###
# Keep only Master Dataset ####
### ### ###
rm(list=setdiff(ls(), c("dat_master_specchio", "dat_master_professions")))