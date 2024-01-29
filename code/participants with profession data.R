pacman::p_load(here)
source(here("code","02_combine_datasets.R"))

exp_st22 <- dat_st_22 |> select(codbar)
exp_st23 <- dat_st_23 |> select(codbar)
exp_incl_kids <- incl_kids |> select(parent1_codbar, parent1_profession, ) |> 
  filter(!is.na(parent1_profession)) |> 
  mutate(filled_inc_kids = TRUE) |> 
  arrange(parent1_codbar) |> 
  distinct() |> 
  group_by(parent1_codbar) |> filter(n() == 1) |> ungroup()

exp_inc <- dat_inclusion |> select(codbar, work_pilote.inc, serocov_work.inc)

# Combine to show which participants completed EITHER the st_22 or st_23 questionnaires
dat_st_both <- full_join(exp_st22, exp_st23) |> mutate(filled_st = TRUE)


# Merge st_both with inclusion 
master_dat <- left_join(exp_inc, dat_st_both)
# Merge with incl_kids
master_dat <- left_join(master_dat, exp_incl_kids, by = join_by("codbar" == "parent1_codbar"))

# Add a variable to show if participant completed any st questionnaire OR participated in Sero-Cov-WORK OR completed KIDS inclusion
master_dat <- master_dat |> 
  mutate(filled_st = case_when(is.na(filled_st) ~ FALSE,
                               .default = filled_st),
         filled_inc_kids = case_when(is.na(filled_inc_kids) ~ FALSE,
                                     .default = filled_inc_kids),
         work_OR_st_OR_incKIDS = work_pilote.inc | serocov_work.inc | filled_st |filled_inc_kids) |> 
  filter(work_OR_st_OR_incKIDS == TRUE) # 7,156 participants with free-text profession information

inc_gh_merge <- inner_join(master_dat, dat_gh_22)

gh_merge <- inner_join(dat_gh_22, dat_gh_23)
both_gh_master_merge <- inner_join(gh_merge, master_dat)
