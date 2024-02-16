pacman::p_load(
  tidyverse,
  here,
  data.table,
  fuzzyjoin,
  stringi
)

# Occupations from I14Y Swiss platform ####

## Read in occupations file at level 6 ####
# Downloaded from https://www.i14y.admin.ch/fr/catalog/datasets/HCL_CH_ISCO_19_PROF_1_2/api
# Export à un niveau
# Niveau: 5
# Langue: FR
# Annotations: Sans annotations
# Format: CSV
# Statut de la profession: Actuel
# Doublons: Sans doublons

professions_fr <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_6.csv"))
# professions_fr_5 <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_5.csv")) |> arrange(Parent)
professions_fr_3 <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_levels_3-6.csv"))

## Transform strings and codes for fuzzy matching ####
professions <- professions_fr |> 
  # Remove military codes that start with 0 (here they are the ones only with 4 digits)
  filter(str_length(Parent)>4) |> 
  mutate(
    ISCO = as.integer(str_sub(Parent, start = 1, end = 4)), # Keep only first 4 digits of ISCO-08 code
    Name_fr = str_to_lower(stri_trans_general(Name_fr, id = "Latin-ASCII")), # Make lowercase and remove accents
  ) |> 
  filter(!str_ends(ISCO, pattern = "00")) |> # remove codes ending in 00 -> we want minimum match to 3-digits
  separate_longer_delim(Name_fr, delim = " | ") |> # Split the masculin / feminin versions into separate rows
  select(-c(Code, Parent)) |> 
  # Manually add some common abbreviations or other occupations that are not in the file
  add_row(Name_fr = "assc", ISCO = 3221) |> 
  add_row(Name_fr = "a.s.e", ISCO = 5322) |> 
  arrange(Name_fr)


# Read in our dataset ####
# source(here("code","03_participants with profession data.R"))
dat_master_professions_2 <- readRDS(here("output", "dat_master_professions_2.rds"))

## Keep variables for assigning ISCO-08 codes ####
# (key free-text information is sometimes in the master_profession, job_sector.st_22, job_sector_other.st_22 variables)
occup <- dat_master_professions_2 %>% 
  select(master_profession, profession_source, profession.WORK, profession_other.inc, parent1_profession.inc_kids, 
         parent1_occupation_other.inc_kids, profession.st_22,job.st_23, ew_professsion.st_23,
         job_sector.st_22, job_sector_other.st_22, supervision.st_22, job_sector.st_23,
         Occupation.WORK, Sector.WORK,serocov_work.inc,
         participant_id, codbar, date_inclusion:beyond_inclusion, Hug_Date_Derniere_Soumission_C.WORK) %>%
  # Make some changes to the free-text columns to make them easier to work with
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(master_profession = stringi::stri_trans_general(str = master_profession, 
                                                         id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
         master_profession = str_squish(str_to_lower(master_profession)),             # Convert all capital letters to small letters
         
         job_sector_other.st_22 = stringi::stri_trans_general(str = job_sector_other.st_22, 
                                                              id = "Latin-ASCII"),
         job_sector_other.st_22 = str_squish(str_to_lower(job_sector_other.st_22)),
         ISCO = NA                                                               # Empty column that we'll fill in the next steps
  ) %>%
  add_count(master_profession, sort = TRUE) %>% 
  arrange(master_profession, desc(n)) |>
  sample_n(30) |> # Take a random sample of n rows (when trying things out, to save time)
  select(participant_id, master_profession)

a <- fuzzyjoin::stringdist_left_join(
  x = occup,
  y = professions,
  by = c(master_profession = "Name_fr"),
  method = "jw", #use Jaro-Winkler distance metric
  distance_col = "dist",
  max_dist = 0.5 # Set a cutoff for the matches, and any NAs beyond that can be manually classified
) |> 
  group_by(participant_id) |>
  slice_min(order_by=dist, n=5) # Keep the n best matches (least "distance")

# What I want to do now: 
# If the top match has a distance lower than 0.1 (or whatever number to be specified), I want to keep only the top match and remove all secondary matches. Otherwise, I want to keep all matches for downstream visual screening and manual classification.
b <- a |> 
  group_by(participant_id) |> 
  # mutate(distinct = n()) |> # Add a column for number of instances of participant_id
  ungroup() |> 
  # Add a column we can use to screen "low-quality" or no matches
  mutate(
    NA_or_high_distance = case_when(
      is.na(ISCO)| # no match
        # distinct > 1 | # multiple matches
        dist >= 0.1 ~ TRUE, # distance >= specified cutoff
      .default = FALSE)) |> 
  arrange(master_profession, participant_id, dist) |>  # Arrange best match first for each participant_id
  group_by(participant_id) |> 
  # arrange(dist, .by_group = TRUE) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist)), # index of order of top matches by participant_id
    # top_match = duplicated(participant_id) == FALSE,  # specify top match
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  ungroup() |> 
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"


# Merge with ISCO labels file ####
## Read in ISCO labels ####
occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

## Merge ####
occup_final <- a |> 
  left_join(occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession)
