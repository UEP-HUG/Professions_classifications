pacman::p_load(
  tidyverse,
  here,
  data.table,
  fuzzyjoin,
  # fuzzywuzzy,
  # stringdist,
  stopwords, # package with list of stopwords
  tidytext,
  stringi
)

# French "stopwords" (common words) list to remove from professions
stopwords_fr <- tibble(word = stopwords("fr")) |> 
  filter(word != c("avions", "son")) |> 
  add_row(word = c("l'", "d'")) |> 
  mutate(word = stri_trans_general(word, id = "Latin-ASCII"))

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
# professions_fr_3 <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_levels_3-6.csv"))

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
  add_row(Name_fr = "ase", ISCO = 5322) |> 
  arrange(Name_fr) |> 
  rowid_to_column()

# Remove common "stopwords"
prof_stop <- professions |> 
  unnest_tokens(word, Name_fr) |> 
  anti_join(stopwords_fr) |> 
  group_by(rowid) |> 
  mutate(Name_fr_2 = paste(word, collapse = " ")) |> 
  ungroup() |> 
  select(-word) |> 
  distinct()

professions <- left_join(professions, prof_stop)


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
  mutate(
    master_profession = str_replace(master_profession, " rh| rh", "ressources humaines"),
    master_profession = str_replace(master_profession, "l'onu|l'oms", "organisation international"),
    master_profession = str_replace(master_profession, "infitmier", "infirmier"),
    master_profession = str_replace(master_profession, "couffeuse", "coiffeuse"),
    master_profession = str_replace(master_profession, "projer", "projet"),
    master_profession = str_replace(master_profession, "consierge", "concierge"),
    master_profession = str_replace(master_profession, "coseiller", "conseiller"),
    master_profession = str_replace(master_profession, "medevin", "medecin"),
    master_profession = str_replace(master_profession, "prof |prof.", "professeur"),
    master_profession = str_replace(master_profession, "resp.|responsabke", "responsable"),
    master_profession = str_replace(master_profession, "tpg", "bus"),
    # master_profession = str_replace(master_profession, "", ""),
    # master_profession = str_replace(master_profession, "", ""),
    # master_profession = str_replace(master_profession, "", ""),
    # master_profession = str_replace(master_profession, "", "")
    ) |> 
  add_count(master_profession, sort = TRUE) %>% 
  arrange(master_profession, desc(n)) |>
  # sample_n(30) |> # Take a random sample of n rows (when trying things out, to save time)
  select(participant_id, master_profession) |> 
# Remove stopwords (trial)
  mutate(master_profession_short = master_profession) |> 
  unnest_tokens(word, master_profession_short) |> 
  anti_join(stopwords_fr) |> 
  group_by(participant_id) |>
  mutate(master_profession_short = paste(word, collapse = " ")) |>
  ungroup() |>
  select(-word) |> 
  #keep only distinct rows
  distinct() #|> 
  # pivot_longer(cols = !participant_id, values_to = "profession", names_to = "source") |> 
  # arrange(participant_id, profession, source)


# Jaccard fuzzy matching ####
matches_jaccard <- fuzzyjoin::stringdist_left_join(
  x = occup,
  y = professions,
  by = c(master_profession_short = "Name_fr_2"), # Match against the stopwords removed versions
  method = "jaccard", q = 3, # q = the size of the q-grams
  # method = "jw", #use Jaro-Winkler distance metric
  # method = "osa", #use Optimal String Alignment distance metric
  distance_col = "dist_jaccard",
  max_dist = 0.7 # Set a cutoff for the matches, Jaro-Winkler / Jaccard
  # max_dist = 4 # Set a cutoff for the matches, OSA
) |> 
  group_by(
    participant_id
    # ,profession
    ) |>
  slice_min(order_by=dist_jaccard, n=5) # Keep the n best matches (least "distance")


# If the top match has a distance lower than 0.1 (or whatever number to be specified), I want to keep only the top match and remove all secondary matches. Otherwise, I want to keep all matches for downstream visual screening and manual classification.
cleaner_matches_jaccard <- matches_jaccard |> 
  # group_by(participant_id, word) |> 
  # mutate(distinct = n()) |> # Add a column for number of instances of participant_id
  # ungroup() |> 
  # Add a column we can use to screen "low-quality" or no matches
  mutate(
    NA_or_high_distance = case_when(
      is.na(ISCO)| # no match
        # distinct > 1 | # multiple matches
        # dist_jaccard >= 0.1 ~ TRUE, # distance >= specified cutoff with Jaro-Winkler distance
        dist_jaccard >= 0.51 ~ TRUE, # distance >= specified cutoff with Jaccard distance
        # dist_jaccard >= 3 ~ TRUE, # distance >= specified cutoff with OSA distance
      .default = FALSE)) |> 
  arrange(master_profession, participant_id, 
          # word, 
          dist_jaccard) |>  # Arrange best match first for each participant_id
  group_by(participant_id
           # , word
           ) |> 
  # arrange(dist_jaccard, .by_group = TRUE) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist_jaccard)), # index of order of top matches by participant_id
    # top_match = duplicated(participant_id) == FALSE,  # specify top match
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  ungroup() |> 
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"

# saveRDS(matches_jaccard, file = paste0(here("output"), "/", "fuzzy_matched_occupations_a.rds"))
# saveRDS(cleaner_matches_jaccard, file = paste0(here("output"), "/", "fuzzy_matched_occupations_cleaned_b.rds"))

# cleaner_matches_jaccard |> filter(group_top_match == "Not good") |> count(participant_id)

good_matches <- cleaner_matches_jaccard |> filter(group_top_match == "Good")
bad_matches <- cleaner_matches_jaccard |> filter(group_top_match == "Not good")

# Jaro-Winkler fuzzy matching ####
## Initialize dataset from bad_matches ####
matches_jw_prep <- bad_matches |> 
  select(-rowid, -NA_or_high_distance, - group_top_match) |>  # change dist to dist_jw
  rename(Name_fr_jaccard = Name_fr, Name_fr_2_jaccard = Name_fr_2, ISCO_jaccard = ISCO) |> 
  filter(id_index == 1) |> 
  select(-Name_fr_jaccard, -id_index)

## Run the JW matching ####
matches_jw <- fuzzyjoin::stringdist_left_join(
  x = matches_jw_prep,
  y = professions,
  by = c(master_profession_short = "Name_fr_2"), # Match against the stopwords removed versions
  # method = "jaccard", q = 3, # q = the size of the q-grams
  method = "jw", #use Jaro-Winkler distance metric
  # method = "osa", #use Optimal String Alignment distance metric
  distance_col = "dist_jw",
  max_dist = 0.3 # Set a cutoff for the matches, Jaro-Winkler / Jaccard
  # max_dist = 4 # Set a cutoff for the matches, OSA
) |> 
  group_by(
    participant_id
    # ,profession
  ) |>
  slice_min(order_by=dist_jw, n=5) # Keep the n best matches (least "distance")
  



# Merge with ISCO labels file ####
## Read in ISCO labels ####
occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

## Merge ####
occup_final <- good_matches |> 
  left_join(occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession)
