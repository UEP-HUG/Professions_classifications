pacman::p_load(
  tidyverse,
  here,
  data.table,
  fuzzyjoin,
  # fuzzywuzzy, # Alternative package for fuzzy joins
  # stringdist, # This is that package used by the fuzzyjoin package
  stopwords, # package with list of stopwords
  tidytext,
  cld2,   # package to detect the language of text (bayesian based model)
  cld3,   # package to detect the language of text (neural network model)
  fastText,
  stringi
)

pacman::p_load_current_gh("trinker/lemmar") #  install the development version of lemmatization package

# Read in dat_master_professions_2.rds
if (file.exists(here("output", "dat_master_professions_2.rds"))) {
  dat_master_professions_2 <- readRDS(here("output", "dat_master_professions_2.rds"))
} else {
  source(here("code","03_participants with profession data.R"))
} 

# French "stopwords" (common words) list to remove from professions
stopwords_fr <- tibble(word = stopwords("fr")) |> 
  filter(!word %in% c("avions", "son", "non")) |> # remove these ones
  # add_row(word = c("l'", "d'")) |> # add these in
  mutate(word = stri_trans_general(word, id = "Latin-ASCII")) # remove accents

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

# French professions
professions_fr <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_6.csv")) |> 
  filter(str_length(Parent)>4) |> # remove military lines
  # label the language (french or english)
  mutate(
    lang_Name_cld2 = cld2::detect_language(Name_fr),
    lang_Name_cld3 = cld3::detect_language(Name_fr),
    language.Name = case_when(lang_Name_cld2 == "en" & lang_Name_cld3 == "en" ~ "en", .default = "fr")
  ) |>
  select(-lang_Name_cld2, -lang_Name_cld3)

# English professions
professions_en <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_6_en.csv")) |> 
  rename(Name_fr = Name_en) |> 
  mutate(language.Name = "en") |> 
  filter(str_length(Parent)>4) # remove military lines

# Higher level labels - these were used for Sante-Travail 2023
professions_fr_5 <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_5.csv")) |> 
  arrange(Parent) |> 
  filter(
    str_length(Parent)>3, # remove military lines
    # remove codes ending in 00 or 000
    !str_ends(Parent, pattern = "00|000")
  ) |> 
  mutate(
    Name_fr = str_remove(Name_fr, "\\bsip\\b"),
    # Label the language
    lang_Name_cld2 = cld2::detect_language(Name_fr),
    lang_Name_cld3 = cld3::detect_language(Name_fr),
    language.Name = case_when(lang_Name_cld2 == "en" & lang_Name_cld3 == "en" ~ "en", .default = "fr")
  ) |> # remove "sip" from the labels
  select(-lang_Name_cld2, -lang_Name_cld3)

## Transform strings and codes for fuzzy matching ####
professions <- rbind(professions_fr, professions_en, professions_fr_5) |>
  # Remove military codes that start with 0 (here they are the ones only with 4 digits)
  mutate(
    # ISCO = as.integer(str_sub(Parent, start = 1, end = 4)), # Keep only first 4 digits of ISCO-08 code
    ISCO = Parent,
    Name_fr = str_to_lower(stri_trans_general(Name_fr, id = "Latin-ASCII")), # Make lowercase and remove accents
  ) |> 
  select(-c(Code, Parent)) |> 
  # Manually add some common abbreviations or other occupations that are not in the file
  add_row(Name_fr = "assc | apprenti assc | apprentie assc", ISCO = 3221) |> 
  add_row(Name_fr = "a.s.e | ase | apprenti ase | apprentie ase", ISCO = 5322) |> 
  add_row(Name_fr = "administrateur | administratrice", ISCO = 4110) |> 
  add_row(Name_fr = "administratif", ISCO = 4419) |> 
  add_row(Name_fr = "assistant in international organisation", ISCO = 3412) |>
  add_row(Name_fr = "assistante d'ambassadeur", ISCO = 3412) |>
  add_row(Name_fr = "banque", ISCO = 42112) |>
  add_row(Name_fr = "chauffeur bus | chauffeuse bus", ISCO = 83310) |>
  add_row(Name_fr = "agent de surete | agente de surete", ISCO = 33550) |>
  add_row(Name_fr = "asp2 | asp 2", ISCO = 54120) |>
  add_row(Name_fr = "gestionnaire ressources humaines", ISCO = 12120) |>
  add_row(Name_fr = "femme au foyer | mere au foyer", ISCO = -999) |>
  add_row(Name_fr = "petite enfance", ISCO = 2342) |>
  # add_row(Name_fr = "", ISCO = ) |> 
  # add_row(Name_fr = "", ISCO = ) |> 
  # add_row(Name_fr = "", ISCO = ) |> 
  # add_row(Name_fr = "", ISCO = ) |> 
  # add_row(Name_fr = "", ISCO = ) |> 
  separate_longer_delim(Name_fr, delim = " | ") |> # Split the masculin / feminin versions into separate rows
  arrange(Name_fr) |> 
  rowid_to_column()

# Remove common "stopwords"
prof_stop <- professions |> 
  mutate(Name_fr = str_replace(Name_fr, "l'|d'", "")) |> 
  unnest_tokens(word, Name_fr) |>
  anti_join(stopwords_fr) |> 
  mutate(Name_fr_lem = case_when(language.Name =="en" ~ lemmatize_words_fr(word),
                                 .default = lemmatize_words_fr(word))) |>
  group_by(rowid) |> 
  mutate(Name_fr_2 = paste(word, collapse = " "),
         Name_fr_2_lem = paste(Name_fr_lem, collapse = " ")) |> 
  ungroup() |> 
  select(-word, -Name_fr_lem) |> 
  distinct()

# Merge back in with the professions object
professions <- left_join(professions, prof_stop) |> select(-language.Name)

rm(prof_stop, professions_fr, professions_en, professions_fr_5) # remove intermediate objects


# Clean up profession entries for matching ####
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
  mutate(
    lang_Name_cld2 = cld2::detect_language(master_profession),
    lang_Name_cld3 = cld3::detect_language(master_profession),
    language.occup = case_when(lang_Name_cld2 == "en" & lang_Name_cld3 == "en" ~ "en", .default = "fr"),
    master_profession = stringi::stri_trans_general(str = master_profession, 
                                                    id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
    master_profession = str_squish(str_to_lower(master_profession)),             # Convert all capital letters to small letters
    
    job_sector_other.st_22 = stringi::stri_trans_general(str = job_sector_other.st_22, 
                                                         id = "Latin-ASCII"),
    job_sector_other.st_22 = str_squish(str_to_lower(job_sector_other.st_22)),
    ISCO = NA                                                               # Empty column that we'll fill in the next steps
  ) %>%
  mutate(
    #
    master_profession_original = master_profession, # keep original separately
    master_profession = str_replace_all(master_profession, "\\.", ""),
    master_profession = str_replace(master_profession, "l'|d'", ""),  # remove the l' and d'
    # use "\\b" on each side of a string to indicate that the match should be on a whole word
    master_profession = str_replace(master_profession, "r&d", "recherche et developpement"),
    master_profession = str_replace(master_profession, "\\brh\\b", "ressources humaines"),
    master_profession = str_replace(master_profession, "\\bsup\\b", "superieur"),
    master_profession = str_replace(master_profession, "\\bhr\\b", "human resources"),
    master_profession = str_replace(master_profession, "\\badmin\\b", "administrative"),
    master_profession = str_replace(master_profession, "\\bonu\\b|\\boms\\b|\\bunited nations\\b|\\bong\\b",
                                    "organisation international"),
    master_profession = str_replace(master_profession, "infitmier", "infirmier"),
    master_profession = str_replace(master_profession, "couffeuse", "coiffeuse"),
    master_profession = str_replace(master_profession, "projer", "projet"),
    master_profession = str_replace(master_profession, "consierge", "concierge"),
    master_profession = str_replace(master_profession, "coseiller", "conseiller"),
    master_profession = str_replace(master_profession, "medevin", "medecin"),
    master_profession = str_replace(master_profession, "\\bprof\\b", "professeur"),
    master_profession = str_replace(master_profession, "\\bresp\\b|responsabke", "responsable"),
    master_profession = str_replace(master_profession, "tpg", "bus"),
    master_profession = str_replace(master_profession, "\\bems\\b", "etablissement medico social"),
    master_profession = str_replace(master_profession, "\\bhets\\b", "travail social"),
    # # master_profession = str_replace(master_profession, "", ""),
    # # master_profession = str_replace(master_profession, "", ""),
    # # master_profession = str_replace(master_profession, "", ""),
    # # master_profession = str_replace(master_profession, "", ""),
    
    # resquish in case spaces were introduced
    master_profession = str_squish(master_profession)
  ) |> 
  add_count(master_profession, sort = TRUE) %>% 
  arrange(master_profession, desc(n)) |>
  # sample_n(100) |> # Take a random sample of n rows (when trying things out, to save time)
  select(participant_id, master_profession_original, master_profession, profession_source, language.occup) |> 
  # Remove stopwords (trial)
  unnest_tokens(word, master_profession) |> 
  anti_join(stopwords_fr) |> 
  mutate(master_profession_lem = case_when(language.occup == "en" ~ lemmatize_words_en(word), 
                                           .default = lemmatize_words_fr(word))) |>
  group_by(participant_id) |>
  mutate(master_profession = paste(word, collapse = " "),
         master_profession_lem = paste(master_profession_lem, collapse = " ")) |>
  ungroup() |>
  select(-word, -language.occup) |> 
  #keep only distinct rows
  distinct() #|> 
# pivot_longer(cols = !participant_id, values_to = "profession", names_to = "source") |> 
# arrange(participant_id, profession, source)


# Jaro-Winkler fuzzy matching ####
start <- Sys.time() ; matches_jw <- fuzzyjoin::stringdist_left_join(
  x = occup,
  y = professions,
  by = c(master_profession = "Name_fr_2"), # Match against the stopwords removed versions
  method = "jw", #use Jaro-Winkler distance metric
  distance_col = "dist_jw",
  max_dist = 0.3 # Set a cutoff for the matches
) |>
  # Keep only the top matches
  group_by(participant_id) |>
  slice_min(order_by=dist_jw, n=5);end <- Sys.time() ; print(end-start)

# If the top match has a distance lower than 0.1 (or whatever number to be specified), I want to keep only the top match and remove all secondary matches. Otherwise, I want to keep all matches for downstream visual screening and manual classification.
cleaner_matches_jw <- matches_jw |> 
  # Add a column we can use to screen "low-quality" or no matches
  mutate(
    dist_jaccard = NA,
    NA_or_high_distance = case_when(
      is.na(ISCO)| # no match
        dist_jw >= 0.06 ~ TRUE, # distance >= specified cutoff with Jaro-Winkler distance
      .default = FALSE)) |> 
  arrange(master_profession, participant_id, dist_jw) |>  # Arrange best match first for each participant_id
  group_by(participant_id) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist_jw)), # index of order of top matches by participant_id
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  ungroup() |> 
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"

## Get the good /bad matches ####
good_matches_jw <- cleaner_matches_jw |> filter(group_top_match == "Good")
bad_matches_jw <- cleaner_matches_jw |> filter(group_top_match == "Not good")

# Jaccard fuzzy matching ####
## Initialize dataset from bad_matches ####
matches_jaccard_prep <- bad_matches_jw |> 
  select(-rowid, -NA_or_high_distance, - group_top_match) |>  # change dist to dist_jw
  rename(Name_fr_jw = Name_fr, Name_fr_2_jw = Name_fr_2, ISCO_jw = ISCO) |> 
  filter(id_index == 1) |> 
  select(-Name_fr_jw, -Name_fr_2_lem, -id_index, -dist_jaccard)

## Run the Jaccard matching ####
start <- Sys.time() ; print(paste(start, " --> Processing full dataset takes about 25 mins")); matches_jaccard <- fuzzyjoin::stringdist_left_join(
  x = matches_jaccard_prep,
  y = professions,
  by = c(master_profession_lem = "Name_fr_2_lem"), # Match against the stopwords removed versions
  method = "jaccard", q = 3, # q = the size of the q-grams
  distance_col = "dist_jaccard",
  max_dist = 0.7 # Set a cutoff for the matches
) |> 
  group_by(participant_id) |>
  # Keep the n best matches (least "distance")
  slice_min(order_by=dist_jaccard, n=5) ;end <- Sys.time() ; print(end-start)

## Clean up matches ####
cleaner_matches_jaccard <- matches_jaccard |> 
  # Add a column we can use to screen "low-quality" or no matches
  mutate(
    NA_or_high_distance = case_when(
      is.na(ISCO)| # no match
        dist_jaccard >= 0.35 ~ TRUE, # distance >= specified cutoff with Jaccard distance
      .default = FALSE)) |> 
  arrange(master_profession, participant_id, dist_jaccard) |>  # Arrange best match first for each participant_id
  group_by(participant_id) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist_jaccard)), # index of order of top matches by participant_id
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  ungroup() |> 
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"

## Separate to good / bad matches ####
good_matches_jaccard <- cleaner_matches_jaccard |> filter(group_top_match == "Good")
bad_matches_jaccard <- cleaner_matches_jaccard |> filter(group_top_match == "Not good")

## Get the ones that have a common match for JW and Jaccard ####
better_bad_matches_jaccard <- bad_matches_jaccard |> 
  mutate(similar_ISCO = str_sub(ISCO_jw, 1,3) == str_sub(ISCO, 1,3)) |> 
  filter(similar_ISCO) |> 
  group_by(participant_id) |> 
  slice_min(id_index) |> 
  arrange(master_profession)
remaining_bad_matches <- anti_join(bad_matches_jaccard, better_bad_matches_jaccard, by = "participant_id") |> select(-rowid) |> 
  relocate(c(master_profession, Name_fr, ISCO, Name_fr_2_jw, ISCO_jw, dist_jaccard, dist_jw), .after = master_profession_original)


# Put together the results from the different matching strategies ####
good_matches_jw_clean <- good_matches_jw |> select(participant_id, Name_fr, ISCO, dist_jw, dist_jaccard) |> 
  mutate(confidence = "High")
good_matches_jaccard_clean <- good_matches_jaccard |> select(participant_id, Name_fr, ISCO, dist_jw, dist_jaccard) |> 
  mutate(confidence = "High")
better_bad_matches_jaccard_clean <- better_bad_matches_jaccard |> select(participant_id, Name_fr, ISCO, dist_jw, dist_jaccard) |> 
  mutate(confidence = "Low")

## Combine good matches ####
all_good_matches <- rbind(good_matches_jw_clean, good_matches_jaccard_clean, better_bad_matches_jaccard_clean)

## Merge with occup file ####
occup_matched <- left_join(occup, all_good_matches, by = "participant_id") |> 
  mutate(
    ISCO_full = ISCO,
    ISCO = as.integer(str_sub(ISCO, start = 1, end = 4))
  )

# Merge with ISCO labels file ####
## Read in ISCO labels ####
occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

## Merge ####
occup_final <- occup_matched |> 
  left_join(occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession) |> 
  select(participant_id, profession_source, master_profession_original, Name_fr, Occupation_label, dist_jw, dist_jaccard, confidence, ISCO, ISCO_full) |> 
  mutate(confidence = case_when(dist_jaccard > 0.2 & confidence == "High" ~ "Medium", .default = confidence)) |> 
  distinct() |> 
  arrange(master_profession_original)

saveRDS(occup_final, file = here("output", "fuzzy_classified_occupations_to_clean.rds"))
saveRDS(remaining_bad_matches, file = here("output", "remaining_bad_matches.rds"))
