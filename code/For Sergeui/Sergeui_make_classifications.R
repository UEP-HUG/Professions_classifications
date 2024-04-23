# For Sergeui V5 recoding

pacman::p_load(
  tidyverse,
  here,
  readxl,
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


# Read in datasets ####

## Inclusion ####
inclusion <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  # # Filter for only participants in one of the relevant studies:
  # filter(serocov_pop | pop_pilote | serocov_schools | serocov_kids | serocov_work | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |>
  filter(!testeur) |>  # remove data produced by testers - I think Nick already did this
  filter(!str_starts(codbar, "T"))  |>  # Remove any people with codbar beginning with T (also testers)
  mutate(
    date_soumission = as_date(date_soumission),
    age = time_length(date_soumission - birthdate, "years")) #|> 
  # Slice the dataset to only keep the latest entry in case of double entries for a participant (so that V5 submissions are kept)
  # group_by(participant_id) |> slice_max(order_by = date_soumission, n = 1) |> ungroup()
  
inc_v5_Other <- inclusion |> 
  filter(Origin == "V5_inclusion") |> 
  filter(!is.na(profession_other))


# Read in stopwords and ISCO labels ####
## French "stopwords" (common words) list to remove from professions ####
stopwords_fr <- tibble(word = stopwords("fr")) |> 
  filter(!word %in% c("avions", "son", "non")) |> # remove these ones
  # add_row(word = c("l'", "d'")) |> # add these in
  mutate(word = stri_trans_general(word, id = "Latin-ASCII")) # remove accents

## French professions ####
professions_fr <- fread(here("data", "HCL_CH_ISCO_19_PROF_1_2_level_6.csv")) |> 
  filter(str_length(Parent)>4) |> # remove military lines
  # label the language (french or english)
  mutate(
    lang_Name_cld2 = cld2::detect_language(Name_fr),
    lang_Name_cld3 = cld3::detect_language(Name_fr),
    language.Name = case_when(lang_Name_cld2 == "en" & lang_Name_cld3 == "en" ~ "en", .default = "fr")
  ) |>
  select(-lang_Name_cld2, -lang_Name_cld3)

## English professions ####
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
professions <- left_join(professions, prof_stop) |> select(-language.Name, -rowid)

rm(prof_stop, professions_fr, professions_en, professions_fr_5) # remove intermediate objects


# Clean up profession entries for matching ####
occup <- inc_v5_Other %>%
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(
    lang_Name_cld2 = cld2::detect_language(profession_other),
    lang_Name_cld3 = cld3::detect_language(profession_other),
    language.occup = case_when(lang_Name_cld2 == "en" & lang_Name_cld3 == "en" ~ "en", .default = "fr"),
    profession_other = stringi::stri_trans_general(str = profession_other, 
                                                    id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
    profession_other = str_squish(str_to_lower(profession_other))      # Convert all capital letters to small letters
  ) %>%
  mutate(
    #
    profession_other_original = profession_other, # keep original separately
    profession_other = str_replace_all(profession_other, "\\.", ""),
    profession_other = str_replace(profession_other, "l'|d'", ""),  # remove the l' and d'
    # use "\\b" on each side of a string to indicate that the match should be on a whole word
    profession_other = str_replace(profession_other, "r&d", "recherche et developpement"),
    profession_other = str_replace(profession_other, "\\brh\\b", "ressources humaines"),
    profession_other = str_replace(profession_other, "\\bsup\\b", "superieur"),
    profession_other = str_replace(profession_other, "\\bhr\\b", "human resources"),
    profession_other = str_replace(profession_other, "\\badmin\\b", "administrative"),
    profession_other = str_replace(profession_other, "\\bonu\\b|\\boms\\b|\\bunited nations\\b|\\bong\\b",
                                    "organisation international"),
    profession_other = str_replace(profession_other, "infitmier", "infirmier"),
    profession_other = str_replace(profession_other, "couffeuse", "coiffeuse"),
    profession_other = str_replace(profession_other, "projer", "projet"),
    profession_other = str_replace(profession_other, "consierge", "concierge"),
    profession_other = str_replace(profession_other, "coseiller", "conseiller"),
    profession_other = str_replace(profession_other, "medevin", "medecin"),
    profession_other = str_replace(profession_other, "\\bprof\\b", "professeur"),
    profession_other = str_replace(profession_other, "\\bresp\\b|responsabke", "responsable"),
    profession_other = str_replace(profession_other, "tpg", "bus"),
    profession_other = str_replace(profession_other, "\\bems\\b", "etablissement medico social"),
    profession_other = str_replace(profession_other, "\\bhets\\b", "travail social"),
    # profession_other = str_replace(profession_other, "", ""),
    # profession_other = str_replace(profession_other, "", ""),
    # profession_other = str_replace(profession_other, "", ""),
    # profession_other = str_replace(profession_other, "", ""),
    
    # resquish in case spaces were introduced
    profession_other = str_squish(profession_other)
  ) |> 
  add_count(profession_other, sort = TRUE) %>% 
  arrange(profession_other, desc(n)) |>
  # sample_n(500) |> # Take a random sample of n rows (when trying things out, to save time)
  select(participant_id, codbar, profession_other_original, profession_other, language.occup, date_soumission) |> 
  # Remove stopwords (trial)
  unnest_tokens(word, profession_other) |> 
  anti_join(stopwords_fr) |> 
  mutate(profession_other_lem = case_when(language.occup == "en" ~ lemmatize_words_en(word), 
                                           .default = lemmatize_words_fr(word))) |>
  group_by(participant_id) |>
  mutate(profession_other = paste(word, collapse = " "),
         profession_other_lem = paste(profession_other_lem, collapse = " ")) |>
  ungroup() |>
  select(-word, -language.occup) |> 
  #keep only distinct rows
  distinct()


# Jaro-Winkler fuzzy matching ####
start <- Sys.time() ; matches_jw <- fuzzyjoin::stringdist_left_join(
  x = occup,
  y = professions,
  by = c(profession_other = "Name_fr_2"), # Match against the stopwords removed versions
  method = "jw", #use Jaro-Winkler distance metric
  distance_col = "dist_jw",
  max_dist = 0.3 # Set a cutoff for the matches
) |>
  # Keep only the top matches
  group_by(participant_id) |>
  slice_min(order_by=dist_jw, n=5) |> 
  relocate(profession_other_lem, .after = last_col()) ; end <- Sys.time() ; print(end-start)

# If the top match has a distance lower than 0.1 (or whatever number to be specified), I want to keep only the top match and remove all secondary matches. Otherwise, I want to keep all matches for downstream visual screening and manual classification.
cleaner_matches_jw <- matches_jw |> 
  # Add a column we can use to screen "low-quality" or no matches
  mutate(
    dist_jaccard = NA,
    NA_or_high_distance = case_when(
      is.na(ISCO)| # no match
        dist_jw >= 0.06 ~ TRUE, # distance >= specified cutoff with Jaro-Winkler distance
      .default = FALSE)) |> 
  group_by(participant_id) |> 
  arrange(participant_id, dist_jw) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist_jw)), # index of order of top matches by participant_id
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  arrange(participant_id, id_index) |>  # Arrange best match first for each participant_id
  rowid_to_column() |>
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  ungroup() |> 
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"

## Get the good /bad matches ####
good_matches_jw <- cleaner_matches_jw |> filter(group_top_match == "Good") |> 
  group_by(participant_id) |> slice_min(order_by = rowid, n = 1)
bad_matches_jw <- cleaner_matches_jw |> filter(group_top_match == "Not good")

# Jaccard fuzzy matching ####
## Initialize dataset from bad_matches ####
matches_jaccard_prep <- bad_matches_jw |> 
  select(-NA_or_high_distance, - group_top_match) |>  # change dist to dist_jw
  rename(Name_fr_jw = Name_fr, Name_fr_2_jw = Name_fr_2, ISCO_jw = ISCO) |>
  group_by(participant_id) |> slice_min(order_by = rowid, n = 1) |> # keep only the top entry
  select(-Name_fr_jw, -Name_fr_2_lem, -id_index, -dist_jaccard, -rowid)

## Run the Jaccard matching ####
start <- Sys.time() ; print(paste(start, " --> Processing full dataset takes about 30 mins")); matches_jaccard <- fuzzyjoin::stringdist_left_join(
  x = matches_jaccard_prep,
  y = professions,
  by = c(profession_other = "Name_fr_2"), # Match against the stopwords removed versions
  method = "jaccard", q = 3, # q = the size of the q-grams
  distance_col = "dist_jaccard",
  max_dist = 0.8 # Set a cutoff for the matches
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
  group_by(participant_id) |> 
  arrange(participant_id, dist_jaccard) |> 
  mutate(
    id_index = as.numeric(fct_reorder(factor(Name_fr), dist_jaccard)), # index of order of top matches by participant_id
    # For matches within a participant_id, specify whether or not the top match is good
    group_top_match = case_when(
      id_index == 1 & NA_or_high_distance == FALSE ~ "Good",
      id_index == 1 & NA_or_high_distance == TRUE ~ "Not good",
      .default = NA)) |> 
  arrange(participant_id, id_index) |>  # Arrange best match first for each participant_id
  rowid_to_column() |>
  tidyr::fill(group_top_match, .direction = "down") |> # Fill the variable down to the whole group
  ungroup() |> 
  filter(!(group_top_match == "Good" & id_index != 1)) # Remove secondary matches when a top match is "Good"

## Separate to good / bad matches ####
good_matches_jaccard <- cleaner_matches_jaccard |> filter(group_top_match == "Good") |> 
  group_by(participant_id) |> slice_min(order_by = rowid, n = 1)
bad_matches_jaccard <- cleaner_matches_jaccard |> filter(group_top_match == "Not good")

## Get the ones that have a common match for JW and Jaccard ####
better_bad_matches_jaccard <- bad_matches_jaccard |> 
  mutate(similar_ISCO = str_sub(ISCO_jw, 1,3) == str_sub(ISCO, 1,3)) |> 
  filter(similar_ISCO) |> 
  group_by(participant_id) |> 
  slice_min(rowid) |> 
  arrange(profession_other) |> 
  ungroup()

remaining_bad_matches <- anti_join(bad_matches_jaccard, better_bad_matches_jaccard, by = join_by("participant_id" == "participant_id")) |> select(-rowid) |> 
  relocate(c(profession_other, Name_fr, ISCO, Name_fr_2_jw, ISCO_jw, dist_jaccard, dist_jw), .after = profession_other_original)


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
occup_matched <- left_join(occup, all_good_matches, by = join_by("participant_id" == "participant_id")) |> 
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
  relocate(Occupation_label, .after = profession_other) |> 
  select(participant_id, date_soumission, profession_other_original, Name_fr, Occupation_label, dist_jw, dist_jaccard, confidence, ISCO, ISCO_full) |> 
  mutate(confidence = case_when(dist_jaccard > 0.2 & confidence == "High" ~ "Medium", .default = confidence)) |> 
  distinct() |> 
  arrange(profession_other_original) |> 
  ungroup()

saveRDS(occup_final, file = here("code", "For Sergeui", "Sergeui_classified_occupations_to_clean.rds"))
saveRDS(remaining_bad_matches, file = here("code", "For Sergeui", "Sergeui_remaining_bad_matches_long.rds"))