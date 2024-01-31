pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  janitor,      
  readxl,
  stringi       # String manipulation
)

source(here("code","participants with profession data.R"))

occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

# Keep the variables that can be helpful for choosing how to assign ISCO-08 codes
# (key free-text information is sometimes in the master_profession, job_sector.st_22, job_sector_other.st_22 variables)
occup <- dat_master_professions_2 %>% 
  select(master_profession, profession_source, profession_other.inc, parent1_profession.inc_kids, 
         parent1_occupation_other.inc_kids, profession.st_22,job.st_23, ew_professsion.st_23,
         job_sector.st_22, supervision.st_22,
         job_sector_other.st_22,
         codbar) %>%
  # Make some changes to the free-text columns to make them easier to work with
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(master_profession = stringi::stri_trans_general(str = master_profession, 
                                                         id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
         master_profession = str_trim(str_to_lower(master_profession)),             # Convert all capital letters to small letters
         
         job_sector_other.st_22 = stringi::stri_trans_general(str = job_sector_other.st_22, 
                                                              id = "Latin-ASCII"),
         job_sector_other.st_22 = str_trim(str_to_lower(job_sector_other.st_22)),
         ISCO = NA                                                               # Empty column that we'll fill in the next steps
  ) %>%
  add_count(master_profession, sort = TRUE) %>% 
  arrange(master_profession, desc(n)) |> 
  relocate(ISCO, .after = master_profession)


# Update the dataset to include the ISCO-08 codes and occupation labels, using the below definitions:
occup_ISCO <- occup %>% 
  mutate(ISCO = case_when(
    # Nurses
    is.na(ISCO) & str_detect(master_profession, "infi|sage femme|sage-femme") ~ 222,
    # Assistants to doctors / dentists / etc.
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "assistant") &
      str_detect(master_profession, "medic|dent|soins|soci|medec") ~ 325,
    # Doctors
    is.na(ISCO) & str_detect(master_profession, "medecin") ~ 221,

    
    .default = ISCO
  )) %>% 
  left_join(., occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession) %>% 
  select(-n) %>% 
  filter(
    !is.na(master_profession),
    !is.na(Occupation_label), 
    # !ISCO %in% c(9999)
  ) %>%      # Remove rows with unassigned ISCO codes, to work on what's left
  mutate(ISCO = case_when(is.na(ISCO) ~ -999, 
                          .default = ISCO)
  ) %>% 
  add_count(
    # job_sector.st_22,
    master_profession,
    sort = FALSE) %>% 
  arrange( 
          master_profession,
          desc(n)
          # job_sector.st_22
          ) #%>% 
# select(master_profession, master_profession, master_profession, ISCO, job_sector.st_22, supervision.st_22, n)  # remove this line once you've got it all done

# Indices ####

# # Read teleworkability indices from locally saved file
indices <- read_csv(here("data", "Telework ISCO indices.txt")) %>%     # read in the teleworkability indices (low "physical_interaction" = low teleworkability)
  janitor::clean_names() %>% select(!occupation_title) %>% rename(isco_3 = isco08)
# Read teleworkability indices from original source
# indices <- read_csv("https://raw.githubusercontent.com/m-sostero/telework-occupations/master/Telework%20ISCO%20indices.csv") %>%     # read in directly from source GitHub page
#   janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# Read in ISCO code occupation labels
indices_2 <- read_xlsx(here("data", "EWCS 3 digit.xlsx")) %>% mutate(isco_3 = as.numeric(isco3d)) %>% select(isco_3,telework, physicalb)
# Merge the indices with the ISCO labels
indices <- left_join(indices, indices_2)

# Finalizing the file ####
# In the final file, keep only the relevant columns that will be merged with our full dataset
occup_ISCO_final <- occup_ISCO %>% select(codbar, master_profession, ISCO) |> filter(ISCO != -999)

# Read in the classified occupations data
# (generated from "01_prep_a_classify occupations.R")
occup_ISCO_final <- occup_ISCO_final %>% mutate(
  isco_full = ISCO,
  isco_3 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 3))),
  isco_2 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 2))),
  isco_1 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 1)))
) %>% 
  select(-c(ISCO)) %>% # Remove the extra columns
  left_join(indices) # Merge with the indices dataframe

# Label the occupations from the ISCO classifications for each code level (from 4 = "full" down to level 1)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_full" = "ISCO")) %>% rename(Occupation_label_full = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_3" = "ISCO")) %>% rename(Occupation_label_3 = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_2" = "ISCO")) %>% rename(Occupation_label_2 = Occupation_label)
occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_1" = "ISCO")) %>% rename(Occupation_label_1 = Occupation_label)

occup_ISCO_final <- occup_ISCO_final |> 
  relocate(Occupation_label_full:Occupation_label_1, .after = master_profession)

# # Save the final dataset
saveRDS(occup_ISCO_final, here("data", "Classified_occupations.RDS"), ascii = TRUE)