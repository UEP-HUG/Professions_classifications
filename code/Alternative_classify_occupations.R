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
    is.na(ISCO) & master_profession %in% c("") ~ 111,
    is.na(ISCO) & master_profession %in% c("") ~ 112,
    is.na(ISCO) & master_profession %in% c("") ~ 121,
    is.na(ISCO) & master_profession %in% c("") ~ 122,
    is.na(ISCO) & master_profession %in% c("") ~ 131,
    is.na(ISCO) & master_profession %in% c("") ~ 132,
    is.na(ISCO) & master_profession %in% c("") ~ 133,
    is.na(ISCO) & master_profession %in% c("") ~ 134,
    is.na(ISCO) & master_profession %in% c("") ~ 141,
    is.na(ISCO) & master_profession %in% c("") ~ 142,
    is.na(ISCO) & master_profession %in% c("") ~ 143,
    is.na(ISCO) & master_profession %in% c("") ~ 211,
    is.na(ISCO) & master_profession %in% c("") ~ 212,
    is.na(ISCO) & master_profession %in% c("") ~ 213,
    is.na(ISCO) & master_profession %in% c("") ~ 214,
    is.na(ISCO) & master_profession %in% c("") ~ 215,
    is.na(ISCO) & master_profession %in% c("") ~ 216,
    is.na(ISCO) & master_profession %in% c("medecin", "chirurgien", "chirurgienne") ~ 221,
    is.na(ISCO) & master_profession %in% c("infirmier", "infirmiere") ~ 222,
    is.na(ISCO) & master_profession %in% c("") ~ 223,
    is.na(ISCO) & master_profession %in% c("") ~ 224,
    is.na(ISCO) & master_profession %in% c("") ~ 225,
    is.na(ISCO) & master_profession %in% c("") ~ 226,
    is.na(ISCO) & master_profession %in% c("") ~ 231,
    is.na(ISCO) & master_profession %in% c("") ~ 232,
    is.na(ISCO) & master_profession %in% c("") ~ 233,
    is.na(ISCO) & master_profession %in% c("") ~ 234,
    is.na(ISCO) & master_profession %in% c("") ~ 235,
    is.na(ISCO) & master_profession %in% c("") ~ 241,
    is.na(ISCO) & master_profession %in% c("") ~ 242,
    is.na(ISCO) & master_profession %in% c("") ~ 243,
    is.na(ISCO) & master_profession %in% c("") ~ 251,
    is.na(ISCO) & master_profession %in% c("") ~ 252,
    is.na(ISCO) & master_profession %in% c("") ~ 261,
    is.na(ISCO) & master_profession %in% c("") ~ 262,
    is.na(ISCO) & master_profession %in% c("") ~ 263,
    is.na(ISCO) & master_profession %in% c("") ~ 264,
    is.na(ISCO) & master_profession %in% c("") ~ 265,
    is.na(ISCO) & master_profession %in% c("") ~ 311,
    is.na(ISCO) & master_profession %in% c("") ~ 312,
    is.na(ISCO) & master_profession %in% c("") ~ 313,
    is.na(ISCO) & master_profession %in% c("") ~ 314,
    is.na(ISCO) & master_profession %in% c("") ~ 315,
    is.na(ISCO) & master_profession %in% c("") ~ 321,
    is.na(ISCO) & master_profession %in% c("") ~ 322,
    is.na(ISCO) & master_profession %in% c("") ~ 323,
    is.na(ISCO) & master_profession %in% c("") ~ 324,
    is.na(ISCO) & master_profession %in% c("") ~ 325,
    is.na(ISCO) & master_profession %in% c("") ~ 331,
    is.na(ISCO) & master_profession %in% c("") ~ 332,
    is.na(ISCO) & master_profession %in% c("") ~ 333,
    is.na(ISCO) & master_profession %in% c("") ~ 334,
    is.na(ISCO) & master_profession %in% c("") ~ 335,
    is.na(ISCO) & master_profession %in% c("") ~ 341,
    is.na(ISCO) & master_profession %in% c("") ~ 342,
    is.na(ISCO) & master_profession %in% c("") ~ 343,
    is.na(ISCO) & master_profession %in% c("") ~ 351,
    is.na(ISCO) & master_profession %in% c("") ~ 352,
    is.na(ISCO) & master_profession %in% c("") ~ 411,
    is.na(ISCO) & master_profession %in% c("") ~ 412,
    is.na(ISCO) & master_profession %in% c("") ~ 413,
    is.na(ISCO) & master_profession %in% c("") ~ 421,
    is.na(ISCO) & master_profession %in% c("") ~ 422,
    is.na(ISCO) & master_profession %in% c("") ~ 431,
    is.na(ISCO) & master_profession %in% c("") ~ 432,
    is.na(ISCO) & master_profession %in% c("") ~ 441,
    is.na(ISCO) & master_profession %in% c("") ~ 511,
    is.na(ISCO) & master_profession %in% c("") ~ 512,
    is.na(ISCO) & master_profession %in% c("") ~ 513,
    is.na(ISCO) & master_profession %in% c("") ~ 514,
    is.na(ISCO) & master_profession %in% c("") ~ 515,
    is.na(ISCO) & master_profession %in% c("") ~ 516,
    is.na(ISCO) & master_profession %in% c("") ~ 521,
    is.na(ISCO) & master_profession %in% c("") ~ 522,
    is.na(ISCO) & master_profession %in% c("") ~ 523,
    is.na(ISCO) & master_profession %in% c("") ~ 524,
    is.na(ISCO) & master_profession %in% c("") ~ 531,
    is.na(ISCO) & master_profession %in% c("") ~ 532,
    is.na(ISCO) & master_profession %in% c("") ~ 541,
    is.na(ISCO) & master_profession %in% c("") ~ 611,
    is.na(ISCO) & master_profession %in% c("") ~ 612,
    is.na(ISCO) & master_profession %in% c("") ~ 613,
    is.na(ISCO) & master_profession %in% c("") ~ 621,
    is.na(ISCO) & master_profession %in% c("") ~ 622,
    is.na(ISCO) & master_profession %in% c("") ~ 631,
    is.na(ISCO) & master_profession %in% c("") ~ 632,
    is.na(ISCO) & master_profession %in% c("") ~ 633,
    is.na(ISCO) & master_profession %in% c("") ~ 634,
    is.na(ISCO) & master_profession %in% c("") ~ 711,
    is.na(ISCO) & master_profession %in% c("") ~ 712,
    is.na(ISCO) & master_profession %in% c("") ~ 713,
    is.na(ISCO) & master_profession %in% c("") ~ 721,
    is.na(ISCO) & master_profession %in% c("") ~ 722,
    is.na(ISCO) & master_profession %in% c("") ~ 723,
    is.na(ISCO) & master_profession %in% c("") ~ 731,
    is.na(ISCO) & master_profession %in% c("") ~ 732,
    is.na(ISCO) & master_profession %in% c("") ~ 741,
    is.na(ISCO) & master_profession %in% c("") ~ 742,
    is.na(ISCO) & master_profession %in% c("") ~ 751,
    is.na(ISCO) & master_profession %in% c("") ~ 752,
    is.na(ISCO) & master_profession %in% c("") ~ 753,
    is.na(ISCO) & master_profession %in% c("") ~ 754,
    is.na(ISCO) & master_profession %in% c("") ~ 811,
    is.na(ISCO) & master_profession %in% c("") ~ 812,
    is.na(ISCO) & master_profession %in% c("") ~ 813,
    is.na(ISCO) & master_profession %in% c("") ~ 814,
    is.na(ISCO) & master_profession %in% c("") ~ 815,
    is.na(ISCO) & master_profession %in% c("") ~ 816,
    is.na(ISCO) & master_profession %in% c("") ~ 817,
    is.na(ISCO) & master_profession %in% c("") ~ 818,
    is.na(ISCO) & master_profession %in% c("") ~ 821,
    is.na(ISCO) & master_profession %in% c("") ~ 831,
    is.na(ISCO) & master_profession %in% c("") ~ 832,
    is.na(ISCO) & master_profession %in% c("") ~ 833,
    is.na(ISCO) & master_profession %in% c("") ~ 834,
    is.na(ISCO) & master_profession %in% c("") ~ 835,
    is.na(ISCO) & master_profession %in% c("") ~ 911,
    is.na(ISCO) & master_profession %in% c("") ~ 912,
    is.na(ISCO) & master_profession %in% c("") ~ 921,
    is.na(ISCO) & master_profession %in% c("") ~ 931,
    is.na(ISCO) & master_profession %in% c("") ~ 932,
    is.na(ISCO) & master_profession %in% c("") ~ 933,
    is.na(ISCO) & master_profession %in% c("") ~ 941,
    is.na(ISCO) & master_profession %in% c("") ~ 951,
    is.na(ISCO) & master_profession %in% c("") ~ 952,
    is.na(ISCO) & master_profession %in% c("") ~ 961,
    is.na(ISCO) & master_profession %in% c("") ~ 962,
    is.na(ISCO) & master_profession %in% c("") ~ 011,
    is.na(ISCO) & master_profession %in% c("") ~ 021,
    is.na(ISCO) & master_profession %in% c("") ~ 031,
    is.na(ISCO) & master_profession %in% c("") ~ -999,
    
    .default = ISCO
  )) %>% 
  left_join(., occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession) %>% 
  select(-n) %>% 
  filter(
    !is.na(master_profession),
    is.na(Occupation_label),
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