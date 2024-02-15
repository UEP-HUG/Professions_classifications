pacman::p_load(    # Installs / loads packages if they are not already installed / loaded
  here,         # File locator
  janitor,      
  readxl,
  stringi       # String manipulation
)

# source(here("code","03_participants with profession data.R"))
dat_master_professions_2 <- readRDS(here("output", "dat_master_professions_2.rds"))

occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset

# Keep the variables that can be helpful for choosing how to assign ISCO-08 codes
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
  relocate(ISCO, .after = master_profession)


# Update the dataset to include the ISCO-08 codes and occupation labels ####
occup_ISCO <- occup %>% 
  mutate(ISCO = case_when(
    # 
    ## Managers ####
    ### Legislators and senior officials ####
    master_profession %in% c("") ~ 111,
    ### Managing directors and chief executives ####
    master_profession %in% c("") ~ 112,
    ### Business services and administration managers ####
    master_profession %in% c("") ~ 121,
    ### Sales, marketing and development managers ####
    master_profession %in% c("") ~ 122,
    ### Production managers in agriculture, forestry and fisheries ####
    master_profession %in% c("") ~ 131,
    ### Manufacturing, mining, construction, and distribution managers ####
    master_profession %in% c("") ~ 132,
    ### Information and communications technology service managers ####
    master_profession %in% c("") ~ 133,
    ### Professional services managers ####
    #### Child care services managers ####
    master_profession %in% c("") ~ 1341,
    #### Health services managers ####
    master_profession %in% c("") ~ 1342,
    #### Aged care services managers ####
    master_profession %in% c("") ~ 1343,
    #### Social welfare managers ####
    master_profession %in% c("") ~ 1344,
    #### Education managers ####
    master_profession %in% c("psychomotricienne a 20% et directice d'une ecole de danse 50%") ~ 1345,
    #### Financial and insurance services branch managers ####
    master_profession %in% c("") ~ 1346,
    #### Professional services managers not elsewhere classified ####
    master_profession %in% c("") ~ 1349,
    ### Hotel and restaurant managers ####
    master_profession %in% c("") ~ 141,
    ### Retail and wholesale trade managers ####
    master_profession %in% c("") ~ 142,
    ### Other services managers ####
    master_profession %in% c("") ~ 143,
    
    
    ## Science and engineering professionals ####
    ### Physical and earth science professionals ####
    master_profession %in% c("") ~ 211,
    ### Mathematicians, actuaries and statisticians ####
    master_profession %in% c("") ~ 212,
    ### Life science professionals ####
    master_profession %in% c("") ~ 213,
    ### Engineering professionals (excluding electrotechnology) ####
    master_profession %in% c("") ~ 214,
    ### Electrotechnology engineers ####
    master_profession %in% c("") ~ 215,
    ### Architects, planners, surveyors and designers ####
    master_profession %in% c("") ~ 216,
    
    
    ## Health Professionals ####
    ### Medical doctors ####
    #### General practitioners ####
    master_profession %in% c("medecin", "medecin - responsable de projet", "medecin a l' universite de ge",
                             "medecin generaliste","medecin i dependant", "medecin independant",
                             "medecin interne", "medecin interne hug", "medecin interniste",
                             "medecin responsable de l'unite sante du personnel chez msf",
                             "medecins generalistes", "medecin, fonctionnaire international") ~ 2211,
    #### Specialists ####
    master_profession %in% c("medecin specialiste", "medecin gynecologue", "medecin agrege", 
                             "medecin anesthesiste", "medecin chirurgien","medecin gynecologue",
                             "medecin je", "medecin neurologue", "medecin ophtalmologue",
                             "medecin pediatre", "medecin psychiatre", "medecin radiologue", 
                             "medecin radiopediatre", "medecin specialiste gastroenterologue",
                             "medecins specialistes", "medevin psychiatre et psychotherapeute",
                             "psychiatre psychotherapeute", "chirurgien", "chirurgienne",
                             "physicienne medicale en radiotherapie") ~ 2212,
    ### Nurses ####
    #### Nursing professionals ####
    master_profession %in% c("infirmier", "infirmiere", "infirmiere et therapeute") ~ 2221,
    #### Midwifery professionals ####
    master_profession %in% c("sage femme", "sage-femme") ~ 2222,
    ### Traditional and complementary medicine professionals ####
    master_profession %in% c("therapeute en mtc", "therapeute energeticienne", "therapeute et conseillere en sejours linguistique") ~ 223,
    ### Paramedical practitioners ####
    master_profession %in% c("") ~ 224,
    ### Veterinarians ####
    master_profession %in% c("") ~ 225,
    ### Other health professionals ####
    #### Dentists ####
    master_profession %in% c("dentist", "dentiste", "medecin dentiste", "medecin-dentiste") ~ 2261,
    #### Pharmacists ####
    master_profession %in% c("pharmacien", "pharmacienne", "pharmacien hospitalier", "pharmacien industriel",
                             "pharmacien responable", "pharmacien responsable", "pharmacien responsble",
                             "pharmacienne cheffe de projet", "pharmacienne clinicienne", "pharmacienne doctorante",
                             "pharmacienne had", "pharmacienne responsable", "pharmaciens", "pharmacist") ~ 2262,
    #### Environmental and occupational health and hygiene professionals ####
    master_profession %in% c("maitre de readaptation") ~ 2263,
    #### Physiotherapists ####
    master_profession %in% c("physio", "physiotherapeute", "physiotherapeute cardio respiratoire", 
                             "physiotherapeute cardio-respiratoire", "physiotherapeutes", "phisiotherapeute",
                             "physiotherapeute independant, chef d'un cabinet avec 1 employee") ~ 2264,
    #### Dieticians and nutritionists ####
    master_profession %in% c("dieteticien", "dieteticienne", "dieteticiens et specialistes de la nutrition",
                             "therapeute nutritionniste") ~ 2265,
    #### Audiologists and speech therapists ####
    master_profession %in% c("") ~ 2266,
    #### Optometrists and ophthalmic opticians ####
    master_profession %in% c("orthoptiste") ~ 2267,
    #### The rest ####
    master_profession %in% c("ergotherapeute", "art-therapeute", "apres retraite therapeute", "musicotherapeute", 
                             "therapeute", "therapeute en reflexologie", "ergotherapie",
                             "therapeute en techniques manuelles, massages therapeutiques, reflexologie, sonotherapie") ~ 2269,
    ## Teaching professionals ####
    master_profession %in% c("") ~ 231,
    master_profession %in% c("") ~ 232,
    master_profession %in% c("") ~ 233,
    master_profession %in% c("") ~ 234,
    master_profession %in% c("") ~ 235,
    master_profession %in% c("") ~ 241,
    master_profession %in% c("") ~ 242,
    master_profession %in% c("") ~ 243,
    master_profession %in% c("") ~ 251,
    master_profession %in% c("") ~ 252,
    ## Legal, social and cultural professionals ####
    master_profession %in% c("") ~ 261,
    master_profession %in% c("") ~ 262,
    ### Social and religious professionals ####
    #### Psychologists ####
    master_profession %in% c("neuropsychologue", "psychologue", "psychologue conseillere en orientation", 
                             "psychologue du travail et gestion des carrieres", "psychologue fsp",
                             "psychologue psychotherapeute", "psychologues", "psychomotricien", "psychomotricienne",
                             "psychotherapeute", "psychotherapeute en cabinet privee, independante", 
                             "psychotherapeute independante", "stagiaire psychologue en formation",
                             "therapeute en psychomotricite", "psycotherapeute, coach", "responsable service ergotherapie"
                             ) ~ 2634,
    #### Rest ####
    master_profession %in% c("") ~ 263,
    master_profession %in% c("") ~ 264,
    ### Creative and performing artists ####
    master_profession %in% c("metteur en scene et therapeute") ~ 265,
    master_profession %in% c("") ~ 311,
    master_profession %in% c("") ~ 312,
    master_profession %in% c("") ~ 313,
    master_profession %in% c("") ~ 314,
    master_profession %in% c("") ~ 315,
    ## Health Associate professionals ####
    ### Medical and pharmaceutical technicians ####
    #### Pharmaceutical technicians and assistants ####
    master_profession %in% c("apprentie assistante en pharmacie", "apprentie en pharmacie",
                             "assistamte en pharmacie", "assistant en pharmacie", "assistante en pharmacie",
                             "assistante en pharmacie (officine)", "assistante gestion en pharmacie",
                             "assistante pharmacie", "assistante pharmacien", "preparatrice en pharmacie") ~ 3213,
    ### Nursing and midwifery associate professionals ####
    master_profession %in% c("") ~ 322,
    ### Traditional and complementary medicine associate professionals ####
    master_profession %in% c("independente nutrition holistique / therapeute de fleurs de bach / hypnotherapeute (hypnopraxie)") ~ 323,
    ### Veterinary technicians and assistants ####
    master_profession %in% c("") ~ 324,
    ### Other health associate professionals ####
    master_profession %in% c("massotherapeute", "massotherapeute et personal trainer", "massotherapeute/ reflexologie",
                             "therapeute de shiatsu") ~ 325,
    
    
    master_profession %in% c("") ~ 331,
    master_profession %in% c("") ~ 332,
    master_profession %in% c("") ~ 333,
    master_profession %in% c("") ~ 334,
    master_profession %in% c("") ~ 335,
    ## Legal, social, cultural and related associate professionals ####
    ### Legal, social and religious associate professionals ####
    master_profession %in% c("intervenante psychosociale") ~ 341,
    master_profession %in% c("") ~ 342,
    master_profession %in% c("") ~ 343,
    master_profession %in% c("") ~ 351,
    master_profession %in% c("") ~ 352,
    master_profession %in% c("") ~ 411,
    master_profession %in% c("") ~ 412,
    master_profession %in% c("") ~ 413,
    master_profession %in% c("") ~ 421,
    master_profession %in% c("") ~ 422,
    master_profession %in% c("") ~ 431,
    master_profession %in% c("") ~ 432,
    master_profession %in% c("") ~ 441,
    master_profession %in% c("") ~ 511,
    master_profession %in% c("") ~ 512,
    master_profession %in% c("") ~ 513,
    master_profession %in% c("") ~ 514,
    ## Personal service workers ####
    master_profession %in% c("") ~ 515,
    ### Other personal services workers ####
    master_profession %in% c("astro-psychologue") ~ 516,
    master_profession %in% c("") ~ 521,
    master_profession %in% c("") ~ 522,
    master_profession %in% c("") ~ 523,
    master_profession %in% c("") ~ 524,
    master_profession %in% c("") ~ 531,
    master_profession %in% c("") ~ 532,
    master_profession %in% c("") ~ 541,
    master_profession %in% c("") ~ 611,
    master_profession %in% c("") ~ 612,
    master_profession %in% c("") ~ 613,
    master_profession %in% c("") ~ 621,
    master_profession %in% c("") ~ 622,
    master_profession %in% c("") ~ 631,
    master_profession %in% c("") ~ 632,
    master_profession %in% c("") ~ 633,
    master_profession %in% c("") ~ 634,
    master_profession %in% c("") ~ 711,
    master_profession %in% c("") ~ 712,
    master_profession %in% c("") ~ 713,
    master_profession %in% c("") ~ 721,
    master_profession %in% c("") ~ 722,
    master_profession %in% c("") ~ 723,
    master_profession %in% c("") ~ 731,
    master_profession %in% c("") ~ 732,
    master_profession %in% c("") ~ 741,
    master_profession %in% c("") ~ 742,
    master_profession %in% c("") ~ 751,
    master_profession %in% c("") ~ 752,
    master_profession %in% c("") ~ 753,
    master_profession %in% c("") ~ 754,
    master_profession %in% c("") ~ 811,
    master_profession %in% c("") ~ 812,
    master_profession %in% c("") ~ 813,
    master_profession %in% c("") ~ 814,
    master_profession %in% c("") ~ 815,
    master_profession %in% c("") ~ 816,
    master_profession %in% c("") ~ 817,
    master_profession %in% c("") ~ 818,
    master_profession %in% c("") ~ 821,
    master_profession %in% c("") ~ 831,
    master_profession %in% c("") ~ 832,
    master_profession %in% c("") ~ 833,
    master_profession %in% c("") ~ 834,
    master_profession %in% c("") ~ 835,
    master_profession %in% c("") ~ 911,
    master_profession %in% c("") ~ 912,
    master_profession %in% c("") ~ 921,
    master_profession %in% c("") ~ 931,
    master_profession %in% c("") ~ 932,
    master_profession %in% c("") ~ 933,
    master_profession %in% c("") ~ 941,
    master_profession %in% c("") ~ 951,
    master_profession %in% c("") ~ 952,
    master_profession %in% c("") ~ 961,
    master_profession %in% c("") ~ 962,
    master_profession %in% c("") ~ 011,
    master_profession %in% c("") ~ 021,
    master_profession %in% c("") ~ 031,
    master_profession %in% c("") ~ -999,
    
    .default = ISCO
  )) %>% 
  left_join(., occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = master_profession) %>% 
  select(-n) %>% 
  filter(
    is.na(Occupation_label),
    # !ISCO %in% c(-999)
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

# Working set (to refer to for assigning codes) ####
a <- occup_ISCO |> filter(str_detect(master_profession, "therap"))

# # Indices ####
# 
# # # Read teleworkability indices from locally saved file
# indices <- read_csv(here("data", "Telework ISCO indices.txt")) %>%     # read in the teleworkability indices (low "physical_interaction" = low teleworkability)
#   janitor::clean_names() %>% select(!occupation_title) %>% rename(isco_3 = isco08)
# # Read teleworkability indices from original source
# # indices <- read_csv("https://raw.githubusercontent.com/m-sostero/telework-occupations/master/Telework%20ISCO%20indices.csv") %>%     # read in directly from source GitHub page
# #   janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# # Read in ISCO code occupation labels
# indices_2 <- read_xlsx(here("data", "EWCS 3 digit.xlsx")) %>% mutate(isco_3 = as.numeric(isco3d)) %>% select(isco_3,telework, physicalb)
# # Merge the indices with the ISCO labels
# indices <- left_join(indices, indices_2)
# 
# # Finalizing the file ####
# # In the final file, keep only the relevant columns that will be merged with our full dataset
# occup_ISCO_final <- occup_ISCO %>% select(codbar, master_profession, ISCO) |> filter(ISCO != -999)
# 
# # Read in the classified occupations data
# # (generated from "01_prep_a_classify occupations.R")
# occup_ISCO_final <- occup_ISCO_final %>% mutate(
#   isco_full = ISCO,
#   isco_3 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 3))),
#   isco_2 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 2))),
#   isco_1 = case_when(ISCO == -999 ~ -999, .default = as.numeric(str_sub(ISCO, end = 1)))
# ) %>% 
#   select(-c(ISCO)) %>% # Remove the extra columns
#   left_join(indices) # Merge with the indices dataframe
# 
# # Label the occupations from the ISCO classifications for each code level (from 4 = "full" down to level 1)
# occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_full" = "ISCO")) %>% rename(Occupation_label_full = Occupation_label)
# occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_3" = "ISCO")) %>% rename(Occupation_label_3 = Occupation_label)
# occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_2" = "ISCO")) %>% rename(Occupation_label_2 = Occupation_label)
# occup_ISCO_final <- left_join(occup_ISCO_final, occ_labels, by = c("isco_1" = "ISCO")) %>% rename(Occupation_label_1 = Occupation_label)
# 
# occup_ISCO_final <- occup_ISCO_final |> 
#   relocate(Occupation_label_full:Occupation_label_1, .after = master_profession)
# 
# # # Save the final dataset
# # saveRDS(occup_ISCO_final, here("data", "Classified_occupations.RDS"), ascii = TRUE)