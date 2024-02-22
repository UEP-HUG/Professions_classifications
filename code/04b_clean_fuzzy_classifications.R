pacman::p_load(here, tidyverse)

# Read in files ####
if (file.exists(here("output", "fuzzy_classified_occupations_to_clean_2024-02-21.rds"))) {
  occup_final <- readRDS(here("output", "fuzzy_classified_occupations_to_clean_2024-02-21.rds"))
  remaining_bad_matches <- readRDS(here("output", "remaining_bad_matches.rds"))
} else {
  source(here("code", "04a_make_fuzzy_classifications.R"))
} 
## Read in ISCO labels ####
occ_labels <- readxl::read_xlsx(here("data", "do-e-00-isco08-01.xlsx"), # read in occupation titles based on ISCO code
                                sheet = "ISCO-08 English version") %>% drop_na() %>% mutate(ISCO = as.numeric(ISCO)) %>% 
  filter(!str_detect(Occupation_label, "armed forces|Armed forces")) # Remove armed services as their numbers cause weirdness and we don't have them in our dataset


# Count the number of participants that are categorized
occup_final |> filter(!is.na(ISCO)) |> distinct() |> summarise(n = n_distinct(participant_id))


# Manually assign ISCO codes ####
occup_final_cleaned <- occup_final |> 
  mutate(ISCO_new = case_when(
    ## **Managers** ####
    ### Chief executives, senior officials and legislators ####
    #### Legislators and senior officials ####
    master_profession_original %in% c("") ~ 111,
    ##### Senior government officials ####
    master_profession_original %in% c("cheffe etat ge (ne souhaite pas preciser quel service)", 
                                      "che service dip etat de geneve") ~ 1112,
    ##### Senior officials of special-interest organisations ####
    master_profession_original %in% c("directeur- secretaire general") ~ 1114,
    #### Managing directors and chief executives ####
    master_profession_original %in% c("ceo") ~ 112,
    ### Administrative and commercial managers ####
    #### Business services and administration managers ####
    master_profession_original %in% c("chief risk officer", "gerant d'investissements") ~ 121,
    ##### Finance managers ####
    master_profession_original %in% c("chef de service service financier", "cfo") ~ 1211,
    ##### Human Resources managers ####
    master_profession_original %in% c("administratrice ressources humaines", "cheffe de service rh") ~ 1212,
    ##### Business services and administration managers not elsewhere classified ####
    master_profession_original %in% c("gestionnaire sinistres rc corporelles") ~ 1219,
    
    #### Sales, marketing and development managers ####
    master_profession_original %in% c("") ~ 122,
    ### Production and specialized services managers ####
    #### Production managers in agriculture, forestry and fisheries ####
    master_profession_original %in% c("") ~ 131,
    #### Manufacturing, mining, construction, and distribution managers ####
    master_profession_original %in% c("") ~ 132,
    #### Information and communications technology service managers ####
    master_profession_original %in% c("directeur risk management et it dans societe financiere") ~ 133,
    #### Professional services managers ####
    ##### Child care services managers ####
    master_profession_original %in% c("directrice de secteur petite enfance", "responsable de secteur parascolaire") ~ 1341,
    ##### Health services managers ####
    master_profession_original %in% c(
      "cadre de sante", "cadre superieur de sante", "cadres de direction, services de sante",
      "cqdre de sante", "directeur rse sante surete environnement", 
      "sante publique haut fonctionnaire, onu (oms)") ~ 1342,
    ##### Aged care services managers ####
    master_profession_original %in% c("") ~ 1343,
    ##### Social welfare managers ####
    master_profession_original %in% c("") ~ 1344,
    ##### Education managers ####
    master_profession_original %in% c("psychomotricienne a 20% et directice d'une ecole de danse 50%") ~ 1345,
    ##### Financial and insurance services branch managers ####
    str_detect(master_profession_original, "cadre bancaire") ~ 1346,
    master_profession_original %in% c("") ~ 1346,
    ##### Professional services managers not elsewhere classified ####
    master_profession_original %in% c("") ~ 1349,
    ### Hospitality, retail and other services managers ####
    #### Hotel and restaurant managers ####
    master_profession_original %in% c("") ~ 141,
    #### Retail and wholesale trade managers ####
    master_profession_original %in% c("") ~ 142,
    #### Other services managers ####
    master_profession_original %in% c("") ~ 143,
    
    ## **Professionals** ####
    ### Not easy to classify ####
    master_profession_original %in% c("chercheur doctorant", "adjoint scientifique") ~ 2000,
    ### Science and engineering professionals ####
    #### Physical and earth science professionals ####
    master_profession_original %in% c("") ~ 211,
    #### Mathematicians, actuaries and statisticians ####
    master_profession_original %in% c("") ~ 212,
    #### Life science professionals ####
    master_profession_original %in% c("") ~ 213,
    #### Engineering professionals (excluding electrotechnology) ####
    master_profession_original %in% c("") ~ 214,
    ##### Civil engineers ####
    master_profession_original %in% c("ingenieur en automatisme du batiment") ~ 2142,
    #### Electrotechnology engineers ####
    master_profession_original %in% c("") ~ 215,
    #### Architects, planners, surveyors and designers ####
    master_profession_original %in% c("concepteurs graphiques, multimedia - graphistes") ~ 216,
    
    ### Health Professionals ####
    #### Medical doctors ####
    ##### General practitioners ####
    master_profession_original %in% c(
      "medecin", "medecin - responsable de projet", "medecin a l' universite de ge",
      "medecin generaliste","medecin i dependant", "medecin independant",
      "medecin interne", "medecin interne hug", "medecin interniste",
      "medecin responsable de l'unite sante du personnel chez msf",
      "medecins generalistes", "medecin, fonctionnaire international") ~ 2211,
    ##### Specialists ####
    master_profession_original %in% c(
      "medecin specialiste", "medecin gynecologue", "medecin agrege", 
      "medecin anesthesiste", "medecin chirurgien","medecin gynecologue",
      "medecin je", "medecin neurologue", "medecin ophtalmologue",
      "medecin pediatre", "medecin psychiatre", "medecin radiologue", 
      "medecin radiopediatre", "medecin specialiste gastroenterologue",
      "medecins specialistes", "medevin psychiatre et psychotherapeute",
      "psychiatre psychotherapeute", "chirurgien", "chirurgienne",
      "physicienne medicale en radiotherapie") ~ 2212,
    #### Nurses ####
    ##### Nursing professionals ####
    master_profession_original %in% c(
      "infirmier", "infirmiere", "infirmiere et therapeute", "infirmiere de sante au travail",
      "infirmiere de sante communautaire", "infirmiere sante travail", 
      "infitmiere de sante au travail", "infirmiere en sante communautaire") ~ 2221,
    ##### Midwifery professionals ####
    master_profession_original %in% c("sage femme", "sage-femme") ~ 2222,
    #### Traditional and complementary medicine professionals ####
    master_profession_original %in% c(
      "therapeute en mtc", "therapeute energeticienne", "acupunctrice", "acupunctutrice",
      "therapeute et conseillere en sejours linguistique", 
      "enseignant taiji - qi gong, reflexologue, acupuncteur") ~ 223,
    #### Paramedical practitioners ####
    master_profession_original %in% c("") ~ 224,
    #### Veterinarians ####
    master_profession_original %in% c("") ~ 225,
    #### Other health professionals ####
    ##### Dentists ####
    master_profession_original %in% c("dentist", "dentiste", "medecin dentiste", "medecin-dentiste") ~ 2261,
    ##### Pharmacists ####
    master_profession_original %in% c(
      "pharmacien", "pharmacienne", "pharmacien hospitalier", "pharmacien industriel",
      "pharmacien responable", "pharmacien responsable", "pharmacien responsble",
      "pharmacienne cheffe de projet", "pharmacienne clinicienne", "pharmacienne doctorante",
      "pharmacienne had", "pharmacienne responsable", "pharmaciens", "pharmacist") ~ 2262,
    ##### Environmental and occupational health and hygiene professionals ####
    master_profession_original %in% c("maitre de readaptation") ~ 2263,
    ##### Physiotherapists ####
    master_profession_original %in% c(
      "physio", "physiotherapeute", "physiotherapeute cardio respiratoire", 
      "physiotherapeute cardio-respiratoire", "physiotherapeutes", "phisiotherapeute",
      "physiotherapeute independant, chef d'un cabinet avec 1 employee") ~ 2264,
    ##### Dieticians and nutritionists ####
    master_profession_original %in% c("dieteticien", "dieteticienne", "dieteticiens et specialistes de la nutrition",
                                      "therapeute nutritionniste") ~ 2265,
    ##### Audiologists and speech therapists ####
    master_profession_original %in% c("logopediste") ~ 2266,
    ##### Optometrists and ophthalmic opticians ####
    master_profession_original %in% c("orthoptiste", "optometriste") ~ 2267,
    ##### The rest ####
    master_profession_original %in% c(
      "ergotherapeute", "art-therapeute", "apres retraite therapeute", "musicotherapeute", 
      "therapeute", "therapeute en reflexologie", "ergotherapie", "ergothera peute",
      "therapeute en techniques manuelles, massages therapeutiques, reflexologie, sonotherapie",
      "domaine de la sante") ~ 2269,
    
    ### Teaching professionals ####
    master_profession_original %in% c("") ~ 231,
    master_profession_original %in% c("") ~ 232,
    master_profession_original %in% c("") ~ 233,
    master_profession_original %in% c("enseignante benevole") ~ 234,
    master_profession_original %in% c("") ~ 235,
    
    ### Business and administration professionals ####
    master_profession_original %in% c(
      "administratif financier", "conseil en gestion des risques", "gestionnaire de fortune indepedant",
      "gestionnaire financiere et administrative", "gestionnaire de risques") ~ 241,
    master_profession_original %in% c("") ~ 242,
    master_profession_original %in% c("") ~ 243,
    ### Information and communications technology professionals ####
    master_profession_original %in% c("") ~ 251,
    master_profession_original %in% c("") ~ 252,
    ### Legal, social and cultural professionals ####
    #### Legal professionals ####
    master_profession_original %in% c("") ~ 261,
    ##### Lawyers ####
    master_profession_original %in% c("") ~ 2611,
    ##### Judges ####
    master_profession_original %in% c("juge a la cour") ~ 2612,
    ##### Legal professionals not elsewhere classified ####
    master_profession_original %in% c("analyste en criminalite") ~ 2619,
    #### Librarians, archivists and curators ####
    master_profession_original %in% c("") ~ 262,
    #### Social and religious professionals ####
    ##### Psychologists ####
    master_profession_original %in% c(
      "neuropsychologue", "psychologue", "psychologue conseillere en orientation", 
      "psychologue du travail et gestion des carrieres", "psychologue fsp",
      "psychologue psychotherapeute", "psychologues", "psychomotricien", "psychomotricienne",
      "psychotherapeute", "psychotherapeute en cabinet privee, independante", 
      "psychotherapeute independante", "stagiaire psychologue en formation",
      "therapeute en psychomotricite", "psycotherapeute, coach", "responsable service ergotherapie"
    ) ~ 2634,
    ##### Rest ####
    master_profession_original %in% c("") ~ 263,
    master_profession_original %in% c("auteurs, journalistes et linguistes") ~ 264,
    #### Creative and performing artists ####
    master_profession_original %in% c("metteur en scene et therapeute") ~ 265,
    
    ## **Technicians and associate professionals** ####
    
    ### Science and engineering associate professionals ####
    master_profession_original %in% c("") ~ 311,
    master_profession_original %in% c("") ~ 312,
    master_profession_original %in% c("") ~ 313,
    master_profession_original %in% c("") ~ 314,
    master_profession_original %in% c("") ~ 315,
    master_profession_original %in% c("aircraft coordinator") ~ 31540,
    ### Health Associate professionals ####
    #### Medical and pharmaceutical technicians ####
    master_profession_original %in% c("") ~ 321,
    ##### Medical imaging and therapeutic equipment technicians ####
    master_profession_original %in% c("assistante en audiologir") ~ 3211,
    ##### Pharmaceutical technicians and assistants ####
    master_profession_original %in% c(
      "apprentie assistante en pharmacie", "apprentie en pharmacie",
      "assistamte en pharmacie", "assistant en pharmacie", "assistante en pharmacie",
      "assistante en pharmacie (officine)", "assistante gestion en pharmacie",
      "assistante pharmacie", "assistante pharmacien", "preparatrice en pharmacie") ~ 3213,
    ##### Medical and dental prosthetic technicians ####
    master_profession_original %in% c("technician dentist", "technicien dentiste", "technicien pour dentiste") ~ 3214,
    #### Nursing and midwifery associate professionals ####
    master_profession_original %in% c(
      "aide en soins et sante communautaire", "assistant en soins et sante communautaire",
      "assistante en sante et soins communautaires", "assistante en soins et sante communautaire",
      "assitante en soin et sante communautaire", "assistante en soins et sante communautaire assc",
      "assistante en soins et sante comunautaire", "assc urgences"
    ) ~ 322,
    #### Traditional and complementary medicine associate professionals ####
    # master_profession_original %in% c("independente nutrition holistique / therapeute de fleurs de bach / hypnotherapeute (hypnopraxie)") ~ 323,
    #### Veterinary technicians and assistants ####
    master_profession_original %in% c("") ~ 324,
    #### Other health associate professionals ####
    master_profession_original %in% c(
      "massotherapeute", "massotherapeute et personal trainer", "massotherapeute/ reflexologie",
      "therapeute de shiatsu", "charge de projet, sante et migrants",
      "chargeet de projet de sante publique", "cheffe de projet, sante publique",
      "consultante en sante publique", "consultante sante publique",
      "formatrice consultante en promotion et education a la sante",
      "resp sante & securite", "responsable pour des projet dans le metier sante",
      "sante travail", "specialiste de sante au travail", "assistante dentaire",
      "assistants en medecine dentaire", "hygieniste dentaire", "hygienistes dentaires",
      "hygieniste dentaire (desolee, je ne sais pas dans quelle categorie je dois le classer)",
      "opticien", "assistance maternel"
    ) ~ 325,
    #####  Ambulance workers ####
    master_profession_original %in% c(
      "ambulancier", "ambulancier dipl. es", "ambulancier diplome es",
      "ambulancier es", "ambulancier es / formateur fsea", "ambulancier et enseignant",
      "ambulanciere", "ambulanciere diplomee es", "ambulanciere es", "ambulanciere es et directrice",
      "ambulancieres", "etudiant ambulancier", "etudiante ambulanciere / technicienne ambulanciere",
      "pompiers ambulancier", "technicien ambulancier", "technicienne ambulanciere",
      "tecnicien ambulancier") ~ 3258,
    
    ### Business and administration associate professionals ####
    master_profession_original %in% c("") ~ 331,
    master_profession_original %in% c("account director", "acheteuse assitante commerciale") ~ 332,
    master_profession_original %in% c("") ~ 333,
    master_profession_original %in% c("coordinatrice de secteur") ~ 334,
    #### Regulatory government associate professionals ####
    master_profession_original %in% c("responsable surete surveillance") ~ 335,
    ### Legal, social, cultural and related associate professionals ####
    
    #### Legal, social and religious associate professionals ####
    master_profession_original %in% c("intervenante psychosociale") ~ 341,
    ##### Social work associate professionals ####
    master_profession_original %in% c("educatrice") ~ 3412,
    ##### Religious associate professionals ####
    str_detect(master_profession_original, "pastorale") ~ 34130,
    #### Sports and fitness workers ####
    master_profession_original %in% c("") ~ 342,
    master_profession_original %in% c("enseignant d'education physique") ~ 3422,
    #### Artistic, cultural and culinary associate professionals ####
    master_profession_original %in% c("accueil musee") ~ 343,
    #### Information and communications technicians ####
    master_profession_original %in% c("") ~ 351,
    master_profession_original %in% c("") ~ 352,
    
    ## **Clerical support workers** ####
    ### General and keyboard clerks ####
    #### General office clerks ####
    master_profession_original %in% c("autres employes de type administratif") ~ 411,
    #### Secretaries (general) ####
    master_profession_original %in% c("") ~ 412,
    #### Keyboard operators ####
    master_profession_original %in% c("") ~ 413,
    ### Customer services clerks ####
    master_profession_original %in% c("") ~ 421,
    master_profession_original %in% c("") ~ 422,
    ### Numerical and material recording clerks ####
    master_profession_original %in% c("") ~ 431,
    master_profession_original %in% c("disposante", "coordinateur logistique") ~ 432,
    ### Other clerical support workers ####
    master_profession_original %in% c("achats") ~ 441,
    str_detect(master_profession_original, "adjointe administrative - support rh") ~ 4416,
    ## **Service and sales workers** ####
    ### Personal service workers ####
    master_profession_original %in% c("") ~ 511,
    str_detect(master_profession_original, "agent d'escale|agente d'escale") ~ 5111,
    master_profession_original %in% c("") ~ 512,
    master_profession_original %in% c("") ~ 513,
    master_profession_original %in% c("") ~ 514,
    str_detect(master_profession_original, "coiffeuse") ~ 5141,
    master_profession_original %in% c("") ~ 515,
    #### Other personal services workers ####
    # master_profession_original %in% c("astro-psychologue") ~ 516,
    ### Sales workers ####
    master_profession_original %in% c("") ~ 521,
    # master_profession_original %in% c("droguiste") ~ 522,
    master_profession_original %in% c("") ~ 523,
    master_profession_original %in% c("") ~ 524,
    ### Personal Care workers ####
    ##### Child care workers and teachers' aides ####
    (str_detect(master_profession_original, "accueil") & str_detect(master_profession_original, "familial")) |
      master_profession_original %in% c("acceuillante familiale a la journee") ~ 5311,
    ##### Personal care workers in health services ####
    # (str_detect(master_profession_original, "aid") & str_detect(master_profession_original, "soign")) | 
    #   master_profession_original %in% c("auxiliaire de sante", "auxiliaire de sante dans un ems") ~ 532,
    master_profession_original %in% c("a$e", "accompagnante, aide a domicile", "accompagnant") ~ 5322,
    ### Protective services workers ####
    master_profession_original %in% c("") ~ 541,
    
    ## **Skilled agricultural, forestry and fishery workers** ####
    ### Market-oriented skilled agricultural workers ####
    master_profession_original %in% c("") ~ 611,
    master_profession_original %in% c("") ~ 612,
    master_profession_original %in% c("") ~ 613,
    ### Market-oriented skilled forestry, fishery and hunting workers ####
    master_profession_original %in% c("") ~ 621,
    master_profession_original %in% c("") ~ 622,
    ### Subsistence farmers, fishers, hunters and gatherers ####
    master_profession_original %in% c("") ~ 631,
    master_profession_original %in% c("") ~ 632,
    master_profession_original %in% c("") ~ 633,
    master_profession_original %in% c("") ~ 634,
    
    ## **Craft and related trades workers** ####
    ### Building and related trades workers, excluding electricians ####
    
    master_profession_original %in% c("") ~ 711,
    master_profession_original %in% c("") ~ 712,
    master_profession_original %in% c("") ~ 713,
    ### Metal, machinery and related trades workers ####
    master_profession_original %in% c("") ~ 721,
    master_profession_original %in% c("cableur") ~ 7215,
    master_profession_original %in% c("") ~ 722,
    master_profession_original %in% c("") ~ 723,
    ### Handicraft and printing workers ####
    master_profession_original %in% c("") ~ 731,
    master_profession_original %in% c("") ~ 732,
    ### Electrical and electronic trades workers ####
    master_profession_original %in% c("employe sig") ~ 741,
    master_profession_original %in% c("") ~ 742,
    ### Food processing, wood working, garment and other craft and related trades workers ####
    master_profession_original %in% c("") ~ 751,
    master_profession_original %in% c("") ~ 752,
    master_profession_original %in% c("") ~ 753,
    master_profession_original %in% c("") ~ 754,
    
    ## **Plant and machine operators and assemblers** ####
    ### Stationary plant and machine operators ####
    master_profession_original %in% c("") ~ 811,
    master_profession_original %in% c("") ~ 812,
    master_profession_original %in% c("") ~ 813,
    master_profession_original %in% c("") ~ 814,
    master_profession_original %in% c("") ~ 815,
    master_profession_original %in% c("") ~ 816,
    master_profession_original %in% c("") ~ 817,
    master_profession_original %in% c("") ~ 818,
    ### Assemblers ####
    master_profession_original %in% c("") ~ 821,
    ### Drivers and mobile plant operators ####
    master_profession_original %in% c("") ~ 831,
    master_profession_original %in% c("") ~ 832,
    master_profession_original %in% c("") ~ 833,
    master_profession_original %in% c("") ~ 834,
    master_profession_original %in% c("") ~ 835,
    
    ## **Elementary occupations** ####
    
    ### Cleaners and helpers ####
    master_profession_original %in% c("") ~ 911,
    master_profession_original %in% c("") ~ 912,
    ### Agricultural, forestry and fishery labourers ####
    master_profession_original %in% c("") ~ 921,
    ### Labourers in mining, construction, manufacturing and transport ####
    master_profession_original %in% c("") ~ 931,
    master_profession_original %in% c("") ~ 932,
    master_profession_original %in% c("") ~ 933,
    ### Food preparation assistants ####
    master_profession_original %in% c("") ~ 941,
    ### Street and related sales and service workers ####
    master_profession_original %in% c("") ~ 951,
    master_profession_original %in% c("") ~ 952,
    ### Refuse workers and other elementary workers ####
    master_profession_original %in% c("") ~ 961,
    master_profession_original %in% c("") ~ 962,
    
    ## **Military** ####
    master_profession_original %in% c("") ~ 011,
    master_profession_original %in% c("") ~ 021,
    master_profession_original %in% c("") ~ 031,
    
    ## **Not possible to define** ####
    master_profession_original %in% c(
      "actuellement mere au foyer - avant banquiere", "au foyer",
      "aucun", "aucune",
                                      ) ~ -999,
    
    
    .default = NA
  )) |>
  mutate(confidence = case_when(!is.na(ISCO_new) ~ "High", .default = confidence),
         ISCO = case_when(is.na(ISCO_new) ~ ISCO, .default = as.integer(str_sub(ISCO_new, start = 1, end = 4)))) |> 
  select(-Occupation_label) |> 
  left_join(occ_labels) |>                       # Merge with ISCO occupations file
  relocate(Occupation_label, .after = Name_fr) |> 
  # update the remaining_bad_matches object for reference
  arrange(master_profession_original) ; remaining_bad_matches <- inner_join(
    remaining_bad_matches, occup_final_cleaned |> 
      filter(is.na(ISCO)) |> 
      select(participant_id), by = "participant_id") |> 
  arrange(master_profession_original, participant_id) ; occup_final_cleaned |> filter(!is.na(ISCO)) |> distinct() |> 
  summarise(n = n_distinct(participant_id))
