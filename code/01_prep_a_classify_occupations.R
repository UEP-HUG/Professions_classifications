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
# (key free-text information is sometimes in the profession_other.inc, job_sector.st_22, job_sector_other.st_22 variables)
occup <- dat_master_professions %>% 
  select(profession.inc, profession.st_22, job_sector.st_22, supervision.st_22,
         job_sector_other.st_22, profession_other.inc, job.st_23,
         codbar) %>%
  # Make some changes to the free-text columns to make them easier to work with
  # (Convert accent characters, e.g. "é" to "e", convert all capital letters to small letters)
  mutate(profession.st_22 = stringi::stri_trans_general(str = profession.st_22, 
                                                    id = "Latin-ASCII"), # Convert accent characters, e.g. "é" to "e"
         profession.st_22 = str_trim(str_to_lower(profession.st_22)),             # Convert all capital letters to small letters
         
         job_sector_other.st_22 = stringi::stri_trans_general(str = job_sector_other.st_22, 
                                                        id = "Latin-ASCII"),
         job_sector_other.st_22 = str_trim(str_to_lower(job_sector_other.st_22)),
         
         profession_other.inc = stringi::stri_trans_general(str = profession_other.inc, 
                                                        id = "Latin-ASCII"),
         profession_other.inc = str_trim(str_to_lower(profession_other.inc)),
         job.st_23 = stringi::stri_trans_general(str = job.st_23, 
                                                              id = "Latin-ASCII"),
         job.st_23 = str_trim(str_to_lower(job.st_23)),
         ISCO = NA                                                               # Empty column that we'll fill in the next steps
  ) %>%
  add_count(profession.st_22, sort = TRUE) %>% 
  relocate(ISCO, .after = profession.st_22)


# Update the dataset to include the ISCO-08 codes and occupation labels, using the below definitions:
occup_ISCO <- occup %>% 
  mutate(ISCO = case_when(
    
    # Services managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "resource|ressource| rh | rh") ~ 1212,
    
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Ambassade") ~ 111,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Administration publique") ~ 1112,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "industrie|construction|production") ~ 132,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Information") ~ 133,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Enseignement|Santé|transports|Banques|Sécurité|comptabilité") ~ 134,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Hébergement") ~ 141,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Commerce") ~ 142,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | profession.st_22 %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(profession.st_22, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Immobilier|Other|Arts") ~ 143,
    is.na(ISCO) & str_detect(profession.st_22, "ceo") ~ 112,
    
    
    # Gym / Sports teacher
    is.na(ISCO) & str_detect(profession.st_22, "professeur de gym|educateur sportif") ~ 342,
    
    
    # HR workers
    is.na(ISCO) & str_detect(profession.st_22, "resource|ressource| rh | rh| hr|hr ") | profession.st_22 %in% c("rh") ~ 242,
    is.na(ISCO) & str_detect(profession.st_22, "planific|recrut") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "administration et ressources humaines") ~ 242,
    
    
    # Health managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "infir|medecin|pharma") ~ 1342,
    is.na(ISCO) & str_detect(profession.st_22, "cadre de sante|directrice d'iepa") ~ 1342,
    
    # Nurses
    is.na(ISCO) & str_detect(profession.st_22, "infi|sage femme|sage-femme") ~ 222,
    # Doctors
    is.na(ISCO) & str_detect(profession.st_22, "medecin") ~ 221,
    
    
    # Care workers
    is.na(ISCO) & str_detect(profession.st_22, "aide") & str_detect(profession.st_22, "domic|soig|soin") ~ 532,
    is.na(ISCO) & str_detect(profession.st_22, "assc|ambulan|a.s.s.c.|soins ems|chauffeur bls aed|opticien|employee ems|asam iepa") ~ 325,
    
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "assistant") & str_detect(profession.st_22, "medic|dent|soins|soci") ~ 325,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "coordinat") ~ 325,
    
    # Social workers
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "social|educat|curat") ~ 2635,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "accompagn") ~ 3221,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "animat|socioprof|socioc|fonctionnaire a imad") ~ 3412,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & profession.st_22 %in% c("ais") ~ 3412,
    is.na(ISCO) & str_detect(profession.st_22, "responsable animation") ~ 3412,
    is.na(ISCO) & str_detect(profession.st_22, "insertion") ~ 3412,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & profession.st_22 %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") & profession.st_22 %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & profession.st_22 %in% c("maitre de readaptation") ~ 2263,
    is.na(ISCO) & str_detect(profession.st_22, "orthoptiste") ~ 2267,
    is.na(ISCO) & str_detect(profession.st_22, "sociolog|anthropolog|archeolog") ~ 2632,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") & str_detect(profession.st_22, "develop") ~ 2635,
    
    # Religious worker
    is.na(ISCO) & str_detect(profession.st_22, "pastor|eglise|pasteur") & str_detect(profession.st_22, "assistant") ~ 3413,
    is.na(ISCO) & str_detect(profession.st_22, "pastor|eglise|pasteur") ~ 2636,
    
    
    # Education managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "enseign") ~ 1345,
    is.na(ISCO) & str_detect(profession.st_22, "direction d'ecole") ~ 1345,
    
    # Professors
    is.na(ISCO) & str_detect(profession.st_22, "professe|profress|prof a une universite") ~ 2310,
    is.na(ISCO) & str_detect(profession.st_22, "directeur d'une ecole de commerce") ~ 2320,
    
    # Childcare
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Petite enfance") & !str_detect(profession.st_22, "policier") ~ 1341,
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") & str_detect(profession.st_22, "educat|anima") ~ 5311,
    is.na(ISCO) & str_detect(profession.st_22, "educatrice petite enfance|petite enfance") ~ 5311,
    is.na(ISCO) & str_detect(job_sector.st_22, "Enseignement") & str_detect(profession.st_22, "primaire") ~ 2341,
    # Other teaching professionals
    is.na(ISCO) & str_detect(profession.st_22, "enseignement|enseignan| formation|formateur|formatrice") ~ 235,
    is.na(ISCO) & str_detect(profession.st_22, "educatrice specialisee") ~ 2352,
    
    
    # Scientists / Science professionals
    is.na(ISCO) & str_detect(profession.st_22, "biologist") ~ 2131,
    is.na(ISCO) & str_detect(profession.st_22, "laborant|laborat|labonratine") ~ 3212,
    is.na(ISCO) & str_detect(profession.st_22, "geolog|geog") ~ 2114,
    is.na(ISCO) & str_detect(profession.st_22, "geom") ~ 2165,
    is.na(ISCO) & str_detect(profession.st_22, "chercheu|checheu|scientifique en recherche et developpement") ~ 213,
    is.na(ISCO) & str_detect(profession.st_22, "rechercher|doctorant") ~ 213,
    is.na(ISCO) & str_detect(profession.st_22, "graphisme|graphidme") ~ 2166,
    
    
    
    # Project managers
    is.na(ISCO) & str_detect(profession.st_22, "chef|manager|responsable|charge|gestion|coordinat") & 
      str_detect(profession.st_22, "project | projet|product |produit|program") ~ 1219,
    is.na(ISCO) & str_detect(profession.st_22, "planificateur|directrice de projets") ~ 1219,
    
    # Accounting managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "comptab") ~ 1211,
    is.na(ISCO) & str_detect(profession.st_22, "comptab|audit|fiduci|facturist") ~ 2411,
    
    # Security services
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Sécurité") & str_detect(profession.st_22, "yoga teacher") ~ 3423,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Sécurité") ~ 1349, # managers
    is.na(ISCO) & str_detect(job_sector.st_22, "Sécurité") & str_detect(profession.st_22, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(job_sector.st_22, "Sécurité") & str_detect(profession_other.inc, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(profession.st_22, "policier|police|garde bain|gendarm|douanier") ~ 541, # Other security
    is.na(ISCO) & str_detect(profession.st_22, "agent") & str_detect(profession.st_22, "surete|securite") ~ 541, # Other security
    is.na(ISCO) & profession.st_22 %in% c("enqueteur") ~ 335,
    is.na(ISCO) & str_detect(profession.st_22, "pompier") ~ 541,
    
    
    # Assistants / receptionists
    is.na(ISCO) & str_detect(profession.st_22, "assistant") & str_detect(profession.st_22, "medic")  ~ 3256,
    is.na(ISCO) & str_detect(profession.st_22, "assistant") & str_detect(profession.st_22, "pharma")  ~ 3213,
    is.na(ISCO) & str_detect(profession.st_22, "assistant") & str_detect(profession.st_22, "denta")  ~ 3251,
    is.na(ISCO) & str_detect(profession.st_22, "secretaire|secretar") & str_detect(profession.st_22, "medic|pharma|denta")  ~ 3344,
    is.na(ISCO) & str_detect(profession.st_22, "assistant") & str_detect(profession.st_22, "administrative|direction|gestion|parlem|gesti|parl|execu|")  ~ 334,
    is.na(ISCO) & str_detect(profession.st_22, "adjoint|gestion") & str_detect(profession.st_22, "administr") ~ 334,
    is.na(ISCO) & str_detect(profession.st_22, "huiss") ~ 334,
    is.na(ISCO) & !str_detect(profession.inc, "Cadre") & str_detect(profession.st_22, "secretaire|secretar|telephonist") ~ 412,
    is.na(ISCO) & profession.st_22 == "assistant" | profession.st_22 == "assistante" ~ 334,
    is.na(ISCO) & str_detect(profession.st_22, "reception") ~ 4226,
    is.na(ISCO) & str_detect(profession.st_22, "hotesse") & str_detect(profession.st_22, "accueil") ~ 4226,
    is.na(ISCO) & str_detect(profession.st_22, "office manager") ~ 3341,
    
    # Other services
    is.na(ISCO) & str_detect(profession.st_22, "voyage") ~ 4221,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "tourism") ~ 4221,
    is.na(ISCO) & str_detect(profession.st_22, "photograp") ~ 3431,
    is.na(ISCO) & str_detect(profession.st_22, "cameraman|videast") ~ 3521,
    is.na(ISCO) & str_detect(profession.st_22, "telecommunication|telemat") ~ 3522,
    is.na(ISCO) & str_detect(profession.st_22, "videoconference") ~ 352,
    is.na(ISCO) & str_detect(profession.st_22, "funera") ~ 5163,
    
    
    # Other health professionals
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "dentist|dentaire|pharma|dietet|audiol|optom|ergother|physio") ~ 134,
    is.na(ISCO) & str_detect(profession.st_22, "dentist|dentaire") ~ 2261,
    is.na(ISCO) & str_detect(profession.st_22, "pharma|drogu") ~ 2262,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "preparatrice") ~ 3213,
    is.na(ISCO) & str_detect(profession.st_22, "dietet") ~ 2265,
    is.na(ISCO) & str_detect(profession.st_22, "logoped") ~ 2266,
    is.na(ISCO) & str_detect(profession.st_22, "audiol|optom|ergother") ~ 226,
    is.na(ISCO) & str_detect(profession.st_22, "physio") ~ 2264,
    # Psychologists / therapists / other
    is.na(ISCO) & str_detect(profession.st_22, "radiotherap") ~ 3211,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(profession.st_22, "trm") ~ 3211,
    is.na(ISCO) & str_detect(profession.st_22, "psycho|therap") ~ 226,
    is.na(ISCO) & str_detect(profession.st_22, "sante en entreprise") ~ 2263,
    is.na(ISCO) & str_detect(profession.st_22, "senior emergency officer|medical affairs lead|regulateur sanitaire") ~ 2269,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & 
      str_detect(profession.st_22, "msp|aucun|responsable|affairs manager") ~ 2269,
    
    
    
    # Greffier / Juristes
    is.na(ISCO) & str_detect(profession.st_22, "greffier") | str_detect(profession_other.inc, "greffier") ~ 2619,
    is.na(ISCO) & str_detect(profession.st_22, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(profession_other.inc, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(profession.st_22, "compliance|conformite") ~ 2619,
    
    
    # IT
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "informatic|informatiq") ~ 1330,
    is.na(ISCO) & str_detect(profession.st_22, "informatic|informatiq|developpeur|helpdesk|helpdek") ~ 351,
    is.na(ISCO) & str_detect(profession.st_22, "information|programmatrice|solutions technologiques|solution engineer") ~ 251,
    is.na(ISCO) & profession.st_22 %in% c("programmeur") ~ 251,
    is.na(ISCO) & profession.st_22 %in% c("it", "consultant it", "developpeur") ~ 351,
    is.na(ISCO) & str_detect(profession.st_22, "charge de securite") ~ 2529,
    is.na(ISCO) & str_detect(profession.st_22, "conseil|administrat") & str_detect(profession.st_22, "donnee")  ~ 2511,
    # HR again
    is.na(ISCO) & str_detect(profession.st_22, "personn")  ~ 2423,
    
    
    
    
    # Banking / finance
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "banq") ~ 1346,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(profession.st_22, "financ") ~ 1211,
    is.na(ISCO) & str_detect(profession.st_22, "banq|fortune|financ|banker|bancair") ~ 241,
    is.na(ISCO) & str_detect(profession.st_22, "trader|trading") ~ 3311,
    is.na(ISCO) & str_detect(profession.st_22, "economist") ~ 2631,
    is.na(ISCO) & str_detect(profession.st_22, "patrimo|assuranc") ~ 241,
    is.na(ISCO) & str_detect(profession.st_22, "fiscalist|fscaliste|taxat") ~ 2411,
    is.na(ISCO) & str_detect(profession.st_22, "risk|risq") ~ 2413,
    is.na(ISCO) & str_detect(job_sector.st_22, "Banques") & str_detect(profession.st_22, "analyst") ~ 2413,
    is.na(ISCO) & str_detect(profession.st_22, "gerant d'investissements") ~ 241,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "technologie & finance") ~ 241,
    
    
    
    
    
    
    ## Other manager positions not picked up already
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Santé") ~ 1342,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Administration publique") ~ 1112,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Ambassade") ~ 111,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Banques, assurances") ~ 1346,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Hébergement et restauration") ~ 141,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "industrie|construction|production") ~ 132,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Enseignement") ~ 1345,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Commerce") ~ 142,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & !str_detect(job_sector.st_22, "Services domestiques") ~ 1439,
    
    ## Technicians
    is.na(ISCO) & str_detect(profession.st_22, "technicien") & str_detect(profession.st_22, "audiovis") ~ 352,
    is.na(ISCO) & str_detect(profession.st_22, "technicien") & str_detect(job_sector.st_22, "Santé") ~ 321,
    is.na(ISCO) & str_detect(profession.st_22, "technicien") & str_detect(job_sector.st_22, "Arts") ~ 265,
    is.na(ISCO) & str_detect(profession.st_22, "technicien") & str_detect(job_sector.st_22, "industrie|construction|production") ~ 311,
    is.na(ISCO) & str_detect(profession.st_22, "technicien") & str_detect(job_sector_other.st_22, "ascenseur") ~ 741,
    
    # Scientific collaborator
    is.na(ISCO) & str_detect(profession.st_22, "collabor|colabor|adjoint") & str_detect(profession.st_22, "scientif") ~ 213,
    is.na(ISCO) & str_detect(profession.st_22, "statisticien|analyste de donnees|data analyst") ~ 212,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Enseignement") & str_detect(profession.st_22, "") ~ 212,
    
    
    
    # Architect
    is.na(ISCO) & str_detect(profession.st_22, "architect|urbanist") & !str_detect(profession.st_22, "informat") ~ 216,
    is.na(ISCO) & str_detect(profession_other.inc, "architect|urbanist") ~ 216,
    
    is.na(ISCO) & str_detect(profession.st_22, "designer industriel") ~ 2163,
    is.na(ISCO) & str_detect(profession.st_22, "dessin") ~ 3118,
    is.na(ISCO) & str_detect(profession.st_22, "designer") ~ 216,
    
    
    # Historian
    is.na(ISCO) & str_detect(profession.st_22, "historien") ~ 2633,
    
    # Authors / journalists / interpreters
    is.na(ISCO) & str_detect(profession.st_22, "journalist") ~ 2642,
    is.na(ISCO) & str_detect(profession.st_22, "scripte") ~ 2641,
    is.na(ISCO) & str_detect(profession.st_22, "interpret|traduc|linguist") ~ 2643,
    is.na(ISCO) & str_detect(profession.st_22, "redact|monteur news") ~ 264,
    
    
    # Library
    is.na(ISCO) & str_detect(profession.st_22, "biblioth|archiv|documental|musee") ~ 262,
    
    # Process
    is.na(ISCO) & str_detect(profession.st_22, "quality manager") ~ 7543,
    # is.na(ISCO) & str_detect(profession.st_22, "control") & str_detect(profession.st_22, "qualit") ~ 7543,
    is.na(ISCO) & str_detect(profession.st_22, "qualit") ~ 7543,
    
    # Transport workers
    is.na(ISCO) & (str_detect(profession.st_22, "conduct|chauffeur|chauffer") | str_detect(profession_other.inc, "conduct|chauffeur|chauffer")) &
      (str_detect(profession.st_22, "bus|tram|tpg") | str_detect(profession_other.inc, "bus|tram|tpg")) ~ 8331,
    is.na(ISCO) & str_detect(profession.st_22, "pilot") & str_detect(profession.st_22, "locom") ~ 8311,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(profession.st_22, "regulate") ~ 8312,
    is.na(ISCO) & str_detect(profession.st_22, "conduct|chauffeur|conduczeur") ~ 832,
    is.na(ISCO) & str_detect(profession.st_22, "agent de train") ~ 5112,
    is.na(ISCO) & str_detect(profession.st_22, "pilot") & str_detect(profession.st_22, "ligne") ~ 3153,
    is.na(ISCO) & str_detect(profession.st_22, "control") & str_detect(profession.st_22, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(profession_other.inc, "control") & str_detect(profession_other.inc, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(profession.st_22, "control") & str_detect(job_sector.st_22, "transport") ~ 5112,
    is.na(ISCO) & str_detect(profession.st_22, "escale|aerop") | str_detect(profession_other.inc, "escale|arop") ~ 511,
    is.na(ISCO) & str_detect(profession.st_22, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(profession_other.inc, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(profession.st_22, "horaire") ~ 4323,
    is.na(ISCO) & str_detect(profession.st_22, "gestionnaire parc vehicule|responsable du parc de vehicule|gestionnaire de parc mobilite") ~ 9623, # or 1324, # fleet manager?
    is.na(ISCO) & str_detect(profession.st_22, "adjoint responsable reseau") ~ 2164,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "ingenieure mobilite") ~ 2164,
    is.na(ISCO) & str_detect(profession.st_22, "secteur transports|cff") ~ 3339,
    
    
    
    
    
    
    # Engineers
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "electr") ~ 215,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "telecom") ~ 2153,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "civil") ~ 2142,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "agron") ~ 2143,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "mecani") ~ 2144,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "chem") ~ 2145,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") & str_detect(profession.st_22, "medic") ~ 2149,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie|engen|engineer") & str_detect(profession.st_22, "inform|infom|system|it|reseau|logiciel|banking") ~ 252,
    is.na(ISCO) & str_detect(profession.st_22, "ingenie") ~ 214,
    is.na(ISCO) & str_detect(profession.st_22, "transferts de technologie") ~ 252,
    is.na(ISCO) & str_detect(profession.st_22, "chimi") ~ 2145,
    is.na(ISCO) & str_detect(profession.st_22, "dispatcher en electricite") ~ 3113,
    
    
    
    # Electrician
    is.na(ISCO) & str_detect(profession.st_22, "electricien") ~ 741,
    is.na(ISCO) & str_detect(profession.st_22, "electr") & str_detect(profession.st_22, "meca") ~ 7412,
    is.na(ISCO) & str_detect(profession.st_22, "electronicien") ~ 742,
    
    
    # Mechanic
    is.na(ISCO) & str_detect(profession.st_22, "meca|depanneur|machinist") ~ 723,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(profession.st_22, "technicien") ~ 723,
    is.na(ISCO) & str_detect(profession.st_22, "serrurier-constucteur") ~ 721,
    
    
    # Plumber
    is.na(ISCO) & str_detect(profession.st_22, "plomb") ~ 7126,
    
    
    
    # Gardener / agriculture
    is.na(ISCO) & str_detect(profession.st_22, "jardinier|horticult|viti|vigner") ~ 611,
    is.na(ISCO) & str_detect(profession.st_22, "ouvrier agricole") ~ 921,
    is.na(ISCO) & str_detect(profession.st_22, "oenologue") ~ 2132,
    is.na(ISCO) & str_detect(profession.st_22, "cavist") ~ 816,
    
    
    # Concierge
    is.na(ISCO) & str_detect(profession.st_22, "concierge") ~ 5153,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "concierge") ~ 515,
    
    
    # Postal worker / courier
    is.na(ISCO) & str_detect(profession.st_22, "poste|posta") | str_detect(profession_other.inc, "poste|posta") ~ 4412,
    is.na(ISCO) & str_detect(profession.st_22, "coursier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other.inc, "courrier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other.inc, "livreur|livreuse|bagagist") ~ 9621,
    is.na(ISCO) & str_detect(profession.st_22, "agent de distribution") ~ 4412,
    
    ## Fonctionnaire / employe
    is.na(ISCO) & str_detect(profession.st_22, "fonctionn|fonnctionn") & str_detect(profession.st_22, "international") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(profession_other.inc, "fonctionn") & str_detect(profession_other.inc, "onu") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(profession.st_22, "diplomat") ~ 1112,
    is.na(ISCO) & (str_detect(profession.st_22, "fonction|emplo|aucune") | 
                     profession.st_22 %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Ambassade|Administration publique") ~ 242,
    is.na(ISCO) & (str_detect(profession.st_22, "fonction|emplo|aucune") | 
                     profession.st_22 %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Banque") ~ 242,
    is.na(ISCO) & (str_detect(profession.st_22, "fonction|emplo|aucune") |
                     profession.st_22 %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(profession.st_22, "bureau|commerc|immobil") ~ 411,
    is.na(ISCO) & (str_detect(profession.st_22, "fonction|emplo|aucune") | 
                     profession.st_22 %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "transports") ~ 832,
    is.na(ISCO) & (str_detect(profession.st_22, "fonction|emplo|aucune|analyst") | 
                     profession.st_22 %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Information et communication") ~ 251,
    is.na(ISCO) & str_detect(profession.st_22, "ong|bug") & str_detect(job_sector_other.st_22, "ong|organisation internationale") ~ 2635, #diplomats?
    
    
    # Government official
    is.na(ISCO) & str_detect(job_sector_other.st_22, "office des poursuites") ~ 335,
    
    ## Gestionnaire
    is.na(ISCO) & str_detect(profession.st_22, "gestion|administrat|responsabl|coordinat") & str_detect(job_sector.st_22, "Banque") ~ 241,
    is.na(ISCO) & str_detect(profession.st_22, "gestion|administrat|responsabl|coordinat|commerc") & str_detect(job_sector.st_22, "Administration publique") ~ 242,
    is.na(ISCO) & profession.st_22 %in% c("gestionnaire", "controleur de gestion", "business analyst") ~ 242,
    
    ## Public relations
    is.na(ISCO) & str_detect(profession.st_22, "communication|event manager|evenem") & !str_detect(profession.st_22, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "communication|event manager|evenem") & !str_detect(profession.st_22, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(profession.st_22, "production") ~ 243,
    
    # Commerce
    is.na(ISCO) & str_detect(profession.st_22, "vendeu") ~ 522,
    is.na(ISCO) & str_detect(profession.st_22, "merchandiser") ~ 524,
    is.na(ISCO) & str_detect(profession.st_22, "commerce|vent|agent|fonction|responsabl|magasin") & str_detect(job_sector.st_22, "Commerce") ~ 524,
    is.na(ISCO) & str_detect(profession.st_22, "marketing manager") ~ 242,
    is.na(ISCO) & str_detect(profession.st_22, "achat|achet") ~ 3323,
    is.na(ISCO) & str_detect(profession.st_22, "courtier") ~ 3324,
    is.na(ISCO) & str_detect(profession.st_22, "caiss") ~ 523,
    is.na(ISCO) & str_detect(profession.st_22, "sales|vent") ~ 243,
    is.na(ISCO) & str_detect(profession.st_22, "immobil") ~ 411,
    is.na(ISCO) & str_detect(profession.st_22, "commerc") ~ 243,
    is.na(ISCO) & str_detect(profession.st_22, "charge") & str_detect(profession.st_22, "client") ~ 4229,
    
    
    
    
    
    # Food
    is.na(ISCO) & str_detect(profession.st_22, "aide de cuisine") ~ 941,
    is.na(ISCO) & str_detect(profession.st_22, "cuisinier|cuisiner") ~ 5120,
    is.na(ISCO) & str_detect(profession.st_22, "laboratoire alimentaire") ~ 213,
    is.na(ISCO) & str_detect(profession.st_22, "boucher") ~ 7511,
    is.na(ISCO) & str_detect(profession.st_22, "bar|barman|cafet|serveu|restaura") ~ 513,
    
    
    
    # Cleaners / maids
    is.na(ISCO) & str_detect(profession.st_22, "demenage") ~ 9333,
    is.na(ISCO) & str_detect(profession.st_22, "aide menagere|femme de menage|maison|gouvernant|menage") ~ 911,
    is.na(ISCO) & str_detect(profession.st_22, "proprete hygiene|pro-rete et higiene|agent d'entretien|agent de nettoyage") ~ 912,
    
    
    # Other admin
    is.na(ISCO) & str_detect(profession.st_22, "administr") ~ 334,
    is.na(ISCO) & str_detect(profession.st_22, "logisti|magasinier") ~ 432,
    
    
    # Consultants
    is.na(ISCO) & str_detect(profession.st_22, "conseil") & str_detect(profession.st_22, "scienti") ~ 242,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") & str_detect(profession.st_22, "conseil|consult") ~ 2635,
    is.na(ISCO) & str_detect(profession.st_22, "consultant|business analyst|conseil") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "nations uni") & str_detect(profession.st_22, "advisor") ~ 242,
    
    
    
    # Other childcare
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") ~ 531,
    
    # Performing artists
    is.na(ISCO) & str_detect(profession.st_22, "pianist|music|musiq|orchest|opera|comedien|comdedien|chorist") ~ 265,
    is.na(ISCO) & str_detect(job_sector.st_22, "Arts") & str_detect(profession.st_22, "artist|mediat|decorat") ~ 265,
    
    # Handicraft
    is.na(ISCO) & str_detect(profession.st_22, "bijou|horlog|joaill") ~ 731,
    is.na(ISCO) & str_detect(profession.st_22, "fleurist") ~ 7549,
    
    
    # Construction
    is.na(ISCO) & str_detect(profession.st_22, "peint") ~ 713,
    is.na(ISCO) & str_detect(profession.st_22, "calculat|direction de travaux") ~ 3112,
    
    # Miscellaneous
    is.na(ISCO) & str_detect(profession.st_22, "animatrice") ~ 3412,
    is.na(ISCO) & str_detect(profession.st_22, "syndicalist|symdicalist") ~ 1114,
    
    # Unclassifiable
    is.na(ISCO) & profession.st_22 %in% c("pas d'activite non salariee","pas d'activite", "pas d'activite independante, seulement salarie",
                                      "aucun","non applicabel", "1 seule activite", "non, j'ai une activite salariee", 
                                      "pas d'aittes avtivites", "0", "::::", "je suis que salarie", "mon travail principal", 
                                      "pas  autre", "employe", "employee", "n/a", "pas d'activite non salariee", "actuellement j'ai une activite salariee",
                                      "non", "aucun", "aucune", "mere au foyer", "sans objet", ".", "ne souhaite pas repondre",
                                      "-", "--", "...", "?", "pas concerne", "na", "n.a.", "re´´´", "j'ai qu'une seule activite" 
    ) ~ -999,
    is.na(ISCO) & str_detect(profession.st_22, "pas de plusieurs activite") ~ 9999,
    
    # Final cleanup
    is.na(ISCO) & str_detect(job_sector.st_22, "Administration") ~ 731,
    is.na(ISCO) & str_detect(job_sector.st_22, "construction") ~ 711,
    is.na(ISCO) & str_detect(job_sector.st_22, "Banques") ~ 241,
    is.na(ISCO) & str_detect(job_sector.st_22, "Information et communication") ~ 243,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") ~ 3339,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") | str_detect(profession.st_22, "humanitarian") ~ 2635,
    is.na(ISCO) & str_detect(job_sector.st_22, "Hébergement") ~ 141,
    is.na(ISCO) & str_detect(job_sector.st_22, "Immobilier") ~ 3334,
    is.na(ISCO) & str_detect(job_sector.st_22, "Bureaux d’études") ~ 213,
    is.na(ISCO) & str_detect(job_sector.st_22, "production") ~ 311,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Commerce") ~ ,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Enseignement, recherche") ~ ,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Santé, social, médico-social") ~ ,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Other") ~ ,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Secteur de l’industrie") ~ ,
    
    .default = ISCO
  )) %>% 
  left_join(., occ_labels) %>%                      # Merge with ISCO occupations file
  relocate(Occupation_label, .after = profession.st_22) %>% 
  select(-n) %>% 
  # filter(
  #   !is.na(Occupation_label)
  #                       , !ISCO %in% c(9999)
  #                       ) %>%      # Remove rows with unassigned ISCO codes, to work on what's left
  mutate(ISCO = case_when(is.na(ISCO) ~ -999, 
                                      .default = ISCO)
         ) %>% 
  add_count(
    job_sector.st_22,
    # profession.st_22, 
    sort = FALSE) %>% 
  arrange(desc(n), 
          # profession.st_22, 
          job_sector.st_22) #%>% 
  # select(profession.inc, profession.st_22, profession_other.inc, ISCO, job_sector.st_22, supervision.st_22, n)  # remove this line once you've got it all done

# Finalizing the file ####

# In the final file, keep only the relevant columns that will be merged with our full dataset
occup_ISCO_final <- occup_ISCO %>% select(codbar, ISCO)

# # Read teleworkability indices from locally saved file
indices <- read_csv(here("data", "Initial datasets", "Telework ISCO indices.txt")) %>%     # read in the teleworkability indices (low "physical_interaction" = low teleworkability)
  janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# Read teleworkability indices from original source
# indices <- read_csv("https://raw.githubusercontent.com/m-sostero/telework-occupations/master/Telework%20ISCO%20indices.csv") %>%     # read in directly from source GitHub page
#   janitor::clean_names() %>% select(!occupation_title) %>% mutate(isco_3 = isco08) %>% select(-isco08)
# Read in ISCO code occupation labels
indices_2 <- read_xlsx(here("data", "Initial datasets", "EWCS 3 digit.xlsx")) %>% mutate(isco_3 = as.numeric(isco3d)) %>% select(isco_3,telework, physicalb)
# Merge the indices with the ISCO labels
indices <- left_join(indices, indices_2)

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

# # Save the final dataset
saveRDS(occup_ISCO_final, here("data","Generated datasets", "Classified_occupations.RDS"), ascii = TRUE)

