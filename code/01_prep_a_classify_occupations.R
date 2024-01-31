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
  select(master_profession, job_sector.st_22, supervision.st_22,
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
    
    # Services managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "resource|ressource| rh | rh") ~ 1212,
    
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Ambassade") ~ 111,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Administration publique") ~ 1112,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "industrie|construction|production") ~ 132,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Information") ~ 133,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Enseignement|Santé|transports|Banques|Sécurité|comptabilité") ~ 134,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Hébergement") ~ 141,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Commerce") ~ 142,
    is.na(ISCO) & (str_detect(profession.inc, "Chef-fe") | master_profession %in% c("directeur","directrice","cadre", "manager") | 
                     (str_detect(master_profession, "fonctionnai|emplo|responsa|direct") & str_detect(supervision.st_22, "Oui, et"))) &
      str_detect(job_sector.st_22, "Immobilier|Other|Arts") ~ 143,
    is.na(ISCO) & str_detect(master_profession, "ceo") ~ 112,
    
    
    # Gym / Sports teacher
    is.na(ISCO) & str_detect(master_profession, "professeur de gym|educateur sportif") ~ 342,
    
    
    # HR workers
    is.na(ISCO) & str_detect(master_profession, "resource|ressource| rh | rh| hr|hr ") | master_profession %in% c("rh") ~ 242,
    is.na(ISCO) & str_detect(master_profession, "planific|recrut") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "administration et ressources humaines") ~ 242,
    
    
    # Health managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "infir|medecin|pharma") ~ 1342,
    is.na(ISCO) & str_detect(master_profession, "cadre de sante|directrice d'iepa") ~ 1342,
    
    # Nurses
    is.na(ISCO) & str_detect(master_profession, "infi|sage femme|sage-femme") ~ 222,
    # Doctors
    is.na(ISCO) & str_detect(master_profession, "medecin") ~ 221,
    
    
    # Care workers
    is.na(ISCO) & str_detect(master_profession, "aide") & str_detect(master_profession, "domic|soig|soin") ~ 532,
    is.na(ISCO) & str_detect(master_profession, "assc|ambulan|a.s.s.c.|soins ems|chauffeur bls aed|opticien|employee ems|asam iepa") ~ 325,
    
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "assistant") & str_detect(master_profession, "medic|dent|soins|soci") ~ 325,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "coordinat") ~ 325,
    
    # Social workers
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "social|educat|curat") ~ 2635,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "accompagn") ~ 3221,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "animat|socioprof|socioc|fonctionnaire a imad") ~ 3412,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & master_profession %in% c("ais") ~ 3412,
    is.na(ISCO) & str_detect(master_profession, "responsable animation") ~ 3412,
    is.na(ISCO) & str_detect(master_profession, "insertion") ~ 3412,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & master_profession %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") & master_profession %in% c("ase", "a$e") ~ 2635,
    is.na(ISCO) & master_profession %in% c("maitre de readaptation") ~ 2263,
    is.na(ISCO) & str_detect(master_profession, "orthoptiste") ~ 2267,
    is.na(ISCO) & str_detect(master_profession, "sociolog|anthropolog|archeolog") ~ 2632,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") & str_detect(master_profession, "develop") ~ 2635,
    
    # Religious worker
    is.na(ISCO) & str_detect(master_profession, "pastor|eglise|pasteur") & str_detect(master_profession, "assistant") ~ 3413,
    is.na(ISCO) & str_detect(master_profession, "pastor|eglise|pasteur") ~ 2636,
    
    
    # Education managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "enseign") ~ 1345,
    is.na(ISCO) & str_detect(master_profession, "direction d'ecole") ~ 1345,
    
    # Professors
    is.na(ISCO) & str_detect(master_profession, "professe|profress|prof a une universite") ~ 2310,
    is.na(ISCO) & str_detect(master_profession, "directeur d'une ecole de commerce") ~ 2320,
    
    # Childcare
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Petite enfance") & !str_detect(master_profession, "policier") ~ 1341,
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") & str_detect(master_profession, "educat|anima") ~ 5311,
    is.na(ISCO) & str_detect(master_profession, "educatrice petite enfance|petite enfance") ~ 5311,
    is.na(ISCO) & str_detect(job_sector.st_22, "Enseignement") & str_detect(master_profession, "primaire") ~ 2341,
    # Other teaching professionals
    is.na(ISCO) & str_detect(master_profession, "enseignement|enseignan| formation|formateur|formatrice") ~ 235,
    is.na(ISCO) & str_detect(master_profession, "educatrice specialisee") ~ 2352,
    
    
    # Scientists / Science professionals
    is.na(ISCO) & str_detect(master_profession, "biologist") ~ 2131,
    is.na(ISCO) & str_detect(master_profession, "laborant|laborat|labonratine") ~ 3212,
    is.na(ISCO) & str_detect(master_profession, "geolog|geog") ~ 2114,
    is.na(ISCO) & str_detect(master_profession, "geom") ~ 2165,
    is.na(ISCO) & str_detect(master_profession, "chercheu|checheu|scientifique en recherche et developpement") ~ 213,
    is.na(ISCO) & str_detect(master_profession, "rechercher|doctorant") ~ 213,
    is.na(ISCO) & str_detect(master_profession, "graphisme|graphidme") ~ 2166,
    
    
    
    # Project managers
    is.na(ISCO) & str_detect(master_profession, "chef|manager|responsable|charge|gestion|coordinat") & 
      str_detect(master_profession, "project | projet|product |produit|program") ~ 1219,
    is.na(ISCO) & str_detect(master_profession, "planificateur|directrice de projets") ~ 1219,
    
    # Accounting managers
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "comptab") ~ 1211,
    is.na(ISCO) & str_detect(master_profession, "comptab|audit|fiduci|facturist") ~ 2411,
    
    # Security services
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Sécurité") & str_detect(master_profession, "yoga teacher") ~ 3423,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(job_sector.st_22, "Sécurité") ~ 1349, # managers
    is.na(ISCO) & str_detect(job_sector.st_22, "Sécurité") & str_detect(master_profession, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(job_sector.st_22, "Sécurité") & str_detect(profession_other.inc, "polic|fonction|amb|sape|pomp|agent|securit|gard|-|gend|salar") ~ 541, # Other security
    is.na(ISCO) & str_detect(master_profession, "policier|police|garde bain|gendarm|douanier") ~ 541, # Other security
    is.na(ISCO) & str_detect(master_profession, "agent") & str_detect(master_profession, "surete|securite") ~ 541, # Other security
    is.na(ISCO) & master_profession %in% c("enqueteur") ~ 335,
    is.na(ISCO) & str_detect(master_profession, "pompier") ~ 541,
    
    
    # Assistants / receptionists
    is.na(ISCO) & str_detect(master_profession, "assistant") & str_detect(master_profession, "medic")  ~ 3256,
    is.na(ISCO) & str_detect(master_profession, "assistant") & str_detect(master_profession, "pharma")  ~ 3213,
    is.na(ISCO) & str_detect(master_profession, "assistant") & str_detect(master_profession, "denta")  ~ 3251,
    is.na(ISCO) & str_detect(master_profession, "secretaire|secretar") & str_detect(master_profession, "medic|pharma|denta")  ~ 3344,
    is.na(ISCO) & str_detect(master_profession, "assistant") & str_detect(master_profession, "administrative|direction|gestion|parlem|gesti|parl|execu|")  ~ 334,
    is.na(ISCO) & str_detect(master_profession, "adjoint|gestion") & str_detect(master_profession, "administr") ~ 334,
    is.na(ISCO) & str_detect(master_profession, "huiss") ~ 334,
    is.na(ISCO) & !str_detect(profession.inc, "Cadre") & str_detect(master_profession, "secretaire|secretar|telephonist") ~ 412,
    is.na(ISCO) & master_profession == "assistant" | master_profession == "assistante" ~ 334,
    is.na(ISCO) & str_detect(master_profession, "reception") ~ 4226,
    is.na(ISCO) & str_detect(master_profession, "hotesse") & str_detect(master_profession, "accueil") ~ 4226,
    is.na(ISCO) & str_detect(master_profession, "office manager") ~ 3341,
    
    # Other services
    is.na(ISCO) & str_detect(master_profession, "voyage") ~ 4221,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "tourism") ~ 4221,
    is.na(ISCO) & str_detect(master_profession, "photograp") ~ 3431,
    is.na(ISCO) & str_detect(master_profession, "cameraman|videast") ~ 3521,
    is.na(ISCO) & str_detect(master_profession, "telecommunication|telemat") ~ 3522,
    is.na(ISCO) & str_detect(master_profession, "videoconference") ~ 352,
    is.na(ISCO) & str_detect(master_profession, "funera") ~ 5163,
    
    
    # Other health professionals
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "dentist|dentaire|pharma|dietet|audiol|optom|ergother|physio") ~ 134,
    is.na(ISCO) & str_detect(master_profession, "dentist|dentaire") ~ 2261,
    is.na(ISCO) & str_detect(master_profession, "pharma|drogu") ~ 2262,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "preparatrice") ~ 3213,
    is.na(ISCO) & str_detect(master_profession, "dietet") ~ 2265,
    is.na(ISCO) & str_detect(master_profession, "logoped") ~ 2266,
    is.na(ISCO) & str_detect(master_profession, "audiol|optom|ergother") ~ 226,
    is.na(ISCO) & str_detect(master_profession, "physio") ~ 2264,
    # Psychologists / therapists / other
    is.na(ISCO) & str_detect(master_profession, "radiotherap") ~ 3211,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & str_detect(master_profession, "trm") ~ 3211,
    is.na(ISCO) & str_detect(master_profession, "psycho|therap") ~ 226,
    is.na(ISCO) & str_detect(master_profession, "sante en entreprise") ~ 2263,
    is.na(ISCO) & str_detect(master_profession, "senior emergency officer|medical affairs lead|regulateur sanitaire") ~ 2269,
    is.na(ISCO) & str_detect(job_sector.st_22, "Santé") & 
      str_detect(master_profession, "msp|aucun|responsable|affairs manager") ~ 2269,
    
    
    
    # Greffier / Juristes
    is.na(ISCO) & str_detect(master_profession, "greffier") | str_detect(profession_other.inc, "greffier") ~ 2619,
    is.na(ISCO) & str_detect(master_profession, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(profession_other.inc, "jurist|avocat|jurid|juge|magistrat") ~ 261,
    is.na(ISCO) & str_detect(master_profession, "compliance|conformite") ~ 2619,
    
    
    # IT
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "informatic|informatiq") ~ 1330,
    is.na(ISCO) & str_detect(master_profession, "informatic|informatiq|developpeur|helpdesk|helpdek") ~ 351,
    is.na(ISCO) & str_detect(master_profession, "information|programmatrice|solutions technologiques|solution engineer") ~ 251,
    is.na(ISCO) & master_profession %in% c("programmeur") ~ 251,
    is.na(ISCO) & master_profession %in% c("it", "consultant it", "developpeur") ~ 351,
    is.na(ISCO) & str_detect(master_profession, "charge de securite") ~ 2529,
    is.na(ISCO) & str_detect(master_profession, "conseil|administrat") & str_detect(master_profession, "donnee")  ~ 2511,
    # HR again
    is.na(ISCO) & str_detect(master_profession, "personn")  ~ 2423,
    
    
    
    
    # Banking / finance
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "banq") ~ 1346,
    is.na(ISCO) & str_detect(supervision.st_22, "Oui, et") & str_detect(master_profession, "financ") ~ 1211,
    is.na(ISCO) & str_detect(master_profession, "banq|fortune|financ|banker|bancair") ~ 241,
    is.na(ISCO) & str_detect(master_profession, "trader|trading") ~ 3311,
    is.na(ISCO) & str_detect(master_profession, "economist") ~ 2631,
    is.na(ISCO) & str_detect(master_profession, "patrimo|assuranc") ~ 241,
    is.na(ISCO) & str_detect(master_profession, "fiscalist|fscaliste|taxat") ~ 2411,
    is.na(ISCO) & str_detect(master_profession, "risk|risq") ~ 2413,
    is.na(ISCO) & str_detect(job_sector.st_22, "Banques") & str_detect(master_profession, "analyst") ~ 2413,
    is.na(ISCO) & str_detect(master_profession, "gerant d'investissements") ~ 241,
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
    is.na(ISCO) & str_detect(master_profession, "technicien") & str_detect(master_profession, "audiovis") ~ 352,
    is.na(ISCO) & str_detect(master_profession, "technicien") & str_detect(job_sector.st_22, "Santé") ~ 321,
    is.na(ISCO) & str_detect(master_profession, "technicien") & str_detect(job_sector.st_22, "Arts") ~ 265,
    is.na(ISCO) & str_detect(master_profession, "technicien") & str_detect(job_sector.st_22, "industrie|construction|production") ~ 311,
    is.na(ISCO) & str_detect(master_profession, "technicien") & str_detect(job_sector_other.st_22, "ascenseur") ~ 741,
    
    # Scientific collaborator
    is.na(ISCO) & str_detect(master_profession, "collabor|colabor|adjoint") & str_detect(master_profession, "scientif") ~ 213,
    is.na(ISCO) & str_detect(master_profession, "statisticien|analyste de donnees|data analyst") ~ 212,
    # is.na(ISCO) & str_detect(job_sector.st_22, "Enseignement") & str_detect(master_profession, "") ~ 212,
    
    
    
    # Architect
    is.na(ISCO) & str_detect(master_profession, "architect|urbanist") & !str_detect(master_profession, "informat") ~ 216,
    is.na(ISCO) & str_detect(profession_other.inc, "architect|urbanist") ~ 216,
    
    is.na(ISCO) & str_detect(master_profession, "designer industriel") ~ 2163,
    is.na(ISCO) & str_detect(master_profession, "dessin") ~ 3118,
    is.na(ISCO) & str_detect(master_profession, "designer") ~ 216,
    
    
    # Historian
    is.na(ISCO) & str_detect(master_profession, "historien") ~ 2633,
    
    # Authors / journalists / interpreters
    is.na(ISCO) & str_detect(master_profession, "journalist") ~ 2642,
    is.na(ISCO) & str_detect(master_profession, "scripte") ~ 2641,
    is.na(ISCO) & str_detect(master_profession, "interpret|traduc|linguist") ~ 2643,
    is.na(ISCO) & str_detect(master_profession, "redact|monteur news") ~ 264,
    
    
    # Library
    is.na(ISCO) & str_detect(master_profession, "biblioth|archiv|documental|musee") ~ 262,
    
    # Process
    is.na(ISCO) & str_detect(master_profession, "quality manager") ~ 7543,
    # is.na(ISCO) & str_detect(master_profession, "control") & str_detect(master_profession, "qualit") ~ 7543,
    is.na(ISCO) & str_detect(master_profession, "qualit") ~ 7543,
    
    # Transport workers
    is.na(ISCO) & (str_detect(master_profession, "conduct|chauffeur|chauffer") | str_detect(profession_other.inc, "conduct|chauffeur|chauffer")) &
      (str_detect(master_profession, "bus|tram|tpg") | str_detect(profession_other.inc, "bus|tram|tpg")) ~ 8331,
    is.na(ISCO) & str_detect(master_profession, "pilot") & str_detect(master_profession, "locom") ~ 8311,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(master_profession, "regulate") ~ 8312,
    is.na(ISCO) & str_detect(master_profession, "conduct|chauffeur|conduczeur") ~ 832,
    is.na(ISCO) & str_detect(master_profession, "agent de train") ~ 5112,
    is.na(ISCO) & str_detect(master_profession, "pilot") & str_detect(master_profession, "ligne") ~ 3153,
    is.na(ISCO) & str_detect(master_profession, "control") & str_detect(master_profession, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(profession_other.inc, "control") & str_detect(profession_other.inc, "air|aer") ~ 3154,
    is.na(ISCO) & str_detect(master_profession, "control") & str_detect(job_sector.st_22, "transport") ~ 5112,
    is.na(ISCO) & str_detect(master_profession, "escale|aerop") | str_detect(profession_other.inc, "escale|arop") ~ 511,
    is.na(ISCO) & str_detect(master_profession, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(profession_other.inc, "transitair") ~ 3331,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(master_profession, "horaire") ~ 4323,
    is.na(ISCO) & str_detect(master_profession, "gestionnaire parc vehicule|responsable du parc de vehicule|gestionnaire de parc mobilite") ~ 9623, # or 1324, # fleet manager?
    is.na(ISCO) & str_detect(master_profession, "adjoint responsable reseau") ~ 2164,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "ingenieure mobilite") ~ 2164,
    is.na(ISCO) & str_detect(master_profession, "secteur transports|cff") ~ 3339,
    
    
    
    
    
    
    # Engineers
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "electr") ~ 215,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "telecom") ~ 2153,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "civil") ~ 2142,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "agron") ~ 2143,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "mecani") ~ 2144,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "chem") ~ 2145,
    is.na(ISCO) & str_detect(master_profession, "ingenie") & str_detect(master_profession, "medic") ~ 2149,
    is.na(ISCO) & str_detect(master_profession, "ingenie|engen|engineer") & str_detect(master_profession, "inform|infom|system|it|reseau|logiciel|banking") ~ 252,
    is.na(ISCO) & str_detect(master_profession, "ingenie") ~ 214,
    is.na(ISCO) & str_detect(master_profession, "transferts de technologie") ~ 252,
    is.na(ISCO) & str_detect(master_profession, "chimi") ~ 2145,
    is.na(ISCO) & str_detect(master_profession, "dispatcher en electricite") ~ 3113,
    
    
    
    # Electrician
    is.na(ISCO) & str_detect(master_profession, "electricien") ~ 741,
    is.na(ISCO) & str_detect(master_profession, "electr") & str_detect(master_profession, "meca") ~ 7412,
    is.na(ISCO) & str_detect(master_profession, "electronicien") ~ 742,
    
    
    # Mechanic
    is.na(ISCO) & str_detect(master_profession, "meca|depanneur|machinist") ~ 723,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") & str_detect(master_profession, "technicien") ~ 723,
    is.na(ISCO) & str_detect(master_profession, "serrurier-constucteur") ~ 721,
    
    
    # Plumber
    is.na(ISCO) & str_detect(master_profession, "plomb") ~ 7126,
    
    
    
    # Gardener / agriculture
    is.na(ISCO) & str_detect(master_profession, "jardinier|horticult|viti|vigner") ~ 611,
    is.na(ISCO) & str_detect(master_profession, "ouvrier agricole") ~ 921,
    is.na(ISCO) & str_detect(master_profession, "oenologue") ~ 2132,
    is.na(ISCO) & str_detect(master_profession, "cavist") ~ 816,
    
    
    # Concierge
    is.na(ISCO) & str_detect(master_profession, "concierge") ~ 5153,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "concierge") ~ 515,
    
    
    # Postal worker / courier
    is.na(ISCO) & str_detect(master_profession, "poste|posta") | str_detect(profession_other.inc, "poste|posta") ~ 4412,
    is.na(ISCO) & str_detect(master_profession, "coursier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other.inc, "courrier") ~ 9331,
    is.na(ISCO) & str_detect(profession_other.inc, "livreur|livreuse|bagagist") ~ 9621,
    is.na(ISCO) & str_detect(master_profession, "agent de distribution") ~ 4412,
    
    ## Fonctionnaire / employe
    is.na(ISCO) & str_detect(master_profession, "fonctionn|fonnctionn") & str_detect(master_profession, "international") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(profession_other.inc, "fonctionn") & str_detect(profession_other.inc, "onu") ~ 1112, #diplomats?
    is.na(ISCO) & str_detect(master_profession, "diplomat") ~ 1112,
    is.na(ISCO) & (str_detect(master_profession, "fonction|emplo|aucune") | 
                     master_profession %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Ambassade|Administration publique") ~ 242,
    is.na(ISCO) & (str_detect(master_profession, "fonction|emplo|aucune") | 
                     master_profession %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Banque") ~ 242,
    is.na(ISCO) & (str_detect(master_profession, "fonction|emplo|aucune") |
                     master_profession %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(master_profession, "bureau|commerc|immobil") ~ 411,
    is.na(ISCO) & (str_detect(master_profession, "fonction|emplo|aucune") | 
                     master_profession %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "transports") ~ 832,
    is.na(ISCO) & (str_detect(master_profession, "fonction|emplo|aucune|analyst") | 
                     master_profession %in% c(".", "-", "n.a.", "na", "n/a", "--", "...", "xx", "xxx", "non", "rien")) &
      str_detect(job_sector.st_22, "Information et communication") ~ 251,
    is.na(ISCO) & str_detect(master_profession, "ong|bug") & str_detect(job_sector_other.st_22, "ong|organisation internationale") ~ 2635, #diplomats?
    
    
    # Government official
    is.na(ISCO) & str_detect(job_sector_other.st_22, "office des poursuites") ~ 335,
    
    ## Gestionnaire
    is.na(ISCO) & str_detect(master_profession, "gestion|administrat|responsabl|coordinat") & str_detect(job_sector.st_22, "Banque") ~ 241,
    is.na(ISCO) & str_detect(master_profession, "gestion|administrat|responsabl|coordinat|commerc") & str_detect(job_sector.st_22, "Administration publique") ~ 242,
    is.na(ISCO) & master_profession %in% c("gestionnaire", "controleur de gestion", "business analyst") ~ 242,
    
    ## Public relations
    is.na(ISCO) & str_detect(master_profession, "communication|event manager|evenem") & !str_detect(master_profession, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "communication|event manager|evenem") & !str_detect(master_profession, "telecommunication") ~ 2432,
    is.na(ISCO) & str_detect(master_profession, "production") ~ 243,
    
    # Commerce
    is.na(ISCO) & str_detect(master_profession, "vendeu") ~ 522,
    is.na(ISCO) & str_detect(master_profession, "merchandiser") ~ 524,
    is.na(ISCO) & str_detect(master_profession, "commerce|vent|agent|fonction|responsabl|magasin") & str_detect(job_sector.st_22, "Commerce") ~ 524,
    is.na(ISCO) & str_detect(master_profession, "marketing manager") ~ 242,
    is.na(ISCO) & str_detect(master_profession, "achat|achet") ~ 3323,
    is.na(ISCO) & str_detect(master_profession, "courtier") ~ 3324,
    is.na(ISCO) & str_detect(master_profession, "caiss") ~ 523,
    is.na(ISCO) & str_detect(master_profession, "sales|vent") ~ 243,
    is.na(ISCO) & str_detect(master_profession, "immobil") ~ 411,
    is.na(ISCO) & str_detect(master_profession, "commerc") ~ 243,
    is.na(ISCO) & str_detect(master_profession, "charge") & str_detect(master_profession, "client") ~ 4229,
    
    
    
    
    
    # Food
    is.na(ISCO) & str_detect(master_profession, "aide de cuisine") ~ 941,
    is.na(ISCO) & str_detect(master_profession, "cuisinier|cuisiner") ~ 5120,
    is.na(ISCO) & str_detect(master_profession, "laboratoire alimentaire") ~ 213,
    is.na(ISCO) & str_detect(master_profession, "boucher") ~ 7511,
    is.na(ISCO) & str_detect(master_profession, "bar|barman|cafet|serveu|restaura") ~ 513,
    
    
    
    # Cleaners / maids
    is.na(ISCO) & str_detect(master_profession, "demenage") ~ 9333,
    is.na(ISCO) & str_detect(master_profession, "aide menagere|femme de menage|maison|gouvernant|menage") ~ 911,
    is.na(ISCO) & str_detect(master_profession, "proprete hygiene|pro-rete et higiene|agent d'entretien|agent de nettoyage") ~ 912,
    
    
    # Other admin
    is.na(ISCO) & str_detect(master_profession, "administr") ~ 334,
    is.na(ISCO) & str_detect(master_profession, "logisti|magasinier") ~ 432,
    
    
    # Consultants
    is.na(ISCO) & str_detect(master_profession, "conseil") & str_detect(master_profession, "scienti") ~ 242,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") & str_detect(master_profession, "conseil|consult") ~ 2635,
    is.na(ISCO) & str_detect(master_profession, "consultant|business analyst|conseil") ~ 242,
    is.na(ISCO) & str_detect(job_sector_other.st_22, "nations uni") & str_detect(master_profession, "advisor") ~ 242,
    
    
    
    # Other childcare
    is.na(ISCO) & str_detect(job_sector.st_22, "Petite enfance") ~ 531,
    
    # Performing artists
    is.na(ISCO) & str_detect(master_profession, "pianist|music|musiq|orchest|opera|comedien|comdedien|chorist") ~ 265,
    is.na(ISCO) & str_detect(job_sector.st_22, "Arts") & str_detect(master_profession, "artist|mediat|decorat") ~ 265,
    
    # Handicraft
    is.na(ISCO) & str_detect(master_profession, "bijou|horlog|joaill") ~ 731,
    is.na(ISCO) & str_detect(master_profession, "fleurist") ~ 7549,
    
    
    # Construction
    is.na(ISCO) & str_detect(master_profession, "peint") ~ 713,
    is.na(ISCO) & str_detect(master_profession, "calculat|direction de travaux") ~ 3112,
    
    # Miscellaneous
    is.na(ISCO) & str_detect(master_profession, "animatrice") ~ 3412,
    is.na(ISCO) & str_detect(master_profession, "syndicalist|symdicalist") ~ 1114,
    
    # Unclassifiable
    is.na(ISCO) & master_profession %in% c("pas d'activite non salariee","pas d'activite", "pas d'activite independante, seulement salarie",
                                      "aucun","non applicabel", "1 seule activite", "non, j'ai une activite salariee", 
                                      "pas d'aittes avtivites", "0", "::::", "je suis que salarie", "mon travail principal", 
                                      "pas  autre", "employe", "employee", "n/a", "pas d'activite non salariee", "actuellement j'ai une activite salariee",
                                      "non", "aucun", "aucune", "mere au foyer", "sans objet", ".", "ne souhaite pas repondre",
                                      "-", "--", "...", "?", "pas concerne", "na", "n.a.", "re´´´", "j'ai qu'une seule activite" 
    ) ~ -999,
    is.na(ISCO) & str_detect(master_profession, "pas de plusieurs activite") ~ 9999,
    
    # Final cleanup
    is.na(ISCO) & str_detect(job_sector.st_22, "Administration") ~ 731,
    is.na(ISCO) & str_detect(job_sector.st_22, "construction") ~ 711,
    is.na(ISCO) & str_detect(job_sector.st_22, "Banques") ~ 241,
    is.na(ISCO) & str_detect(job_sector.st_22, "Information et communication") ~ 243,
    is.na(ISCO) & str_detect(job_sector.st_22, "transports") ~ 3339,
    is.na(ISCO) & str_detect(job_sector.st_22, "Ambassade") | str_detect(master_profession, "humanitarian") ~ 2635,
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
  relocate(Occupation_label, .after = master_profession) %>% 
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
    # master_profession, 
    sort = FALSE) %>% 
  arrange(desc(n), 
          # master_profession, 
          job_sector.st_22) #%>% 
  # select(profession.inc, master_profession, profession_other.inc, ISCO, job_sector.st_22, supervision.st_22, n)  # remove this line once you've got it all done

# Finalizing the file ####

# In the final file, keep only the relevant columns that will be merged with our full dataset
occup_ISCO_final <- occup_ISCO %>% select(codbar, ISCO) |> filter(ISCO != -999)

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

