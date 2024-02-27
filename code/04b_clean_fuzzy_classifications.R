pacman::p_load(
  here, 
  tidyverse,
  tidytext
  )

# Read in files ####

## Read in the fuzzy matches dataset that needs to be cleaned ####
if (file.exists(here("output", "fuzzy_classified_occupations_to_clean_2024-02-26.rds"))) {
  occup_final <- readRDS(here("output", "fuzzy_classified_occupations_to_clean_2024-02-26.rds"))
  remaining_bad_matches <- readRDS(here("output", "remaining_bad_matches_2024-02-26.rds"))
} else {
  source(here("code", "04a_make_fuzzy_classifications.R"))
}

## Inclusion dataset ####
inclusion_filtered <- readRDS("P:/ODS/DMCPRU/UEPDATA/Specchio-COVID19/99_data/Bases_for_sharing/2023-10-11-1616_ALLincsVs012345_ALLparticipants.rds") |> 
  # Remove the V5_inclusion people, as these are bus_santé people and there are some duplicates
  filter(Origin != "V5_inclusion") |>
  # Filter for only participants in one of the relevant studies
  filter(serocov_pop | pop_pilote | serocov_schools | serocov_kids | serocov_work | sp2_novdec_2020 | sp3_juin_2021 | sp4_avril_2022) |>
  filter(!testeur) |>  # remove data produced by testers - I think Nick already did this
  filter(!str_starts(codbar, "T"))  |>  # Remove any people with codbar beginning with T (also testers)
  select(participant_id)

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
    master_profession_original %in% c("fonctionnaire, chef d'unite 240 personnes sous mes ordres.") ~ 1000,
    ### Chief executives, senior officials and legislators ####
    #### Legislators and senior officials ####
    master_profession_original %in% c("cadre relations internationales") ~ 111,
    ##### Senior government officials ####
    master_profession_original %in% c(
      "cheffe etat ge (ne souhaite pas preciser quel service)", "che service dip etat de geneve", 
      "administration culturelle", "administratrice culturelle", "chef d'unite adjoint manifestations (fonction publique)",
      "cheffe de service suppleante, employee de la confederation helvetique"
    ) ~ 1112,
    ##### Senior officials of special-interest organisations ####
    master_profession_original %in% c("cadre dans la fonction publique", "cadre etat de geneve", "cadre intermediaire dip",
                                      "cadre tpg", "directeur affaires reglementaires", "international officer",
                                      "je travaille comme responsable des partenariats pour une ong internatioanle",
                                      "gestion de programmes au sein d'une fondation internationale"
                                      ) ~ 1112, # DIP = département de l’instruction publique 
    master_profession_original %in% c("directeur- secretaire general", "administrateur cooperative logement",
                                      "gerant technique dans une cooperative", "directrice du service de l information et porte-parole de l'onu a geneve",
                                      "chef de partie") ~ 1114,
    #### Managing directors and chief executives ####
    master_profession_original %in% c("ceo", "adjointe au directeur commer ial", "adjoins du directeur", "manager alimentaire", 
                                      "patron d'entreprise", "trust manager", "direction", "economiste directrice generale", 
                                      "presidente d'un comite de directeurs (pro bono/benevolat)", "conseillere administrative") ~ 112,
    ### Administrative and commercial managers ####
    #### Business services and administration managers ####
    master_profession_original %in% c("chief risk officer", "gerante administrative", "senior manager", "chef de secteur non alimentaire", "chef de section",
                                      "chef de team", "gestionnaire (travail de bureau)", "sre suppleant du responsable equipe",
                                      "suppleante responsable", "suppleante responsable d'equipe", "suppleante responsable equipe") ~ 121,
    ##### Finance managers ####
    master_profession_original %in% c("chef de service service financier", "cfo", "finance manager",
                                      "gestionnaire des risques financiers", "gerant de fonds de placement",
                                      "gerant d'investissements", "directeur asset management", "chef des investissements",
                                      "gestionnaire de risque", "gestionnaire des risques", "responsable de l'investissement",
                                      "responsable des investissements") ~ 1211,
    ##### Human Resources managers ####
    master_profession_original %in% c("administratrice ressources humaines", "cheffe de service rh", "manager ressources humaines",
                                      "manager rh", "responsable de l'unite de recruitement et soutiens de staff") ~ 1212,
    ##### Business services and administration managers not elsewhere classified ####
    master_profession_original %in% c("gestionnaire sinistres rc corporelles", "airport duty manager", "duty terminal manager", 
                                      "manager departement operationnel", "chef d'entreprises de moins de 10 employes", 
                                      "chef de conduite des operations", "responsable continuite d'activite et gestion de crise") ~ 1219,
    
    #### Sales, marketing and development managers ####
    master_profession_original %in% c("cadre salarie et marchand independant", "commercial manager") ~ 122,
    master_profession_original %in% c("responsable de laboratoire") ~ 1223,
    ### Production and specialized services managers ####
    #### Production managers in agriculture, forestry and fisheries ####
    master_profession_original %in% c("") ~ 131,
    #### Manufacturing, mining, construction, and distribution managers ####
    master_profession_original %in% c("demurrage manager", "shipping manager", "terminal manager", "responsable d'un service de maintenance") ~ 132,
    master_profession_original %in% c("directeur, pharmacovigilance", "chef de l'imprimerie de l'onu", "responsable imprimerie") ~ 1321,
    master_profession_original %in% c("manager du department logistique", "chef de team logistique") ~ 1324,
    #### Information and communications technology service managers ####
    master_profession_original %in% c("directeur risk management et it dans societe financiere", "cadre superieur dans l'informatique",
                                      "senior manager it", "chef d'equipe informatique", "cheffe de service communication palexpo sa",
                                      "gestionnaire de risques informatiques et conseiller a la protection des donnees"
                                      ) ~ 133,
    #### Professional services managers ####
    master_profession_original %in% c("chef adjoint de secteur") ~ 134,
    ##### Child care services managers ####
    master_profession_original %in% c(
      "directrice de secteur petite enfance", "responsable de secteur parascolaire") ~ 1341,
    ##### Health services managers ####
    master_profession_original %in% c(
      "cadre de sante", "cadre superieur de sante", "cadres de direction, services de sante",
      "cqdre de sante", "directeur rse sante surete environnement", "directrice des soins",
      "sante publique haut fonctionnaire, onu (oms)") ~ 1342,
    ##### Aged care services managers ####
    master_profession_original %in% c("") ~ 1343,
    ##### Social welfare managers ####
    master_profession_original %in% c("chargee d'accueil sociale", "gerant social", "gerante sociale iepa", 
                                      "chef de donnez et recherche, programme de migratiin, oms", "cheffe de secteur socio educatif",
                                      "cheffe de secteur socio educatif-", "responsable centre hebergement urgence") ~ 1344,
    ##### Education managers ####
    master_profession_original %in% c("psychomotricienne a 20% et directice d'une ecole de danse 50%", "directeur centre de formation",
                                      "cheffe du service des ecoles et institutions pour l enfance") ~ 1345,
    ##### Financial and insurance services branch managers ####
    str_detect(master_profession_original, "cadre bancaire") ~ 1346,
    master_profession_original %in% c("cadre superieur banque privee", "cadre superieur en banque", "manager caisse", "manager caisse administration",
                                      "gestionnaire assurance maladie") ~ 1346,
    ##### Professional services managers not elsewhere classified ####
    master_profession_original %in% c("lieutenant de securite onu", "manager juridique") ~ 1349,
    ### Hospitality, retail and other services managers ####
    #### Hotel and restaurant managers ####
    master_profession_original %in% c("gerant d'arcade", "manager evenementiel et hebergement", 
                                      "responsable adjointe gestion des lits et du pool") ~ 141,
    master_profession_original %in% c("cheffe de cuisine et de la restauration") ~ 1412,
    #### Retail and wholesale trade managers ####
    master_profession_original %in% c("directeur de plusieurs filiales dans le commerce de detail du luxe", "directrice magasin", "editrice de litterture, directrice d'une maison independante d'edition") ~ 142,
    #### Other services managers ####
    master_profession_original %in% c("") ~ 143,
    master_profession_original %in% c("responsable d,un groupe de conducteurs") ~ 1439,
    
    ## **Professionals** ####
    ### Not easy to classify ####
    master_profession_original %in% c("chercheur doctorant", "adjoint scientifique", "adjointe scientifique", 
                                      "doctorant", "doctorante", "chercheur scientific", "chercheuse et doctorante a l'universite",
                                      "doctorant (arrete)") ~ 2000,
    ### Science and engineering professionals ####
    #### Physical and earth science professionals ####
    master_profession_original %in% c("") ~ 211,
    #### Mathematicians, actuaries and statisticians ####
    master_profession_original %in% c("fonctionnaire etat assistante-statistique") ~ 212,
    #### Life science professionals ####
    master_profession_original %in% c("") ~ 213,
    master_profession_original %in% c("directeur / botaniste") | 
      str_detect(master_profession_original, "bioinformati") ~ 2131,
    #### Engineering professionals (excluding electrotechnology) ####
    master_profession_original %in% c("engineer & project manager", "engineer projet manager") ~ 214,
    ##### Civil engineers ####
    master_profession_original %in% c("ingenieur en automatisme du batiment") ~ 2142,
    master_profession_original %in% c("aeronautique (navigabilite)") ~ 21441,
    master_profession_original %in% c("createur de parfum independant") ~ 2145,
    #### Electrotechnology engineers ####
    master_profession_original %in% c("") ~ 215,
    #### Architects, planners, surveyors and designers ####
    master_profession_original %in% c("concepteurs graphiques, multimedia - graphistes") ~ 216,
    master_profession_original %in% c("architecte adjointe de direction", "associe de mon bureau d'architecte", 
                                      "responsable du service technique - architecte", "salariee a la hes-so et architecte independante") ~ 2161,
    master_profession_original %in% c("ing transports") ~ 2164,
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
      "infirmiere de sante communautaire", "infirmiere sante travail", "infirmiere independante",
      "infitmiere de sante au travail", "infirmiere en sante communautaire", "inf.responsable d'equipe") ~ 2221,
    master_profession_original %in% c("chef de projet/cadre infirmier", "responsable de site / infirmiere cheffe") ~ 22211,
    ##### Midwifery professionals ####
    master_profession_original %in% c("sage femme", "sage-femme") ~ 2222,
    #### Traditional and complementary medicine professionals ####
    master_profession_original %in% c(
      "therapeute en mtc", "therapeute energeticienne", "acupunctrice", "acupunctutrice",
      "therapeute et conseillere en sejours linguistique", 
      "enseignant taiji - qi gong, reflexologue, acupuncteur") ~ 223,
    #### Paramedical practitioners ####
    master_profession_original %in% c("cadre paramedic") ~ 224,
    #### Veterinarians ####
    master_profession_original %in% c("") ~ 225,
    #### Other health professionals ####
    master_profession_original %in% c("independente nutrition holistique / therapeute de fleurs de bach / hypnotherapeute (hypnopraxie)") ~ 226,
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
    master_profession_original %in% c(
      "dieteticien", "dieteticienne", "dieteticiens et specialistes de la nutrition",
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
    master_profession_original %in% c("alpiniste - conferenciere", "auxiliaire de recherche et d'enseignement", "maitre assistant universite de geneve") ~ 231,
    master_profession_original %in% c("independant et assistant hes") ~ 232,
    master_profession_original %in% c("enseignant avec formation universitaire", "enseignante avec formation universitaire",
                                      "chercheuse et enseignante", "enseignante esii dip", "enseignante generaliste",
                                      "enseignant avec charge administrative (decanat)", 
                                      "monitrice d'encadrement en maison de quartier") ~ 233,
    master_profession_original %in% c("enseignante benevole", "etudiante, mais j'enseigne a l'ecole primaire tous les jeudis et vendredis pour mes stages.") ~ 234,
    master_profession_original %in% c("enseignante-chercheuse", "enseignant jusqu'en juin 22 puis seulement directeur de fondation") ~ 2341,
    master_profession_original %in% c("") ~ 235,
    master_profession_original %in% c("chargee de formation", "activite independante pour la conception d'examens, la gestion d'un site") ~ 2351,
    master_profession_original %in% c("enseignant d'allemand (niveau collegial/gymnasial)") ~ 2353,
    master_profession_original %in% c("comedienne et professeure au conservatoire de geneve") ~ 2354,
    master_profession_original %in% c("comedienne et professeur de danse", "enseigne la ceramique", 
                                      "enseignement de la poterie (travail manuel avec cfc)", "assistant danse", 
                                      "je suis coach independante et prof de danse le soir, salariee") ~ 2355,
    
    
    ### Business and administration professionals ####
    master_profession_original %in% c(
      "administratif financier", "conseil en gestion des risques", "gestionnaire de fortune indepedant",
      "gestionnaire financiere et administrative", "gestionnaire de risques", "gestionnaire financiere administrative",
      "responsable des risques et de la conformite", "responsable risques") ~ 241,
    master_profession_original %in% c("analyste en conformite", "assistante audit", "compliance manager", "chef de l'audit interne") ~ 2411,
    master_profession_original %in% c("independant - financial management services") ~ 2412,
    master_profession_original %in% c("2 jobs - secretaire sociale 50% + independante (consultante en entreprise) 50%") ~ 242,
    master_profession_original %in% c("analystes, gestion et organisation", "coseillier aupres de la direction") ~ 2421,
    master_profession_original %in% c("gestionnaire coordinateur pour l'utilisation de l'espace public de la ville de geneve",
                                      "gestionnaire de projets developpement durable") ~ 2422,

    master_profession_original %in% c("") ~ 243,
    master_profession_original %in% c("mediamanager actu rts") ~ 2431,
    ### Information and communications technology professionals ####
    master_profession_original %in% c("analyste developpeur", "masterdata") ~ 251,
    master_profession_original %in% c("data architect") ~ 2511,
    master_profession_original %in% c("project manager, informatique") ~ 25124,
    master_profession_original %in% c("") ~ 252,
    master_profession_original %in% c("gestionnaire du systeme d information rh") ~ 2522,
    master_profession_original %in% c("architecte reseaux informatique", "charge de surete operationnelle reseau", "manager informatics") ~ 2523,
    master_profession_original %in% c("expert it, pole surete securite") ~ 2529,
    
    
    ### Legal, social and cultural professionals ####
    #### Legal professionals ####
    master_profession_original %in% c("juriste et agent commercial") ~ 261,
    ##### Lawyers ####
    master_profession_original %in% c("fonctionnaire (avocat travaillant pour une organisation internationale)") ~ 2611,
    ##### Judges ####
    master_profession_original %in% c("juge a la cour") ~ 2612,
    ##### Legal professionals not elsewhere classified ####
    master_profession_original %in% c("analyste en criminalite", "responsable anti blanchiment") ~ 2619,
    #### Librarians, archivists and curators ####
    master_profession_original %in% c("agent de documentation", "assistante bibliothecaire documentaliste archiviste", 
                                      "juriste-documentaliste (& enseignant a 30%)") ~ 262,
    #### Social and religious professionals ####
    master_profession_original %in% c("") ~ 263,
    master_profession_original %in% c("historienne de l'art, redactrice a 50% salariee et 40 a 50 & independant") ~ 2633,
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
    master_profession_original %in% c("conseillere en relations humaines dans le cadre d'une agence matrimoniale que j'ai creee") ~ 2635,
    master_profession_original %in% c("auteurs, journalistes et linguistes") ~ 264,
    master_profession_original %in% c("monteuse tv et enseignante de fle benevole") ~ 2642,
    master_profession_original %in% c("language assistant aupres de l'oms", "tradcutrice independante") ~ 2643,
    #### Creative and performing artists ####
    master_profession_original %in% c("metteur en scene et therapeute", "artiste designer") ~ 265,
    master_profession_original %in% c("artiste et enseignant remplacant d'art plastique") ~ 2651,
    master_profession_original %in% c("chef de choeur") ~ 2652,
    
    
    ## **Technicians and associate professionals** ####
    
    ### Science and engineering associate professionals ####
    master_profession_original %in% c("charge de projets", "chargee de projet philanthropique") ~ 30001,
    master_profession_original %in% c("") ~ 311,
    master_profession_original %in% c("enfant : etudiante / pere : technicien en micro technique es") ~ 31151,
    master_profession_original %in% c("chargee projets efficience energetique") ~ 3119,
    master_profession_original %in% c("") ~ 312,
    master_profession_original %in% c("adjoint resp. carrosserie", "gerant carrosserie") ~ 3122,
    master_profession_original %in% c("charge de projets genie civil") ~ 31231,
    master_profession_original %in% c("") ~ 313,
    master_profession_original %in% c("") ~ 314,
    master_profession_original %in% c("") ~ 315,
    master_profession_original %in% c("aircraft coordinator", "agent autorite aviation", "agent d'assistance aeroportuaire",
                                      "aircraft coordinateur", "gestionnaire des aires de trafic") ~ 3154,
    ### Health Associate professionals ####
    #### Medical and pharmaceutical technicians ####
    master_profession_original %in% c("") ~ 321,
    ##### Medical imaging and therapeutic equipment technicians ####
    master_profession_original %in% c("assistante en audiologir", "assistante specialisee en bloc operatoire") ~ 3211,
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
      "assistante en soins et sante comunautaire", "assc urgences", "stage assistant en soins dans le cadre de mon diplome de maturite",
      "responsable de l'accompagnement et des soins"
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
      "opticien", "assistance maternel", "agent de sterilisation"
    ) ~ 325,
    master_profession_original %in% c("gestionnaire administrative surete",
                                      "responsable assurance qualite hygiene etsecurite") ~ 3257,
    
    #####  Ambulance workers ####
    master_profession_original %in% c(
      "ambulancier", "ambulancier dipl. es", "ambulancier diplome es",
      "ambulancier es", "ambulancier es / formateur fsea", "ambulancier et enseignant",
      "ambulanciere", "ambulanciere diplomee es", "ambulanciere es", "ambulanciere es et directrice",
      "ambulancieres", "etudiant ambulancier", "etudiante ambulanciere / technicienne ambulanciere",
      "pompiers ambulancier", "technicien ambulancier", "technicienne ambulanciere",
      "tecnicien ambulancier") ~ 3258,
    master_profession_original %in% c("medicsl affairs manager", "medical affairs manager") ~ 3259,
    ### Business and administration associate professionals ####
    master_profession_original %in% c("") ~ 331,
    master_profession_original %in% c("gestionnaire administrative - comptable", "gestionnaire comptable") ~ 3313,
    master_profession_original %in% c("assistante trading achat vente", "commodity trading", "senior salestrader",
                                      "trading", "gestionnaire de dossier", "gestionnaire de dossiers", "gestionnaire de dossiers ai",
                                      "gestionnaire de portefeuille") ~ 3311,
    master_profession_original %in% c("account director", "acheteuse assitante commerciale", "acc. manager", "independant courtier") ~ 332,
    master_profession_original %in% c("independant agent d'assurance") ~ 3321,
    master_profession_original %in% c("approvisionneur , acheteur") ~ 3323,
    
    master_profession_original %in% c("") ~ 333,
    master_profession_original %in% c("gestionnaire de congres medicales", "responsable evenementiel") ~ 3332,
    master_profession_original %in% c("agents d'emploi et de recrutement de main-d'oeuvre") ~ 3333,
    master_profession_original %in% c("gerant technique d'immeubles", "directeur immobilier", "chef de projet immobilier et administrateur de societes") ~ 3334,
    master_profession_original %in% c("coordinatrice de secteur", "administration, bureau de conference", 
                                      "assistant responsable admission accueil et gestion", "assistante de l'intendante cheffe") ~ 334,
    master_profession_original %in% c("charge de clientele", "chargee de clientele", "chargee de clientele b2b") ~ 3341,
    master_profession_original %in% c("assistante de direction - chargee des evenements et de la communication", "assistante executive",
                                      "courtier en petrole assistant"
                                      ) ~ 3343,
    master_profession_original %in% c("secretaire assistante medicale") ~ 3344,
    
    #### Regulatory government associate professionals ####
    master_profession_original %in% c("responsable surete surveillance") ~ 335,
    master_profession_original %in% c("ajointe au service social et communautaire d'une commune") ~ 3353,
    master_profession_original %in% c("agent de surete specialiste", "chef d'equipe surete", "officier de securite", 
                                      "responsable surete", "cadre superieur policier", "sergent chef d'equipe a la police municipale") ~ 3355,
    ### Legal, social, cultural and related associate professionals ####
    
    #### Legal, social and religious associate professionals ####
    master_profession_original %in% c("intervenante psychosociale", "animateur d'atelier", "educatrice et adjointe de direction") ~ 341,
    str_detect(master_profession_original, "huissier") | master_profession_original %in% c("avocat, assistant unige") ~ 3411,
    ##### Social work associate professionals ####
    master_profession_original %in% c("educatrice", "collaborateur social / securite a domicile", "responsable de pole socio-educatif",
                                      "coordinatrice de projets dans le milieu academique international", "international development",
                                      "professionnelle en developpement international", 
                                      "representative aupres l'onu pour organisation non-gouvernmentale internationale") ~ 3412,
    ##### Religious associate professionals ####
    str_detect(master_profession_original, "pastoral") ~ 34130,
    #### Sports and fitness workers ####
    master_profession_original %in% c("adjoint a la responsable des installetions sportives") ~ 342,
    master_profession_original %in% c("enseignant d'education physique", "enseignante en education physique") ~ 3422,
    master_profession_original %in% c("assistante medicale puis instructeur fitness") ~ 3423,
    #### Artistic, cultural and culinary associate professionals ####
    master_profession_original %in% c("accueil musee", "sample handling assistant") ~ 343,
    master_profession_original %in% c("responsable de cuisine") ~ 3434,
    master_profession_original %in% c("assistant a la mise en scene pour des productions d'art lyrique - intermittent du spectacle") ~ 3435,
    #### Information and communications technicians ####
    master_profession_original %in% c("activites de support (it, transports, achat, immobilier)") ~ 351,
    master_profession_original %in% c("") ~ 352,
    
    ## **Clerical support workers** ####
    ### General and keyboard clerks ####
    #### General office clerks ####
    master_profession_original %in% c("autres employes de type administratif", "apprenti employe de commerce",
                                      "employe ville de geneve", "employee a un organisation", "employee communale",
                                      "employee de bureau (assistante administrative )", "employee de bureau administrative",
                                      "collaborateur administratif", "collaboratrice administrative", "commis administratif",
                                      "commis administratif 3", "commis administrative", "commis administrative 3", "commise administrative",
                                      "commise administrative."
                                      ) ~ 411,
    #### Secretaries (general) ####
    master_profession_original %in% c("secretaire/enseignante", "secretaire patronal") ~ 412,
    #### Keyboard operators ####
    master_profession_original %in% c("") ~ 413,
    ### Customer services clerks ####
    master_profession_original %in% c("") ~ 421,
    master_profession_original %in% c("") ~ 422,
    master_profession_original %in% c("job d'etudiante, receptionniste pour un apparthotel") ~ 4224,
    master_profession_original %in% c("chargee d accueil", "responsable accueil reception") ~ 4226,
    ### Numerical and material recording clerks ####
    master_profession_original %in% c("aide comptable et administrative", "aide-comptable dans un structure de petit-enfance etatique") ~ 431,
    master_profession_original %in% c("assistante fiscale", "fiscaliste", "fiscaliste au sein d'une entreprise multi-nationale") ~ 4311,
    master_profession_original %in% c("gestionnaire salaires et assurance sociales", "gestionnaire salaires et assurances sociales") ~ 4313,
    master_profession_original %in% c("disposante", "disposante", "coordinateur logistique", "agent operations", "responsable logistique") ~ 432,
    master_profession_original %in% c("gestionnaire de sous-traitance et stock", "gestionnaire de stock (logisticien de stockage)",
                                      "gestionnaire de stock bloc operatoire") ~ 4321,
    master_profession_original %in% c("agent administratif - unite logistique") ~ 4323,
    ### Other clerical support workers ####
    master_profession_original %in% c("achats", "gestionnaire administrative des residants") ~ 441,
    str_detect(master_profession_original, "adjointe administrative - support rh") ~ 4416,
    master_profession_original %in% c("job d'etudiant dans l'administratif") ~ 4419,
    ## **Service and sales workers** ####
    ### Personal service workers ####
    master_profession_original %in% c("") ~ 511,
    str_detect(master_profession_original, "agent d'escale|agente d'escale|agent escale fret") ~ 5111,
    master_profession_original %in% c("assistance au pmr a l'aeroport", "gestionnaire en information voyageurs") ~ 5111,
    master_profession_original %in% c("") ~ 512,
    master_profession_original %in% c("") ~ 513,
    master_profession_original %in% c("employee de restaurant en ems") ~ 51312,
    master_profession_original %in% c("") ~ 514,
    str_detect(master_profession_original, "coiffeuse") ~ 5141,
    master_profession_original %in% c("") ~ 515,
    master_profession_original %in% c("employee en intendance ems") ~ 51512,
    #### Other personal services workers ####
    # master_profession_original %in% c("astro-psychologue") ~ 516,
    master_profession_original %in% c("aide formateur conducteur") ~ 5165,
    ### Sales workers ####
    master_profession_original %in% c("") ~ 521,
    master_profession_original %in% c("employee cash center", "manager retail netvork") ~ 522,
    master_profession_original %in% c("agent de billetterie", "ticketing manager") ~ 523,
    master_profession_original %in% c("") ~ 524,
    ### Personal Care workers ####
    ##### Child care workers and teachers' aides ####
    (str_detect(master_profession_original, "accueil") & str_detect(master_profession_original, "familial")) |
      master_profession_original %in% c("acceuillante familiale a la journee", "assistante parental") ~ 5311,
    master_profession_original %in% c("aides-enseignants", "assistant a l'integration scolaire") ~ 5312,
    ##### Personal care workers in health services ####
    master_profession_original %in% c("ase / planificateur / enseignant de sport") ~ 532,
    master_profession_original %in% c("a l'hospice", "aide soignante en ems") ~ 5321, 
    master_profession_original %in% c(
      "a$e", "accompagnante, aide a domicile", "accompagnant", "aide socio-educative"
    ) ~ 5322,
    master_profession_original %in% c("assistante animatrice", "employe de maison en ems", "etudiante et permanente nocturne (imad)",
                                      "permanente nocturne/ etudiante", "permanente nocturne et stagiaire medicale") ~ 5329,
    
    ### Protective services workers ####8
    master_profession_original %in% c("") ~ 541,
    master_profession_original %in% c("etudiante/pompier volontaire") ~ 5411,
    master_profession_original %in% c("appointee de gendarmerie") ~ 5412,
    master_profession_original %in% c("assistante chargee de securite", "securite", "securite accueil",
                                      "sergent securite onug", "coordonnateur securite", "security duty manager",
                                      "conductrice de chiens de detection/ cheffe d'equipe") ~ 5414,
    master_profession_original %in% c("responsable gardien de bains") ~ 5419,
    
    ## **Skilled agricultural, forestry and fishery workers** ####
    ### Market-oriented skilled agricultural workers ####
    master_profession_original %in% c("agri ulteur viticulteur") ~ 611,
    master_profession_original %in% c("independant: vigneron encaveur (ingenieur oenologue)") ~ 61122,
    master_profession_original %in% c("") ~ 612,
    master_profession_original %in% c("agriculteurs et ouvriers qualifies, cultures maraicheres") ~ 613,
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
    master_profession_original %in% c("artiste ceramique") ~ 7314,
    
    master_profession_original %in% c("") ~ 732,
    ### Electrical and electronic trades workers ####
    master_profession_original %in% c("employe sig") ~ 741,
    master_profession_original %in% c("") ~ 742,
    master_profession_original %in% c("electro-mecanicien") ~ 7421,
    ### Food processing, wood working, garment and other craft and related trades workers ####
    master_profession_original %in% c("") ~ 751,
    master_profession_original %in% c("") ~ 752,
    master_profession_original %in% c("") ~ 753,
    master_profession_original %in% c("") ~ 754,
    master_profession_original %in% c("assistante qualite") ~ 7543,
    
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
    master_profession_original %in% c("responsable de la maintenance des trams") ~ 8312,
    master_profession_original %in% c("livreur a domivile", "livreur employe polyvalent") ~ 832,
    master_profession_original %in% c("") ~ 833,
    master_profession_original %in% c("") ~ 834,
    master_profession_original %in% c("") ~ 835,
    
    ## **Elementary occupations** ####
    
    ### Cleaners and helpers ####
    master_profession_original %in% c("agent de nettoyage") ~ 911,
    master_profession_original %in% c("") ~ 912,
    ### Agricultural, forestry and fishery labourers ####
    master_profession_original %in% c("apprentissage fleuriste") ~ 9214,
    ### Labourers in mining, construction, manufacturing and transport ####
    master_profession_original %in% c("") ~ 931,
    master_profession_original %in% c("") ~ 932,
    master_profession_original %in% c("") ~ 933,
    ### Food preparation assistants ####
    master_profession_original %in% c("apprentissage cuisinier", "aide cousine") ~ 941,
    ### Street and related sales and service workers ####
    master_profession_original %in% c("") ~ 951,
    master_profession_original %in% c("") ~ 952,
    ### Refuse workers and other elementary workers ####
    master_profession_original %in% c("") ~ 961,
    master_profession_original %in% c("") ~ 962,
    master_profession_original %in% c("gestionnaire parc vehicule", "responsable du parc de vehicules ga") ~ 9629,
    
    ## **Military** ####
    master_profession_original %in% c("") ~ 011,
    master_profession_original %in% c("") ~ 021,
    master_profession_original %in% c("") ~ 031,
    
    ## **Not possible to define** ####
    master_profession_original %in% c(
      "actuellement mere au foyer - avant banquiere", "au foyer", "actuellement mere au foyer, avant cadre financier",
      "aucun", "aucune", "en activite", "pas d'activite non salariee","elle a 5 ans - pas pertinent!",
      "en formation", "en formation (enseignement)", "rechercheuse, couramment sans emploi",
      "bachelor epfl architecture, actuellement en stage obligatoire", "retraite", "etudiant", "etudiant...", "etudiante",
      "etudiante en medecine", "etudiante, stage en traduction", "je suis etudiante", "pc / etudiant medecine",
      "pas repondu", "je n'ai pas ce type d'activites (bug dans le questionnaire)", "je n'ai pas de conjoint",
      "je n'ai pas ete salarie depuis mon mariage", "je n'ai pas plusieurs activites", "je n'ai pas plusieurs emplois",
      "je n'ai qu'un emploi.", "je suis partie voyager apres mes etudes et suis a present au chomage. je n'ai pas encore travaille dans mon domaine",
      "stagiaire", "stagiaire en inclusion numerique", "stagiaire eu spad", "stagiaire msts", "independante", "independente",
      "independante et profession de formation intermediaire", "independante, profession non-manuelle, formation superieure",
      "recherche d'emploi, derniere profession independantes"
    ) |
      str_detect(master_profession_original, "etudiante a l'universite") ~ -999,
    
    
    .default = NA
  )) |>
  mutate(
    manually_classified = case_when(!is.na(ISCO_new) ~ TRUE, .default = FALSE),
    confidence = case_when(manually_classified ~ "High", .default = confidence),
    ISCO = case_when(is.na(ISCO_new) ~ ISCO, .default = as.integer(str_sub(ISCO_new, start = 1, end = 4)))
  ) |> 
  select(-Occupation_label) |> 
  left_join(occ_labels) |>                       # Merge with ISCO occupations file
  relocate(Occupation_label, .after = Name_fr) |> 
  # update the remaining_bad_matches object for reference
  arrange(master_profession_original) ; remaining_bad_matches <- inner_join(
    remaining_bad_matches, occup_final_cleaned |> 
      filter(is.na(ISCO)) |> 
      select(participant_id), by = "participant_id") |> 
  arrange(master_profession_original, participant_id) ; pc <- occup_final_cleaned |> filter(!is.na(ISCO)) |> distinct() |> 
  summarise(n = n_distinct(participant_id), percent = paste0(round(n / 7750, 3)*100, "%")) ; print(pc)

filtered_remaining <- inner_join(remaining_bad_matches, inclusion_filtered)

# Helper file for classifications ####
# a <- remaining_bad_matches |> filter(str_detect(master_profession_original, "formation|enseignant"))
b <- remaining_bad_matches |> 
  group_by(participant_id) |> 
  slice_min(id_index) |> 
  ungroup() |> 
  unnest_tokens(word, master_profession) |> 
  add_count(word, sort = TRUE)

# a <- filtered_remaining |> filter(str_detect(master_profession_original, "responsable|gestionnair|chef"))
a <- remaining_bad_matches |> filter(str_detect(master_profession_original, "consult|conseil"))
