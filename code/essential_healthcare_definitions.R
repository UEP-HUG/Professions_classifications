pacman::p_load(
  here,
  tidyverse,
  flextable
)

# Standard format of our flextables -------
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", digits = 2, na_str = "NA")
  return(x)
}

occup_ISCO_final <- readRDS(here("data", "Classified_occupations.rds"))
key_occupations <- read_csv2(here("data", "key_occupations_ILO_ISCO2.csv"))

merged <- left_join(occup_ISCO_final, key_occupations, by = join_by("isco_2" == "ISCO_2")) |> 
  mutate(key_occupation = case_when(key_occupation ~ TRUE, .default = FALSE),
         WORK = case_when(serocov_work.inc | work_pilote.inc ~ "Yes", .default = "No")
  )


by_overall <- merged |> 
  count(key_occupation) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |> 
  mutate(WORK = "Overall") |> relocate(WORK)

by_work <- merged |> 
  group_by(WORK) |>
  count(key_occupation) |> 
  mutate(percent = paste0(round(n/sum(n)*100,1),"%")) |> 
  filter(WORK == "Yes")

rbind(by_work, by_overall) |> myflextable()
  
