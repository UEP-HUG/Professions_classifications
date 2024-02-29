# Example dataset
data <- tibble(line = 1:2, text = c("This is a sample sentence.", "Another example here!"))
sw <- tibble(word = stopwords("en"))

# Tokenize and remove stopwords
data_clean <- data |> 
  unnest_tokens(word, text) |> 
  anti_join(sw) |> 
  group_by(line) |> 
  mutate(sentence = paste(word, collapse = " ")) |> 
  ungroup() |> 
  select(-word) |> 
  distinct()
  

data_clean <- data %>%
  unnest_tokens(word, text)


occup2 <- occup |> 
  unnest_tokens(word, master_profession) |> 
  anti_join(tibble(word = stopwords("fr"))) |> 
  group_by(participant_id) |> 
  mutate(master_profession_2 = paste(word, collapse = " ")) |> 
  ungroup() |> 
  select(-word) |> 
  distinct()