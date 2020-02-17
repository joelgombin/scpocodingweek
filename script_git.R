# this is a script
library(tidyverse)

data <- read_csv(file = "./slides/data/Repertoire-national-des-elus.zip")

data_young <- data %>% 
  filter(Age <= 25)

data %>% 
  filter(Age <= 25) %>% 
  filter(`Code sexe` %in% "M") %>%
#  slice(1:10) %>% 
  arrange(Age) %>% 
  distinct(`Code sexe`, `Code profession`, Age, .keep_all = TRUE) %>% 
  mutate(etatcivil = paste(`Prénom de l'élu`, `Nom de l'élu`)) %>% 
  # select(-contains("profession")) %>% 
  # rename(prenom = `Prénom de l'élu`) %>% 
  View()


data %>% 
  group_by(`Code sexe`, `Libellé de la profession`) %>% 
  summarise(nombre = n(), age_moyen = mean(Age), min(Age), max(Age)) %>% 
  arrange(age_moyen) %>% 
  View()

data %>% 
  mutate(last_name = paste0(stringr::str_sub(`Nom de l'élu`, 1, 1), 
                            stringr::str_to_lower(
                              stringr::str_sub(`Nom de l'élu`, 2)
                              )
                            )
         ) %>% 
  View






