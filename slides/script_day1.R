library(readr)
library(tidyverse)
rne <- read_csv("./data/Repertoire-national-des-elus.csv")

rne %>% 
  filter(`Code sexe` %in% "F") %>% 
  mutate(`Code sexe` = recode(`Code sexe`, "F" = "Female", "M" = "Male")) %>% 
  arrange(`Date de naissance`) %>% 
#  select(-`Code profession`) %>% 
  group_by(`Libellé de la profession`) %>% 
  summarise(n = n(), age = mean(Age), sd_age = sd(Age)) %>% 
  arrange(desc(n))

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% 
  filter(value %in% TRUE) %>% 
  select(-value) %>% 
  View

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% 
  filter(value %in% TRUE) %>% 
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01"))) %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  summarise(age = mean(Age, na.rm = TRUE))

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% 
  filter(value %in% TRUE) %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(is.na(Age)) %>% 
  summarise(n = n())

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% 
  filter(value %in% TRUE) %>% 
  select(-value) %>% 
  group_by(mandat) %>% 
  filter(`Date de naissance` %in% lubridate::ymd("1900-01-01")) %>% 
  summarise(n = n())

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`:Maire) %>% 
  filter(value %in% TRUE) %>% 
  select(-value) %>% 
  group_by(Identifiant) %>% 
  summarise(offices = n(), occupation = unique(`Libellé de la profession`), gender = unique(`Code sexe`)) %>% 
  ungroup() %>% 
  group_by(occupation, gender) %>% 
  summarise(offices = mean(offices)) %>% 
  arrange(desc(offices))


rne %>% 
  filter(`Code sexe` %in% "F") %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(desc(Age)) %>% 
  slice(2) %>% 
  View

rne %>% 
  filter(`Code sexe` %in% "F") %>% 
  group_by(`Libellé de la profession`) %>% 
  arrange(desc(Age)) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank %in% 2) %>% 
  View

rne %>% 
  mutate(number = case_when(`Nombre de mandats` %in% 1 ~ "one",
                            `Nombre de mandats` %in% 2 ~ "two",
                            `Nombre de mandats` %in% 3 ~ "three",
                            `Nombre de mandats` %in% 4 ~ "four",
                            TRUE ~ NA_character_)) %>% 
  View

rne_f <- rne %>% 
  filter(`Code sexe` %in% "F")
rne_m <- rne %>% 
  filter(`Code sexe` %in% "M")


rne_binded <- bind_rows(rne_f, rne_m)

library(ggplot2)
rne %>% 
  ggplot(aes(x = `Code sexe`)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma)

rne %>% 
  count(`Libellé de la profession`, sort = TRUE) %>% 
  arrange(n) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% 
  ggplot(aes(x = occupation, y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()
