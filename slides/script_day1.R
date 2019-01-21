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
