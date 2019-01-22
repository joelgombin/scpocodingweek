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

library(hrbrthemes)

cairo_pdf(file = "./plot1.pdf", width = 12, height = 7)
rne %>% 
  mutate(gender = recode(`Code sexe`, "M" = "Male", "F" = "Female")) %>% 
  count(`Libellé de la profession`, gender, sort = TRUE) %>% 
  filter(!is.na(`Libellé de la profession`)) %>%
  ungroup %>% 
  arrange(gender, n) %>% 
  filter(n > 1000) %>% 
  mutate(order = row_number()) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% 
  mutate(coord = if_else(n > 22000, n - 1000, n + 1000),
         colour = if_else(n > 22000, "white", "black")) %>% 
  ggplot(aes(x = order, y = n)) +
  geom_bar(aes(fill = gender), stat = "identity", width = 0.8) +
  scale_fill_discrete(guide = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  geom_text(aes(label = occupation, y = coord, colour = colour), hjust = "inward", vjust = "center", size = 2) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  facet_wrap(facets = vars(gender), scales = "free_y") +
  xlab("") +
  ylab("") +
  ylim(0, NA) +
  scale_x_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  labs(title = "Most elected officials are employees, farmers or retired.", subtitle = "Number of elected officials in France in 2018 by occupation.", caption = "Source: RNE (Ministère de l'intérieur), computation by Sciences Po students.")
dev.off()
