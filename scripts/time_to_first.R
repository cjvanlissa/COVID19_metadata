### Functions to calculate time since first case and time since first policy ###

library(tidyverse)

ox <- read.csv("data/OxCGRT/OxCGRT_Oxford_regulation_policies.csv")

ox.dict <- read.csv("data/OxCGRT/data_dictionary.csv")

today <- as.Date(Sys.Date())

data <- ox %>%
  rename(country = countryname, countryiso3 = countrycode) %>%
  pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
  mutate(date = str_sub(var, -8),
         date = as.Date(paste(str_sub(date, 1, 4), 
                      str_sub(date, 5, 6), 
                      str_sub(date, 7, 8), sep = "-")),
         code = str_extract(var, "s[:digit:]"),
         is_general = str_extract(var, "isgeneral"),
         is_general = ifelse(is.na(is_general), "local", is_general)) %>%
  filter(!is.na(code) & is_general == "local" & code != "s8" & code != "s9") %>%
  group_by(country, date) %>%
  select(-c("var", "is_general")) %>%
  mutate(policy = any(value > 0 & !is.na(value))) %>%
  group_by(country) %>%
  summarise(time_policy = today - first(date[policy]))
  