### Functions to get most recent Worldbank data ###

# These functions retrieve the most recent observations for every country in the worldbank data
# Each funtion reads in the wide format data file and outputs a wide format datafile
# The output file has two columns for every variable (summarised over years) in the input file:
# The first column indicates the year of the most recent observation, 
# the second column indicates the value of that observation
# If a country has no observations for a variable, NA is returned


library(tidyverse)

get_recent_wb_gov <- function() {

  wb_gov <- read.csv("data/WB_gov/wb_government_effectiveness.csv")
  
  recent_data <- wb_gov %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  write.csv(recent_data, file.path("data", "WB_GOV", "recent_wb_government_effectiveness.csv"), row.names = FALSE)
}

get_recent_wb_wdi <- function() {

  wb.wdi <- read.csv("data/WB_WDI/wdi.csv")
  
  recent_data <- wb.wdi %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    mutate(year = str_sub(var, -4),
           code = str_sub(var, 1, -6)) %>%
    mutate(country = fct_inorder(country), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, code)  %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = code, values_from = c("latest.year", "latest.value"))
  
  write.csv(recent_data, file.path("data", "WB_WDI", "recent_wdi.csv"), row.names = FALSE)
}

get_recent_wb_gov()
get_recent_wb_wdi()

