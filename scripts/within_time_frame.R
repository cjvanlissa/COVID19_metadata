### Functions to calculate cases, deaths and recoveries in a time frame ###

# Takes the argument time to specify the end of the time frame from first case/death/recovery
# Calculates the confirmed cases/deaths/recoveries for the OxCGRT and CSSE data within that time frame
# Outputs a data frame with the numbers per country/region


library(tidyverse)

within_time_frame_OxCGRT <- function(time = NULL) {
  
  if(is.null(time)) stop("Argument 'time' must be specified!")
  
  ox2<- read.csv("data/OxCGRT/OxCGRT_Oxford_regulation_policies.csv")
  
  time_frame_OxCGRT <- ox2%>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    mutate(date = str_sub(var, -10),
           date = as.Date(paste(str_sub(date, 1, 4), 
                                str_sub(date, 6, 7), 
                                str_sub(date, 9, 10), sep = "-")),
           code = ifelse(str_detect(var, "confirmedcases"), "cases", 
                         ifelse(str_detect(var, "confirmeddeaths"), "deaths", NA))) %>%
    filter(!is.na(code)) %>%
    pivot_wider(id_cols = c("country", "countryiso3", "date"), names_from = code, values_from = value) %>%
    group_by(country, countryiso3) %>%
    filter(!is.na(date)) %>%
    summarise(cases_within_time = last(cases[date <= first(date[cases > 0 & !is.na(cases)]) + 7 & !is.na(date) & !is.na(cases)]),
              deaths_within_time = last(deaths[date <= first(date[deaths > 0 & !is.na(deaths)]) + 7 & !is.na(date) & !is.na(deaths)])) %>%
    arrange(countryiso3)
  
  return(time_frame_OxCGRT)
}


within_time_frame_CSSE <- function(time = NULL) {
  
  if(is.null(time)) stop("Argument 'time' must be specified!")
  
  csse <- read.csv("data/CSSE/CSSE.csv")
  
  time_frame_CSSE <- csse %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3", "region")) %>%
    mutate(country = fct_inorder(country), countryiso3 = fct_inorder(countryiso3), region = fct_inorder(region),
           date = str_sub(var, -10),
           date = as.Date(paste(str_sub(date, -4), 
                                str_sub(date, 1, 2), 
                                str_sub(date, 4, 5), sep = "-")),
           code = ifelse(str_detect(var, "confirmed"), "cases", 
                         ifelse(str_detect(var, "deaths"), "deaths", "recovered"))) %>%
    pivot_wider(id_cols = c("country", "countryiso3", "region", "date"), names_from = code, values_from = value) %>%
    group_by(country, countryiso3, region) %>%
    summarise(cases_within_time = last(cases[date <= first(date[cases > 0 & !is.na(cases)]) + 7 & !is.na(date) & !is.na(cases)]),
              deaths_within_time = last(deaths[date <= first(date[deaths > 0 & !is.na(deaths)]) + 7 & !is.na(date) & !is.na(deaths)]),
              recovered_within_time = last(recovered[date <= first(date[recovered > 0 & !is.na(recovered)]) + 7 & !is.na(date) & !is.na(recovered)]))
  
  return(time_frame_CSSE)
}


