### Functions to calculate time since first case and time since first policy ###

# The first function calculates the time (in days) between the current day and:
# (1) the day when the first policy was applied
# (2) the day when the first case was confirmed
# (3) the day when the first death was confirmed 
# for the OxCGRT data set

# The function does only take policies on items s1 to s7 into account 
# and does not distinguish whether it was a general or local policy

# It takes policy_value as an argument (can be 1, 2 or 3) 
# which indicates the strenth of the first policy

# Please check the website of the database for further details on each policy strength


# The second function calculates the time (in days) between the current day and
# (1) the day the first case was confirmed
# (2) the day the first death was confirmed
# (3) the day the first recovery was confirmed
# for the CSSE data set


library(tidyverse)

time_since_first_OxCGRT <- function(policy_value = 1) {
  
  if(policy_value > 3 || policy_value < 1) stop("Argument 'policy_value' must be 1, 2, or 3!")
  
  ox2<- read.csv("data/OxCGRT/OxCGRT_Oxford_regulation_policies.csv")
  
  today <- as.Date(Sys.Date())
  
  first_policy_data <- ox2%>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    mutate(date = str_sub(var, -10),
           date = as.Date(paste(str_sub(date, 1, 4), 
                                str_sub(date, 6, 7), 
                                str_sub(date, 9, 10), sep = "-")),
           code = str_extract(var, "s[:digit:]"),
           is_general = str_extract(var, "isgeneral"),
           is_general = ifelse(is.na(is_general), "any", is_general)) %>%
    filter(!is.na(code) & is_general == "any" & code != "s8" & code != "s9") %>%
    group_by(country, date) %>%
    mutate(policy = any(value >= policy_value & !is.na(value))) %>%
    group_by(country, countryiso3) %>%
    summarise(time_since_first_policy = today - first(date[policy]))
  
  first_case_death_data <- ox2%>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    mutate(date = str_sub(var, -10),
           date = as.Date(paste(str_sub(date, 1, 4), 
                                str_sub(date, 6, 7), 
                                str_sub(date, 9, 10), sep = "-")),
           code = ifelse(str_detect(var, "confirmedcases"), "cases", 
                         ifelse(str_detect(var, "confirmeddeaths"), "deaths", NA))) %>%
    filter(!is.na(code)) %>%
    pivot_wider(id_cols = c("country", "countryiso3", "date"), names_from = code, values_from = value) %>%
    group_by(country, countryiso3)  %>%
    summarise(time_since_first_case = today - first(date[cases > 0 & !is.na(cases)]),
              time_since_first_death = today - first(date[deaths > 0 & !is.na(deaths)]))
  
  first_data <- first_policy_data %>%
    left_join(first_case_death_data, by = c("country", "countryiso3")) %>%
    arrange(countryiso3)
  
  write.csv(first_data, file.path("data", "OxCGRT", "time_since_first_OxCGRT.csv"), row.names = FALSE)
}


time_since_first_CSSE <- function() {
  
  csse <- read.csv("data/CSSE/CSSE.csv")
  
  today <- as.Date(Sys.Date())
  
  first_data <- csse %>%
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
    summarise(time_since_first_case = today - first(date[cases > 0 & !is.na(cases)]),
              time_since_first_death = today - first(date[deaths > 0 & !is.na(deaths)]),
              time_since_first_recovered = today - first(date[recovered > 0 & !is.na(deaths)]))
  
  write.csv(first_data, file.path("data", "CSSE", "time_since_first_CSSE.csv"), row.names = FALSE)
}

time_since_first_OxCGRT()
time_since_first_CSSE()