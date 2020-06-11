library(gert)
library(rio)
library(readxl)
library(countrycode)
library(tidyr)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(data.table)
library(glue)
library(forcats)
library(wbstats)
library(lubridate)
library(dplyr)

# Identify columns with enough data (at least 0)
enough_data <- function(x) sum(!is.na(x)) >= 0
# Identify columns with no data
no_data <- function(x) any(!is.na(x)) 

# Get selected data
get_worldbank_open_data <- function(){
  get_wb_gov()
  get_wb_dev()
  get_wb_failed()
  get_wb_trade()
  get_wb_business()
  get_wb_logistics()
  get_wb_freedom()
  get_wb_governance()
  get_wb_institutional_profiles()
  get_wb_bureaucracy()
  get_wb_trade_dev()
  get_wb_press_free()
  get_wb_education()
  get_wb_gender()
  get_wb_tourism()
  get_wb_wttc()
  get_wb_pov_equity()
  get_wb_rural()
  get_wb_travel_services()
  get_wb_urban_population()
  get_wb_population_slums()
  get_wb_poverty_headcount()
  get_wb_net_migration()
  get_wb_mortality_rate()
  get_wb_incidence_tuberculosis()
  get_wb_HIV_percent()
  get_wb_GDP_capita()
  get_wb_imports_goods()
  get_wb_consumer_prices()
  get_wb_education_total()
  get_wb_education_student_primary()
  get_wb_education_student_secondary()
  get_wb_education_student_tertiary()
  get_wb_literacy_total()
  get_wb_population_15_64()
  get_wb_population_65_up()
  get_wb_life_expectancy()
  get_wb_employment_total()
  get_wb_adolescent_fertilty()
  get_wb_hospital_beds()
  get_wb_air_departures()
  get_wb_cellular_subscriptions()
  get_wb_population_density()
  get_wb_intl_tourism_expenditures()
  get_wb_employment_female()
  get_wb_employment_male()
}

# get_wb_wdi <- function(){
#   # Download and unzip
# 
#   funzipped <- unzip_from_web(this_url = "http://databank.worldbank.org/data/download/WDI_csv.zip")
# 
#   # Read files into objects with same name
#   for(this_file in funzipped){
#     eval(parse(text = paste0(gsub("-", "_", gsub("^.+\\/(.+)\\.csv$", "\\1", this_file)), " <- read.csv('", this_file, "', stringsAsFactors = FALSE)")))
#   }
#   names(WDIData)[1] <- "country"
#   wdi <- WDIData[, c("country", "Indicator.Code", grep("^X201[6789]", names(WDIData), value = TRUE))]
#   wdi <- pivot_longer(wdi, cols = grep("^X201[6789]", names(wdi), value = TRUE))
#   wdi <- pivot_wider(wdi, id_cols = "country", names_from = c("Indicator.Code", "name"))
#   names(wdi) <- gsub("_X", "_", names(wdi))
#   wdi$countryiso3 <- countrycode(wdi$country, origin = "country.name", destination = "iso3c")
#   wdi <- wdi[, c(1, ncol(wdi), 2:(ncol(wdi)-1))]
# 
# 
# # Malte Lueken's most recent function -------------------------------------
# 
#   recent_data <- wdi %>%
#     pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
#     mutate(year = str_sub(var, -4),
#            code = str_sub(var, 1, -6)) %>%
#     mutate(country = fct_inorder(country), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
#     group_by(country, countryiso3, code)  %>%
#     summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
#               latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
#     ungroup() %>%
#     pivot_wider(id_cols = c("country", "countryiso3"), names_from = code, values_from = c("latest.year", "latest.value"))
# 
# # End
# 
#   checkfilewrite(recent_data, "WB_WDI", "recent_wdi.csv")
#   checkfilewrite(data_dict(WDIData[!duplicated(WDIData$Indicator.Code), ], "Indicator.Code", "Indicator.Name"), "WB_WDI", "data_dictionary.csv")
#   checkfilewrite(wdi, "WB_WDI", "wdi.csv")
#   file.remove(funzipped)
# }

get_wb_gov <- function() {
  print(paste("Processing worldbank data", "GOV ...", sep = " "))
  # Download and unzip
  df <-
    rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/51/dump.csv")
  names(df) <- tolower(names(df))
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  dict <- df[, c("variable", "subindicator type", "indicator")]
  dict <- dict[!duplicated(dict$variable),]
  dict$description <-
    paste(dict$`subindicator type`, dict$indicator)
  dict <- dict[, c(1, 4)]
  
  gov <- select(df, 2, 1, variable, `2013`:`2018`)
  names(gov)[1:2] <- c("country" , "countryiso3")
  
  gov <- pivot_longer(gov, cols = grep("^20", names(gov), value = TRUE))
  gov <- pivot_wider(gov, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))

  # Select only columns with enough data
  gov <- gov %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  recent_data <- gov %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_GOV", "recent_wb_government_effectiveness.csv")
  checkfilewrite(dict, "WB_GOV", "data_dictionary.csv")
  checkfilewrite(gov, "WB_GOV", "wb_government_effectiveness.csv")
}


# "id":56,
# "title":"World Development Indicators",
# "description":"The primary World Bank collection of development indicators,
# compiled from officially-recognized international sources. It presents the most current and accurate global development
# data available, and includes national, regional and global estimates."

get_wb_dev <- function(){
  print(paste("Processing worldbank data", "DEV ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/56/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2013`:`2019`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)

  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_DEV", "recent_world_development_indicators.csv")
  checkfilewrite(dict, "WB_DEV", "data_dictionary.csv")
  checkfilewrite(data, "WB_DEV", "world_development_indicators.csv")
}


# "id":97,
# "title":"Failed States Index",
# "description":"The Fund for Peace is an independent, nonpartisan, non-profit research and educational
# organization that works to prevent violent conflict and promote sustainable security. They promote sustainable
# security through research, training and education, engagement of civil society, building bridges across diverse sectors,
# and developing innovative technologies and tools for policy makers.\r\n\r\nCombining social science techniques with
# information technology, they have produced the patented Conflict Assessment System Tool (CAST), a content analysis
# software product that provides a conceptual framework and a data gathering technique for measuring conflict risk.
# They produce The Failed States Index, an annual ranking of 177 countries across 12 indicators, that is published by
# Foreign Policy magazine."

get_wb_failed <- function(){
  print(paste("Processing worldbank data", "FAILED ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/97/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2013`:`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_FAILED", "recent_failed_states_index.csv")
  checkfilewrite(dict, "WB_FAILED", "data_dictionary.csv")
  checkfilewrite(data, "WB_FAILED", "failed_states_index.csv")
}


# "id":513,
# "title":"United Nations Conference on Trade and Development",
# "description":"UNCTAD produces more than 150 indicators and statistical time series essential for the analysis of: International trade,
# economic trends, foreign direct investment, external financial resources, population and labor force, commodities, information economy,
# creative economy and maritime transport. "

get_wb_trade <- function(){
  print(paste("Processing worldbank data", "TRADE ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/513/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`:`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  
  checkfilewrite(recent_data, "WB_TRADE", "recent_trade_and_development.csv")
  checkfilewrite(dict, "WB_TRADE", "data_dictionary.csv")
  checkfilewrite(data, "WB_TRADE", "trade_and_development.csv")
}


# "id":435,
# "title":"Doing Business",
# "description":"The objective of Doing Business is to measure the simplicity, efficiency and accessibility of the regulatory environment.
# It measures business regulations and their enforcement across 190 economies and selected cities at the subnational and regional level.
# The Doing Business project looks at domestic small and medium-size companies and measures the regulations applying to them through their life cycle.
# By gathering and analyzing comprehensive quantitative data to compare business regulation environments across economies and over time,
# Doing Business encourages economies to compete towards more efficient regulation; offers measurable benchmarks for reform; and serves as a resource
# for academics, journalists, private sector researchers and others interested in the business climate of each economy."

get_wb_business <- function(){
  print(paste("Processing worldbank data", "BUSINESS ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/435/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2013`:`2019`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_BUSINESS", "recent_doing_business.csv")
  checkfilewrite(dict, "WB_BUSINESS", "data_dictionary.csv")
  checkfilewrite(data, "WB_BUSINESS", "doing_business.csv")
}


# "id":50,
# "title":"Logistics Performance Index",
# "description":"The Logistics Performance Index is an interactive benchmarking tool created to help countries identify the challenges
# and opportunities they face in their performance on trade logistics and what they can do to improve their performance.  The LPI 2016 allows
# for comparisons across 160 countries. The LPI is based on a worldwide survey of operators on the ground (global freight forwarders and express
# carriers), providing feedback on the logistics friendliness of the countries in which they operate and those with which they trade. They combine
# in-depth knowledge of the countries in which they operate with informed qualitative assessments of other countries where they trade and experience
# of global logistics environment. Feedback from operators is supplemented with quantitative data on the performance of key components of the logistics
# chain in the country of work.\r\n\r\nThe LPI consists therefore of both qualitative and quantitative measures and helps build profiles of logistics
# friendliness for these countries. It measures performance along the logistics supply chain within a country and offers two different perspectives:
# international and domestic."

get_wb_logistics <- function(){
  print(paste("Processing worldbank data", "LOGISTICS ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/50/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, c(`2014`, `2016`, `2018`))
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_LOGISTICS", "recent_logistics_performance_index.csv")
  checkfilewrite(dict, "WB_LOGISTICS", "data_dictionary.csv")
  checkfilewrite(data, "WB_LOGISTICS", "logistics_performance_index.csv")
}


# id":997,"
# title":"Freedom House",
# "description":"Freedom House is an independent watchdog organization dedicated to the expansion of freedom and democracy around the world.
# They analyze the challenges to freedom, advocate for greater political rights and civil liberties, and support frontline activists to defend
# human rights and promote democratic change. They acts as a catalyst for greater political rights and civil liberties through a combination of
# analysis, advocacy, and action. Their research and analysis frame the policy debate in the United States and abroad on the progress and decline
# of freedom.\r\n\r\nFor each country and territory, Freedom in the World analyzes the electoral process, political pluralism and participation,
# the functioning of the government, freedom of expression and of belief, associational and organizational rights, the rule of law, and personal
# autonomy and individual rights."

get_wb_freedom <- function(){
  print(paste("Processing worldbank data", "FREEDOM ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/997/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2013`:`2018`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_FREEDOM", "recent_freedom_house.csv")
  checkfilewrite(dict, "WB_FREEDOM", "data_dictionary.csv")
  checkfilewrite(data, "WB_FREEDOM", "freedom_house.csv")
}


# "id":429,
# "title":"Global Indicators of Regulatory Governance",
# "description":"The Global Indicators of Regulatory Governance project is an initiative of the World Bank's Global Indicators Group,
# which produces a range of datasets and benchmarking products on regulations and business activity around the world.\r\n\r\nThe Global
# Indicators of Regulatory Governance project explores how governments interact with the public when shaping regulations that affect their
# business community. Concerned stakeholders could be professional associations, civic groups or foreign investors. The project charts how
# interested groups learn about new regulations being considered, and the extent to which they are able to engage with officials on the content.
# It also measures whether or not governments assess the possible impact of new regulations in their countries (including economic, social and
# environmental considerations) and whether those calculations form part of the public consultation. \r\n\r\nFinally, Global Indicators of Regulatory
# Governance capture two additional components of a predictable regulatory environment: the ability of stakeholders to challenge regulations,
# and the ability of people to access all the laws and regulations currently in force in one, consolidated place

get_wb_governance <- function(){
  print(paste("Processing worldbank data", "GOVERNANCE ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/429/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select column
  data <- select(df, 2, 1, variable, c(`2018`))
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_GOVERNANCE", "recent_regulatory_governance.csv")
  checkfilewrite(dict, "WB_GOVERNANCE", "data_dictionary.csv")
  checkfilewrite(data, "WB_GOVERNANCE", "regulatory_governance.csv")
}


# "id":999,
# "title":"Institutional Profiles Database",
# "description":"The \"Institutional Profiles Database\" (IPD) provides an original measure of countries' institutional characteristics through
# composite indicators built from perception data. It aims to quantifying countries' institutional characteristics by gathering the perception of
# experts through a questionnaire. \r\n\r\nIt covers 144 countries and contains 127 indicators, derived from 320 variables describing a broad range
# of institutional characteristics, structured in nine functions: 1) political institutions; 2) security, law and order, control of violence; 3)
# functioning of public administrations; 4) free operation of markets; 5) coordination of stakeholders, strategic vision and innovation; 6) security
# of transactions and contracts; 7) market regulation, social dialogue; 8) openness; 9) social cohesion and social mobility."

get_wb_institutional_profiles <- function(){
  print(paste("Processing worldbank data", "INSTITUTIONAL ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/999/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2012`:`2016`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_INSTITUTIONAL", "recent_institutional_profiles.csv")
  checkfilewrite(dict, "WB_INSTITUTIONAL", "data_dictionary.csv")
  checkfilewrite(data, "WB_INSTITUTIONAL", "institutional_profiles.csv")
}

# "id":4127,
# "title":"Worldwide Bureaucracy Indicators ",
# "description":"The Worldwide Bureaucracy Indicators (WWBI) is a dataset on public sector employment and wages that can help researchers and
# development practitioners gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector on
# the overall labor market, and the fiscal implications of the government wage bill. The WWBI aim to fill the gap in information on the personnel
# of the state by providing more objective measures drawing on administrative data and household surveys, thereby complementing existing, expert
# perception-based approaches.\r\n\r\nThe WWBI dataset encompass three categories of variables: the characteristics of public employment, wages,
# and the wage bill. These variables provide an important, though narrow, picture of the skills and incentives of bureaucrats. The characteristics
# of public employment include the number of public employees, their age, gender distributions, and academic qualifications."
get_wb_bureaucracy <- function(){
  print(paste("Processing worldbank data", "BUREAUCRACY ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/4127/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2013`:`2016`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End  
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_BUREAUCRACY", "recent_bureaucracy_indicators.csv")
  checkfilewrite(dict, "WB_BUREAUCRACY", "data_dictionary.csv")
  checkfilewrite(data, "WB_BUREAUCRACY", "bureaucracy_indicators.csv")
}


# "id":513,
# "title":"United Nations Conference on Trade and Development",
# "description":"UNCTAD produces more than 150 indicators and statistical time series essential for the analysis of: International trade, economic
# trends, foreign direct investment, external financial resources, population and labor force, commodities, information economy, creative economy
# and maritime transport.
get_wb_trade_dev <- function(){
  print(paste("Processing worldbank data", "TRADE ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/513/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`:`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_TRADE_DEV", "recent_trade_and_development.csv")
  checkfilewrite(dict, "WB_TRADE_DEV", "data_dictionary.csv")
  checkfilewrite(data, "WB_TRADE_DEV", "trade_and_development.csv")
}


# "id":1000,
# "title":"Press Freedom Index by Reporters without Borders",
# "description":"Based in Paris, Reporters Without Borders (RSF) is an independent NGO with consultative status with the United Nations, 
# UNESCO, the Council of Europe and the International Organization of the Francophonie (OIF). Its foreign sections, its bureau in ten cities, 
# including Brussels, Washington, Berlin, Tunis, Rio de Janeiro, and Stockholm, and its network of correspondents in 130 countries give RSF 
# the ability to mobilize support, challenge governments and wield influence both on the ground and in the ministries and precincts where media 
# and Internet standards and legislation are drafted."}

get_wb_press_free <- function(){
  print(paste("Processing worldbank data", "PRESS_FREE ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/1000/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`:`2019`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_PRESS_FREE", "recent_press_freedom_index.csv")
  checkfilewrite(dict, "WB_PRESS_FREE", "data_dictionary.csv")
  checkfilewrite(data, "WB_PRESS_FREE", "press_freedom_index.csv")
}

# "id":748,
# "title":"Education Statistics"
# "description":"The World Bank EdStats All Indicator Query holds over 4,000 internationally comparable indicators that describe education access, 
# progression, completion, literacy, teachers, population, and expenditures. The indicators cover the education cycle from pre-primary to vocational 
# and tertiary education.The query also holds learning outcome data from international and regional learning assessments (e.g. PISA, TIMSS, PIRLS), 
# equity data from household surveys, and projection/attainment data to 2050. For further information, please visit the EdStats website."},

get_wb_education <- function(){
  print(paste("Processing worldbank data", "EDUCATION ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/748/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`:`2018`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_EDUCATION", "recent_education_statistics.csv")
  checkfilewrite(dict, "WB_EDUCATION", "data_dictionary.csv")
  checkfilewrite(data, "WB_EDUCATION", "education_statistics.csv")
}

# "id":747,
# "title":"Gender Statistics"
# "description":"The Gender Statistics database is a comprehensive source for the latest sex-disaggregated data and gender statistics covering demography, 
# education, health, access to economic opportunities, public life and decision-making, and agency."},,

get_wb_gender <- function(){
  print(paste("Processing worldbank data", "GENDER ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/747/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2014`:`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  
  checkfilewrite(recent_data, "WB_GENDER", "recent_gender_statistics.csv")
  checkfilewrite(dict, "WB_GENDER", "data_dictionary.csv")
  checkfilewrite(data, "WB_GENDER", "gender_statistics.csv")
}


# "id":78,
# "title":"Travel & Tourism Competitiveness Index"
# "description":"The World Economic Forum has, for the past 11 years, engaged leaders in travel and tourism to carry out an in-depth analysis of the Travel and 
# Tourism competitiveness of 136 economies across the world. Travel and Tourism Competitiveness Index measures the set of factors and policies that enable the 
# sustainable development of the travel and tourism sector, which in turn, contributes to the development and competitiveness of a country.\r\n\r\nThe Travel 
# and Tourism Competitiveness Index enables all stakeholders to work together to improve the industry's competitiveness in their national economies. The theme 
# of this edition Paving the Way for a More Sustainable and Inclusive Future, reflects the increasing focus on ensuring the industry's sustained growth in an 
# uncertain security environment while preserving the natural environment and local communities on which it so richly depends."},

get_wb_tourism <- function(){
  print(paste("Processing worldbank data", "TRAVEL_TOURISM ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/78/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`,`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_TOURISM", "recent_travel_tourism_competitiveness.csv")
  checkfilewrite(dict, "WB_TOURISM", "data_dictionary.csv")
  checkfilewrite(data, "WB_TOURISM", "travel_tourism_competitiveness.csv")
}


# "id":79,
# "title":"World Travel & Tourism Council"
# "description":"The World Travel & Tourism Council (WTTC) was formed in 1991 by a group of Travel & Tourism CEOs who felt that the sector's contribution to economies and job creation was not being recognised. 
# Its objectives were to use empirical evidence to promote awareness of Travel & Tourism's economic contribution; to expand markets in harmony with the environment; and to reduce barriers to growth. WTTC is the 
# only global body that brings together all major players in the Travel & Tourism sector (airlines, hotels, cruise, car rental, travel agencies, tour operators, GDS, and technology), enabling them to speak with 
# One Voice to governments and international bodies. It is important that WTTC has the broadest geographical representation and includes all aspects of the sector, including organisations that provide vital 
# services to Travel & Tourism. With Chief Executives of over 150 of the world's leading Travel & Tourism companies as its members, WTTC has a unique mandate and overview on all matters related to Travel & Tourism. 
# WTTC works to raise awareness of Travel & Tourism as one of the world's largest sectors, supporting 292 million jobs and generating 10.2% of global GDP.\r\n\r\nRead more at: https://www.wttc.org/about/"},

get_wb_wttc <- function(){
  print(paste("Processing worldbank data", "WTTC ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/79/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2015`:`2028`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  
  checkfilewrite(recent_data, "WB_WTTC", "recent_world_travel_tourism_counsil.csv")
  checkfilewrite(dict, "WB_WTTC", "data_dictionary.csv")
  checkfilewrite(data, "WB_WTTC", "world_travel_tourism_counsil.csv")
}


# "id":3755,
# "title":"Poverty and Equity Data "
# "description":"The Poverty and Equity Data Portal is the World Bank Group's comprehensive source for the latest data on poverty, inequality, and shared prosperity. 
# The portal allows you to explore several poverty and inequality indicators for countries and regions as well as explore countries by various income levels - 
# low income, lower middle income, and upper middle income, and access poverty and inequality data for fragile, IDA and other country groupings."},

get_wb_pov_equity <- function(){
  print(paste("Processing worldbank data", "POV_EQUITY ...", sep=" "))
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/3755/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <-
    tolower(paste(
      substr(paste(df$`subindicator type`, gsub(" ", "_", df$indicator), sep = "_"), 1, 25),
      df$`indicator id`,
      sep = "."
    ))
  # Create dictionary dict with `variable`
  dict <- df[, c("variable", "subindicator type", "indicator")]
  # Remove duplicated values from dict
  dict <- dict[!duplicated(dict$variable), ]
  # Create `description`
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  # Dictionary with variables `variable` and `dictionary`
  dict <- dict[, c(1, 4)]
  
  # Select columns
  data <- select(df, 2, 1, variable, `2014`:`2017`)
  # Rename colums 1 and 2
  names(data)[1:2] <- c("country" , "countryiso3")
  
  # Create pivot
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Malte Lueken's most recent function -------------------------------------
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  # End
  
  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  checkfilewrite(recent_data, "WB_POV_EQUITY", "recent_poverty_equity_data.csv")
  checkfilewrite(dict, "WB_POV_EQUITY", "data_dictionary.csv")
  checkfilewrite(data, "WB_POV_EQUITY", "poverty_equity_data.csv")
}


# Rural population (% of total population)
get_wb_rural <- function(){
  process_wb("SP.RUR.TOTL.ZS", "rural_population_percent")
}


# Travel services
get_wb_travel_services <- function(){
  process_wb("TX.VAL.TRVL.ZS.WT", "travel_services")
}


# Urban population (% of total population)
get_wb_urban_population <- function(){
  process_wb("SP.URB.TOTL.IN.ZS", "urban_population")
}


# Population living in slums (% of urban population)
get_wb_population_slums <- function(){
  process_wb("EN.POP.SLUM.UR.ZS", "population_slums")
}


# Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population) 
get_wb_poverty_headcount <- function(){
  process_wb("SI.POV.DDAY", "poverty_headcount")
}


# Net migration
get_wb_net_migration <- function(){
  process_wb("SM.POP.NETM", "net_migration")
}


# Mortality rate, under-5 (per 1,000 live births)
get_wb_mortality_rate <- function(){
  process_wb("SH.DYN.MORT", "mortality_rate")
}


# Incidence of tuberculosis (per 100,000 people) 
get_wb_incidence_tuberculosis <- function(){
  process_wb("SH.TBS.INCD", "incidence_tuberculosis")
}


# Prevalence of HIV, total (% of population ages 15-49) 
get_wb_HIV_percent <- function(){
  process_wb("SH.DYN.AIDS.ZS", "HIV_percent")
}


# GDP per capita (current US$)
get_wb_GDP_capita <- function(){
  process_wb("NY.GDP.PCAP.CD", "GDP_capita")
}


# Imports of goods and services (% of GDP)
get_wb_imports_goods <- function(){
  process_wb("NE.IMP.GNFS.ZS", "imports_goods")
}


# Inflation, consumer prices (annual %)
get_wb_consumer_prices <- function(){
  process_wb("FP.CPI.TOTL.ZG", "consumer_prices")
}


# Government expenditure on education, total (% of GDP)
get_wb_education_total <- function(){
  process_wb("SE.XPD.TOTL.GD.ZS", "education_total")
}


# Government expenditure per student, primary (% of GDP per capita)
get_wb_education_student_primary <- function(){
  process_wb("SE.XPD.PRIM.PC.ZS", "education_student_primary")
}


# Government expenditure per student, secondary (% of GDP per capita)
get_wb_education_student_secondary <- function(){
  process_wb("SE.XPD.SECO.PC.ZS", "education_student_secondary")
}


# Government expenditure per student, tertiary (% of GDP per capita)
get_wb_education_student_tertiary <- function(){
  process_wb("SE.XPD.TERT.PC.ZS", "education_student_tertiary")
}


# Literacy rate, adult total (% of people ages 15 and above)
get_wb_literacy_total <- function(){
  process_wb("SE.ADT.LITR.ZS", "literacy_total")
}


# Population ages 15-64 (% of total population)
get_wb_population_15_64 <- function(){
  process_wb("SP.POP.1564.TO.ZS", "population_15_64")
}


# Population ages 65 and above (% of total population)
get_wb_population_65_up <- function(){
  process_wb("SP.POP.65UP.TO.ZS", "population_65_up")
}


# Life expectancy at birth, total (years)
get_wb_life_expectancy <- function(){
  process_wb("SP.DYN.LE00.IN", "life_expectancy")
}


# Unemployment, total (% of total labor force) (modeled ILO estimate)
get_wb_employment_total <- function(){
  process_wb("SL.UEM.TOTL.ZS", "employment_total")
}


# Adolescent fertility rate (births per 1,000 women ages 15-19)
get_wb_adolescent_fertilty <- function(){
  process_wb("SP.ADO.TFRT", "adolescent_fertilty")
}


# Hospital beds (per 1,000 people)
get_wb_hospital_beds <- function(){
  process_wb("SH.MED.BEDS.ZS", "get_hospital_beds")
}


# Air transport, registered carrier departures worldwide
get_wb_air_departures <- function(){
  process_wb("IS.AIR.DPRT", "air_departures")
}


# Mobile cellular subscriptions (per 100 people)
get_wb_cellular_subscriptions <- function(){
  process_wb("IT.CEL.SETS.P2", "cellular_subscriptions")
}


# Population density (people per sq. km of land area)
get_wb_population_density <- function(){
  process_wb("EN.POP.DNST", "population_density")
}


# International tourism, expenditures (% of total imports)
get_wb_intl_tourism_expenditures <- function(){
  process_wb("ST.INT.XPND.MP.ZS", "intl_tourism_expenditures")
}


# Vulnerable employment, female (% of female employment)
get_wb_employment_female <- function(){
  process_wb("SL.EMP.VULN.FE.ZS", "employment_female")
}


# Vulnerable employment, male (% of male employment) (modeled ILO estimate)
get_wb_employment_male <- function(){
  process_wb("SL.EMP.VULN.MA.ZS", "employment_male")
}


process_wb <- function(wbId, fileName){
  print(paste("Processing worldbank data", toupper(fileName), "...", sep=" "))
  # Read worldbank data
  df <- wb(indicator = c(wbId))
  # Convert format
  df <- df %>%
    select(`country`, `iso3c`, `indicatorID`, `indicator`, `date`, `value`) %>%
    rename(`countryiso3` = `iso3c`, `description` = `indicator`, `variable` = `indicatorID`)
  
  # Dictionary
  dict <- df[, c("variable", "description")]
  dict <- dict[!duplicated(dict$variable), ]
  df <- df %>% select(-c("description"))
  
  # Add year columns
  dfNew <- df[,1:3]
  
  # 2013
  dfYear <- df %>% subset(`date` == 2013)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2013" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2014
  dfYear <- df %>% subset(`date` == 2014)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2014" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2015
  dfYear <- df %>% subset(`date` == 2015)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2015" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2016
  dfYear <- df %>% subset(`date` == 2016)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2016" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2017
  dfYear <- df %>% subset(`date` == 2017)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2017" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2018
  dfYear <- df %>% subset(`date` == 2018)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2018" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2019
  dfYear <- df %>% subset(`date` == 2019)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2019" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # 2020
  dfYear <- df %>% subset(`date` == 2020)
  dfNew <- merge(dfNew, dfYear, by = 1:3, all = TRUE)
  dfNew <- dfNew %>%  
    select(-c("date")) %>% 
    rename("2020" = "value") %>%
    distinct(`countryiso3`, `variable`, .keep_all = TRUE)
  
  # Create pivot
  data <- dfNew
  data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
  data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
  
  # Select only columns with enough data
  data <- data %>% select_if(no_data)
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("code", "year"), "_")
  recent_data <- recent_data %>%  
    mutate(country = fct_inorder(country), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, code)
  recent_data <- recent_data %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == suppressWarnings(max(year[!is.na(value)], na.rm = T))])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # Reorder columns for better readibility
  cols <- NCOL(recent_data)
  if (cols %% 2 == 0 && cols > 2){
    s <- c(1, 2, rbind(seq(3, cols/2+1), seq(cols/2+2, cols)))
    recent_data <- recent_data %>% select(s)
  }
  # Select only columns with enough data
  recent_data <- recent_data %>% select_if(enough_data)
  
  # Write results
  directory <- paste("WB", gsub("[.]", "", wbId), sep="_")
  checkfilewrite(recent_data, directory, paste(paste("recent", fileName, sep="_"), "csv", sep="."))
  checkfilewrite(dict, directory, "data_dictionary.csv")
  checkfilewrite(data, directory, paste(fileName, "csv", sep="."))
}

get_worldbank_open_data()

