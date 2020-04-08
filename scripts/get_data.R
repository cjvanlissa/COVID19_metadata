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
source("scripts/get_google_mobility.R")

get_csse <- function(){
  # Go to temporary directory for git repository
  olddir <- getwd()
  git_dir <- file.path(tempdir(), 'CSSE')
  gert::git_clone("https://github.com/CSSEGISandData/COVID-19.git", git_dir)
  setwd(file.path(git_dir, "csse_covid_19_data", "csse_covid_19_daily_reports"))
  f <- list.files(pattern = "csv")
  # File parsing
  parsed_files <- lapply(f, function(this_file){
    regional <- read_csv(this_file)
    names(regional) <- tolower(names(regional))
    names(regional)[c(grep("province", names(regional)), grep("country", names(regional)))] <- c("region", "country")
    regional <- regional[, c("country", "region", "confirmed", "deaths", "recovered")]
    regional[is.na(regional$region), ]
    regional %>%
      group_by(country) %>%
      summarise(region = NA,
                confirmed = mean(confirmed, na.rm = TRUE),
                deaths = mean(deaths, na.rm = TRUE),
                recovered = mean(recovered, na.rm = TRUE)) -> national
    if(!ncol(national)==ncol(regional)) browser()
    out <- rbind(national, regional)
    names(out) <- gsub("(confirmed|deaths|recovered)", paste0("\\1\\.", substring(gsub("-", "_", this_file), first = 1, last = 10)), names(out))
    out$id <- paste(out$country, out$region, sep = "_")
    out[!duplicated(out$id), ]
    })
  unique_ids <- unique(unlist(lapply(parsed_files, `[[`, "id")))
  dt <- data.table(id = unique_ids)
  dt[, country := gsub("_.*$", "", id)]
  dt[, region := gsub("^.+?_", "", id)]
  dt[region == "NA", region := NA]

  # Merge without running into memory problems
  for(this_file in parsed_files){
    set(x = dt,
        i = match(this_file$id, dt$id),
        j = names(this_file)[3:5],
        value = this_file[, 3:5])
  }
  dt[, id := NULL]

  dt[, countryiso3 := countrycode(country, origin = "country.name", destination = "iso3c")]
  setcolorder(dt, c(1, ncol(dt), 2:(ncol(dt)-1)))

  # Go back to original directory
  setwd(olddir)
  checkfilewrite(dt, "CSSE", "CSSE.csv")
  # Remove git repository
  unlink(git_dir, recursive = TRUE)
}

get_OxCGRT <- function(){
  df <- rio::import("https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx")
  names(df) <- tolower(names(df))
  names(df)[1:2] <- c("country", "countryiso3")
  df[["...35"]] <- NULL
  df <- df[, -grep("_notes", names(df))]
  # Create dict
  dict <- data.frame(variable = grep("^s\\d", names(df), value = TRUE))
  dict$description <- gsub("^.+_(?!isgen)", "", dict$variable, perl = TRUE)
  dict$variable <- gsub("_(?!isgen).*", "", dict$variable, perl = TRUE)

  names(df) <- gsub("_(?!isgen).*", "", names(df), perl = TRUE)

  ox <- pivot_longer(df, cols = 4:ncol(df))
  ox$date <- as.Date(as.character(ox$date), format = "%Y%m%d")
  # Time since first occurrence ---------------------------------------------
  #head(ox)
  #tmp <- ox[ox$countryiso3 == "AFG" & ox$name == "confirmedcases",]
  time_since <- ox %>%
    mutate(value = !(value == 0 | is.na(value))) %>%
    #group_by(country, date) %>%
    #mutate(acted = any(value)) %>%
    group_by(country, countryiso3, name) %>%
    summarise(time_since_first = today - dplyr::first(date[value])) %>%
    mutate(time_since_first = as.integer(time_since_first)) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = name, values_from = time_since_first)
  # End

  #
  ox <- pivot_wider(ox, id_cols = c("country", "countryiso3"), names_from = c("name", "date"))

  checkfilewrite(time_since, "OxCGRT", "time_since_first_OxCGRT.csv")
  checkfilewrite(ox, "OxCGRT","OxCGRT_Oxford_regulation_policies.csv")
  checkfilewrite(dict, "OxCGRT","data_dictionary.csv")
}


get_GHS <- function(){

  funzipped <- unzip_from_web("https://www.ghsindex.org/wp-content/uploads/2019/10/Global-Health-Security-Index-2019-Final-October-2019.zip")

  # Most prepared
  # Score 66.7 to => 100
  most_prepared <- readxl::read_excel(path = funzipped, sheet = 4, range = "E47:F59",
                                    col_names = FALSE, progress = TRUE)

  # More prepared
  # Score 33.4 to => 66.6
  more_prepared <- readxl::read_excel(path = funzipped, sheet = 4, range = "H47:I155",
                                    col_names = FALSE, progress = TRUE)

  # Least prepared
  # Score 0 to => 33.3
  least_prepared <- readxl::read_excel(path = funzipped, sheet = 4, range = "K47:L119",
                                     col_names = FALSE, progress = TRUE)

  #adding names of tibbles as variable because I'm going to merge them later
  name.as.variable.df <-  function (df)
  {
    name=deparse(substitute(df)) #outputs the name of whatever df is
    df[,ncol(df)+1] <- name
    return(df)
  }

  most_prepared <- name.as.variable.df(most_prepared)
  more_prepared <- name.as.variable.df(more_prepared)
  least_prepared <- name.as.variable.df(least_prepared)

  #merging into one big tibble
  preparedness_score <- rbind(most_prepared, more_prepared, least_prepared)

  ### Tidying up the tibble ###
  names(preparedness_score) <- c("country", "score", "category")
  preparedness_score$countryiso3 <- countrycode(preparedness_score$country, origin = "country.name", destination = "iso3c")
  preparedness_score <- preparedness_score[, c(1, 4, 2:3)]
  ###INDICATORS###
  #the indicators were organized hierarchically as such:
  #Categories
  #Indicators
  #Sub-indicators
  #Questions
  #So the question 6.4.1a) is from category 6, indicator 4, subindicator 1 question a)
  #Seeing as this info is contained within the question code, here I loaded only the questions

  per_country_questions <- readxl::read_excel(path = funzipped, sheet = 13, range = "N10:HC276",
                                            col_names = TRUE, progress = TRUE, na = "-")

  #NOTE: varible containing question string is confusingly named Indicators.
  #per_country_questions$Indicators

  #dropping empty rows (those corresponding to higher hieracrchical levels)
  missing <- rowSums(is.na(per_country_questions))
  per_country_questions <- per_country_questions[!missing == max(missing), ]

  #preparedness score is a weighted sum of questions.
  #here I obtain the weights for each question

  question_weights <- readxl::read_excel(path = funzipped, sheet = 14, range = "C183:H408",
                                       col_names = TRUE, progress = TRUE)

  question_weights[c(2:3,5)] <- NULL
  question_weights <- question_weights[!is.na(question_weights$`Weight %`),]
  names(question_weights)[1] <- "indicator"
  question_weights$indicator <- gsub("_", "\\.", gsub("^.+?(?=\\d)", "Q", question_weights$indicator, perl = TRUE))

  ## equal number of rows means I didn't mess something up
  if(!nrow(question_weights)==nrow(per_country_questions)) stop("Number of rows in question_weights is not the same as in per_country_questions. Cannot merge.")

  #question_weights column QUESTIONS is the same as per_country_questions column indicators
  if(!all(question_weights$QUESTIONS==per_country_questions$Indicators)) stop("question_weights column QUESTIONS is not the same as per_country_questions column indicators. Cannot merge.")

  #Realised my mistake. Questions are weighted as percents of subindicator.
  #for the absolute weighting I have to obtain weighting for all higher levels.

  #Have to redo the question weights. Same code as above
  question_weights <- readxl::read_excel(path = funzipped, sheet = 14, range = "C183:H408",
                                       col_names = TRUE, progress = TRUE)

  question_weights <- question_weights[!is.na(question_weights$`Weight %`), ]


  #Subindicator weights here. Same deal as with questions.
  subind_weights <- readxl::read_excel(path = funzipped, sheet = 14, range = "C62:H181",
                                     col_names = TRUE, progress = TRUE)
  subind_weights <- subind_weights[!is.na(subind_weights$`Weight %`),]

  #Indicator weights here. Same deal as with questions.
  ind_weights <- readxl::read_excel(path = funzipped, sheet = 14, range = "C20:H60",
                                  col_names = TRUE, progress = TRUE)

  ind_weights <- ind_weights[!is.na(ind_weights$`Weight %`),]
  #Category weights here. Same deal as with questions.
  cat_weights <- readxl::read_excel(path = funzipped, sheet = 14, range = "C12:H18",
                                  col_names = TRUE, progress = TRUE)
  cat_weights <- cat_weights[!is.na(cat_weights$`Weight %`),]
  #to get absolute weights for each question I have to multiply the weight
  #by all the weights above it

  #takes weights from a lower level and multiplies them by weights from the higher level
  abs_weights <- function(lvl, lvl_up)
  {
    nms=(lvl_up[,1])
    for (i in 1:nrow(nms))
    {
      lvl_up_w=as.numeric(lvl_up[i,6])
      this_nm=as.character(nms[i,])
      is_lvl=grep(pattern = this_nm, x = lvl$...1)
      lvl[is_lvl,6]=lvl[is_lvl,6]*lvl_up_w
    }
    return(lvl)
  }

  #sanity check. sums to 1, so it's ok!
  if(!sum(abs_weights(lvl = ind_weights, lvl_up = cat_weights)[,6]) == 1) stop("Weights do not sum to 1.")

  ind_weights <- abs_weights(lvl = ind_weights, lvl_up = cat_weights)
  subind_weights <- abs_weights(subind_weights, ind_weights)
  question_weights <- abs_weights(question_weights,subind_weights)

  #It worked! The absolute weighting in the question weights now
  #represents the percent contribution it has to the sum score.
  #These weights can be linearly transformed (intercept and slope) as needed
  #sum(question_weights$`Weight %`)

  #Tidying up the question weights tibble
  question_weights[,c(2:3,5)] <- NULL
  names(question_weights)[1] <- "question_code"

  indicator_dictionary <- data.frame(variable = per_country_questions$Indicators,
                                     per_country_questions[, 2:3])
  indicator_dictionary$description <- gsub("^.+?\\) ", "", indicator_dictionary$variable)
  indicator_dictionary$variable <- paste0("Q", gsub("^(.+?)\\).*$", "\\1", indicator_dictionary$variable))
  if(!all(indicator_dictionary$variable == question_weights$indicator)) stop("Could not merge dictionary and weights.")
  indicator_dictionary$weight_percent <- question_weights$`Weight %`
  write.csv(indicator_dictionary, file.path("data", "GHS", "indicator_dictionary.csv"), row.names = FALSE)
  per_country_questions[1:3] <- NULL
  countries <- names(per_country_questions)

  tmp <- t(per_country_questions)
  dims <- dim(tmp)
  tmp <- gsub(",", "", tmp)
  tmp <- as.numeric(tmp)
  tmp <- matrix(tmp, nrow = dims[1])

  #names(table(tmp))[which(is.na(as.numeric(names(table(tmp)))))]
  #any(is.na(as.numeric(names(table(tmp)))))
  indicators <- data.frame(country = countries,
                                      tmp, stringsAsFactors = FALSE)
  names(indicators)[-1] <- indicator_dictionary$variable

  head(indicators$country)
  indicators$countryiso3 <- countrycode(indicators$country, origin = "country.name", destination = "iso3c")
  indicators <- indicators[, c(1,ncol(indicators), 2:(ncol(indicators)-1))]
  write.csv(indicators, file.path("data", "GHS", "preparedness_indicators.csv"), row.names = FALSE)
  write.csv(preparedness_score, file.path("data", "GHS", "preparedness.csv"), row.names = FALSE)
  file.remove(funzipped)
}



get_wb_wdi <- function(){
  # Download and unzip

  funzipped <- unzip_from_web(this_url = "http://databank.worldbank.org/data/download/WDI_csv.zip")

  # Read files into objects with same name
  for(this_file in funzipped){
    eval(parse(text = paste0(gsub("-", "_", gsub("^.+\\/(.+)\\.csv$", "\\1", this_file)), " <- read.csv('", this_file, "', stringsAsFactors = FALSE)")))
  }
  names(WDIData)[1] <- "country"
  wdi <- WDIData[, c("country", "Indicator.Code", grep("^X201[6789]", names(WDIData), value = TRUE))]
  wdi <- pivot_longer(wdi, cols = grep("^X201[6789]", names(wdi), value = TRUE))
  wdi <- pivot_wider(wdi, id_cols = "country", names_from = c("Indicator.Code", "name"))
  names(wdi) <- gsub("_X", "_", names(wdi))
  wdi$countryiso3 <- countrycode(wdi$country, origin = "country.name", destination = "iso3c")
  wdi <- wdi[, c(1, ncol(wdi), 2:(ncol(wdi)-1))]


# Malte Lueken's most recent function -------------------------------------

  recent_data <- wdi %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    mutate(year = str_sub(var, -4),
           code = str_sub(var, 1, -6)) %>%
    mutate(country = fct_inorder(country), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, code)  %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = code, values_from = c("latest.year", "latest.value"))

# End

  checkfilewrite(recent_data, "WB_WDI", "recent_wdi.csv")
  checkfilewrite(data_dict(WDIData[!duplicated(WDIData$Indicator.Code), ], "Indicator.Code", "Indicator.Name"), "WB_WDI", "data_dictionary.csv")
  checkfilewrite(wdi, "WB_WDI", "wdi.csv")
  file.remove(funzipped)
}

get_wb_gov <- function(){
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/51/dump.csv")
  names(df) <- tolower(names(df))
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
  dict <- df[, c("variable", "subindicator type", "indicator")]
  dict <- dict[!duplicated(dict$variable), ]
  dict$description <- paste(dict$`subindicator type`, dict$indicator)
  dict <- dict[, c(1, 4)]

  gov <- select(df, 2, 1, variable, `2013`:`2018`)
  names(gov)[1:2] <- c("country" , "countryiso3")

  gov <- pivot_longer(gov, cols = grep("^20", names(gov), value = TRUE))
  gov <- pivot_wider(gov, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))


# Malte Lueken's most recent function -------------------------------------

  recent_data <- gov %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

# End

  checkfilewrite(recent_data, "WB_GOV", "recent_wb_government_effectiveness.csv")
  checkfilewrite(dict, "WB_GOV", "data_dictionary.csv")
  checkfilewrite(gov, "WB_GOV", "wb_government_effectiveness.csv")

}


# "id":56,
# "title":"World Development Indicators",
# "description":"The primary World Bank collection of development indicators,
# compiled from officially-recognized international sources. It presents the most current and accurate global development
# data available, and includes national, regional and global estimates."

# get_wb_dev <- function(){
#   # Download and unzip
#   df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/56/dump.csv")
#   names(df) <- tolower(names(df))
#   # Concatenate columns 'indicator' and 'subindicator type' as `variable`
#   df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
#   # Create dictionary dict with `variable`
#   dict <- df[, c("variable", "subindicator type", "indicator")]
#   # Remove duplicated values from dict
#   dict <- dict[!duplicated(dict$variable), ]
#   # Create `description`
#   dict$description <- paste(dict$`subindicator type`, dict$indicator)
#   # Dictionary with variables `variable` and `dictionary`
#   dict <- dict[, c(1, 4)]
# 
#   # Select columns
#   data <- select(df, 2, 1, variable, `2013`:`2019`)
#   # Rename colums 1 and 2
#   names(data)[1:2] <- c("country" , "countryiso3")
# 
#   # Create pivot
#   data <- pivot_longer(data, cols = grep("^20", names(data), value = TRUE))
#   data <- pivot_wider(data, id_cols = c("country", "countryiso3"), names_from = c("variable", "name"))
# 
#   # Malte Lueken's most recent function -------------------------------------
# 
#   # Create recent data from latest year with data
#   recent_data <- data %>%
#     pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
#     separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
#     mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
#     group_by(country, countryiso3, type, code) %>%
#     summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
#               latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
#     ungroup() %>%
#     mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
#     pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
# 
#   # End
# 
#   checkfilewrite(recent_data, "WB_DEV", "recent_world_development_indicators.csv")
#   checkfilewrite(dict, "WB_DEV", "data_dictionary.csv")
#   checkfilewrite(data, "WB_DEV", "world_development_indicators.csv")
# }


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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/97/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/513/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/435/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/50/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/997/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/429/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/999/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/4127/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/513/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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

  # Malte Lueken's most recent function -------------------------------------

  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))

  # End

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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/1000/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/748/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
  checkfilewrite(recent_data, "WB_EDUCATION", "recent_education_statistics.csv")
  checkfilewrite(dict, "WB_EDUCATION", "data_dictionary.csv")
  checkfilewrite(data, "WB_EDUCATION", "education_statistics.csv")
}

# "id":747,
# "title":"Gender Statistics"
# "description":"The Gender Statistics database is a comprehensive source for the latest sex-disaggregated data and gender statistics covering demography, 
# education, health, access to economic opportunities, public life and decision-making, and agency."},,

get_wb_gender <- function(){
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/747/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/78/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/79/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
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
  # Download and unzip
  df <- rio::import("https://govdata360-backend.worldbank.org/api/v1/datasets/3755/dump.csv")
  names(df) <- tolower(names(df))
  # Concatenate columns 'indicator' and 'subindicator type' as `variable`
  df$variable <- paste(df$`subindicator type`, df$`indicator id`, sep = ".")
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
  
  # Malte Lueken's most recent function -------------------------------------
  
  # Create recent data from latest year with data
  recent_data <- data %>%
    pivot_longer(names_to = "var", values_to = "value", -c("country", "countryiso3")) %>%
    separate(var, c("type", "var"), "[.]") %>% separate(var, c("code", "year"), "_") %>%
    mutate(country = fct_inorder(country), type = fct_inorder(as.factor(type)), code = fct_inorder(as.factor(code)), year = as.numeric(year)) %>%
    group_by(country, countryiso3, type, code) %>%
    summarise(latest.value = ifelse(all(is.na(value)), NA, value[year == max(year[!is.na(value)], na.rm = T)]),
              latest.year = ifelse(all(is.na(value)), NA, year[year == max(year[!is.na(value)], na.rm = T)])) %>%
    ungroup() %>%
    mutate(var = paste(as.character(type), ".", as.character(code), sep = "")) %>%
    pivot_wider(id_cols = c("country", "countryiso3"), names_from = var, values_from = c("latest.year", "latest.value"))
  
  # End
  
  checkfilewrite(recent_data, "WB_POV_EQUITY", "recent_poverty_equity_data.csv")
  checkfilewrite(dict, "WB_POV_EQUITY", "data_dictionary.csv")
  checkfilewrite(data, "WB_POV_EQUITY", "poverty_equity_data.csv")
}


unzip_from_web <- function(this_url){
  timeout_option <- getOption('timeout')
  options(timeout=500)
  f <- file.path(tempdir(), 'tmp.zip')
  download.file(this_url, destfile = f, mode = "wb", method = "libcurl")
  results <- utils::unzip(zipfile = f, exdir = tempdir())
  file.remove(f)
  options(timeout=ttimeout_option)
  return(results)
}

data_dict <- function(df, item, description, others = NULL){
  dict <- df[, c(item, description, others)]
  names(dict)[1:2] <- c("variable", "description")
  dict
}

checkfilewrite <- function(df, the_dir, the_file){
  if(!dir.exists(file.path("data", the_dir))) dir.create(file.path("data", the_dir))
  if(!file.exists(file.path("data", the_dir, the_file))){
    write.csv(df, file.path("data", the_dir, the_file), row.names = FALSE)
  } else {
    old_sum <- tools::md5sum(file.path("data", the_dir, the_file))
    write.csv(df, file.path("data", the_dir, "XXXTMPXXX.csv"), row.names = FALSE)
    new_sum <- tools::md5sum(file.path("data", the_dir, "XXXTMPXXX.csv"))
    if(old_sum == new_sum){
      file.remove(file.path("data", the_dir, "XXXTMPXXX.csv"))
    } else {
      file.remove(file.path("data", the_dir, the_file))
      file.rename(file.path("data", the_dir, "XXXTMPXXX.csv"), file.path("data", the_dir, the_file))
    }
  }
}

initialize_database <- function(){
  get_csse()
  get_OxCGRT()
  get_google_mobility()
}
