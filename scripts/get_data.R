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

unzip_from_web <- function(this_url){
  timeout_option <- getOption('timeout')
  options(timeout=500)
  f <- file.path(tempdir(), 'tmp.zip')
  download.file(this_url, destfile = f, mode = "wb", method = "libcurl")
  results <- utils::unzip(zipfile = f, exdir = tempdir())
  file.remove(f)
  options(timeout=timeout_option)
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
  df <- read.csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", stringsAsFactors = FALSE)
  #df <- rio::import("https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx")
  names(df) <- tolower(names(df))
  names(df)[1:2] <- c("country", "countryiso3")
  miss <- colSums(is.na(df))
  df[miss == nrow(df)] <- NULL
  if(any(grepl("_notes", names(df)))){
    df <- df[, -grep("_notes", names(df))]
  }
  
  # Create dict
  #dict <- data.frame(variable = grep("^[ceh]\\d", names(df), value = TRUE))
  codes <- unique(gsub("_.*$", "", grep("^[ceh]\\d", names(df), value = TRUE)))
  for(x in codes){
    new_name <- substring(grep(paste0(x, "(?!_flag)"), names(df), value = TRUE, perl = TRUE), first = 4)
    names(df)[which(names(df) == paste0(x, "_", new_name))] <- new_name
    names(df)[which(names(df) == paste0(x, "_flag"))] <- paste0(new_name, "_flag")
  }
  #dict$description <- gsub("^.+_(?!flag)", "", dict$variable, perl = TRUE)
  #dict$variable <- gsub("_(?!flag).*", "", dict$variable, perl = TRUE)

  #names(df) <- gsub("_(?!flag).*", "", names(df), perl = TRUE)

  ox <- pivot_longer(df, cols = 4:ncol(df))
  ox$date <- as.Date(as.character(ox$date), format = "%Y%m%d")
  # Time since first occurrence ---------------------------------------------
  #head(ox)
  #tmp <- ox[ox$countryiso3 == "AFG" & ox$name == "confirmedcases",]
  today <- today()
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
  #checkfilewrite(dict, "OxCGRT","data_dictionary.csv")
  write("https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md", file.path("data", "OxCGRT", "codebook_link.txt"))
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

initialize_database <- function(){
  get_csse()
  get_OxCGRT()
  get_google_mobility()
}

source("scripts/get_google_mobility.R")
source("scripts/get_wb_open_data.R")
source("scripts/get_world_factbook_data.R")
