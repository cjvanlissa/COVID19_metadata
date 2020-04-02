library(gert)
library(rio)
library(readxl)
library(countrycode)
library(tidyr)

get_csse <- function(){
  olddir <- getwd()
  git_dir <- file.path(tempdir(), 'CSSE')
  gert::git_clone("https://github.com/CSSEGISandData/COVID-19.git", git_dir)
  setwd(file.path(git_dir, "csse_covid_19_data", "csse_covid_19_daily_reports"))
  olddir <- "C:/Git_Repositories/psycorona/PsyCorona_phase3"
  f <- list.files(pattern = "csv")
  file.copy(from = f, file.path(olddir, "data", "CSSE", f))
  setwd(olddir)
  unlink(git_dir, recursive = TRUE)
}

get_OxCGRT <- function(){
  df <- rio::import("https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx")
  write.csv(df, file.path("data", "OxCGRT_Oxford_regulation_policies.csv"), row.names = FALSE)
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
  
  if(!dir.exists(file.path("data", "WB_WDI"))) dir.create(file.path("data", "WB_WDI"))
  
  write.csv(data_dict(WDIData[!duplicated(WDIData$Indicator.Code), ], "Indicator.Code", "Indicator.Name"), file.path("data", "WB_WDI", "data_dictionary.csv"), row.names = FALSE)
  write.csv(wdi, file.path("data", "WB_WDI", "wdi.csv"), row.names = FALSE)
  file.remove(funzipped)
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