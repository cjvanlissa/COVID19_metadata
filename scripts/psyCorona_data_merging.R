########## TO DO ##########

# TODO: 
# google_mobility/mobility_March_29_2020.csv: Replace the current end Mar data with response date specific values
# OWID_Tests/OurWorldInData_Tests.csv: Replace the current end Mar data with response date specific values
# WB_DEV/recent_world_development_indicators.csv: Select relevant variables
# WB_GOV/wb_government_effectiveness.csv: Select relevant variables
# WB_INSTITUTIONAL/recent_institutional_profiles.csv:

########## Packages ##########
library(dplyr)
library(countrycode)
library(tidyr)
library(data.table)
########## FUNCTIONS ##########

merge_cj <- function(df, df_dat, date_regexp = "\\d{2}_\\d{2}_\\d{4}$"){
  browser()
  if(!is.data.table(df)){ 
    df <- data.table(df)
  }
  if(!is.data.table(df_dat)){ 
    df_dat <- data.table(df_dat)
  }
  rem_cols <- c("country", "region")
  rem_cols <- rem_cols[rem_cols %in% names(df_dat)]
  if(length(rem_cols) > 0){
    df_dat[, (rem_cols) := NULL]
  }
  
  date_vars <- grepl(date_regexp, names(df_dat))
  the_dates <- names(df_dat)[date_vars]
  
  if(any(!(date_vars | names(df_dat) == "countryiso3"))){
    df_nondated <- df_dat[, .SD, .SDcols =unique(c("countryiso3", names(df_dat)[!date_vars]))]
    df <- merge(df, df_nondated, by = "countryiso3", all.x = TRUE)
  }
  if(any(date_vars)){
    df_dated <- df_dat[, .SD, .SDcols = c("countryiso3", the_dates)]
    df_dated <- pivot_longer(df_dated, cols = names(df_dated)[-1], names_sep = "\\.", names_to = c("variable", "date"))
    df_dated <- data.table(df_dated)
    df_dated <- na.omit(df_dated)
    nreps <- length(unique(df_dated$variable))
    
    df_skeleton <- df[rep(1:nrow(df), nreps), c("countryiso3", "date","rownum")]
    df_skeleton[, "variable" := rep(unique(df_dated$variable), each = nrow(df))]
    df_skeleton[df_dated, "value" := i.value, on = c(countryiso3="countryiso3", date="date", variable = "variable")]
    df_skeleton[, c("countryiso3", "date") := NULL]
    df_skeleton <- na.omit(df_skeleton)
    df_skeleton <- dcast(df_skeleton, rownum ~ variable, value.var = "value")
    df <- merge(df, df_skeleton, by = "rownum", all.x = TRUE)
  }
  
  return(df)
}

# replace date part of variable with day no: 'var.date' will be renamed to '&.var.dayno
# format of date is in date_regexp and date_subst_posix for use in gsub
# format of complete variable is in date_regexp_w_groups and prefix_regexp_w_group
replace_date_variables_with_dayno <- function(df, date_regexp, date_regexp_w_groups, date_subst_posix, prefix_regexp_w_group){
  date_vars <- grepl(paste0(date_regexp, "$"), names(df))
  the_dates <- names(df)[date_vars]
  splits <- strsplit(the_dates, ".", fixed = TRUE)
  dayno <- calc_dayno(as.POSIXct(sapply(splits, `[[`, 2), format = "%m_%d_%Y"))
  
  col_type <- sapply(splits, `[[`, 1)
  new_col <- paste("&", col_type, dayno, sep =".")
  names(df)[date_vars] <- new_col
  df
}

#
replace_dated_variables_based_on_response_date <- function(df){
  # get the respective dated variables
  n <- names(df)
  dat_vars <- names(df)[startsWith(n, "&")]
  browser()
  dated_variable_names <- unique(sapply(strsplit(dat_vars, ".", fixed = TRUE), `[[`, 2))
  
  # for each row win data frame
  for (row in 1:nrow(df)) {
    # find day of responese
    dayresponse <- df[row, "dayresponse"]
    
    # for all dated variable names
    for (var in dated_variable_names) {
      # find the last 16 values starting with day of response
      val <- vector(mode = "list", length = 15)
      for (i in 1:16) {
        val[i] <- df[row, paste("&", var, dayresponse - i + 1, sep = ".")]
      }
      
      # calculate  mean values of day-15 to day-8
      var.mean.1508 <- NA
      c <- na.omit(val[16], val[15], val[14], val[13], val[12], val[11], val[10], Val[9])
      if (!is.null(unlist(c))){
        var.mean.1508 <- sapply(c, mean)
      } 
      
      # calculate  mean value of day-7 to day-0
      var.mean.0700 <- NA
      c <- na.omit(val[8], val[7], val[6], val[5], val[4], val[3], val[2], Val[1])
      if (!is.null(unlist(c))){
        var.mean.0700 <- sapply(c, mean)
      }
      
      # value at day of response - 1
      var.01 <- unlist(val[2])
      var.01 <- as.numeric(var.01)
      
      # value at day of response 
      var.00 <- unlist(val[1])
      var.00 <- as.numeric(var.00)
      
      # calculate delta values
      var.delta.0001 <- var.00 - var.01
      var.delta.mean.0715 <- var.mean.0700 - var.mean.1508
      
      # store calculated values as new variables in df
      if (is.null(var.00) | length(var.00) == 0){ var.00 <- NA }
      df[row, paste0(var,".00")] <- var.00
      if (is.null(var.01) | length(var.01) == 0){ var.01 <- NA }
      df[row, paste0(var,".01")] <- var.01
      if (is.null(var.mean.0700) | length(var.mean.0700) == 0){ var.mean.0700 <- NA }
      df[row, paste0(var,".mean.0700")] <- var.mean.0700
      if (is.null(var.mean.1508) | length(var.mean.1508) == 0){ var.mean.1508 <- NA }
      df[row, paste0(var,".mean.1508")] <- var.mean.1508
      if (is.null(var.delta.0001) | length(var.delta.0001) == 0){ var.delta.0001 <- NA }
      df[row, paste0(var,".delta.0001")] <- var.delta.0001
      if (is.null(var.delta.mean.0715) | length(var.delta.mean.0715) == 0){ var.delta.mean.0715 <- NA }
      df[row, paste0(var,".delta.mean.0715")] <- var.delta.mean.0715
    }
  }
  
  # remove all dated variables from df
  for (var in dated_variable_names) {
    df <- df[,!grepl(paste0("^&[.]", var, "[.][0-9]+"), names(df))]
  }
  
  df
}

#
merge_file_by_countryiso3 <- function(df, file_path, na_percentage){
  merge_file_by_countryiso3_variables(df, file_path, NULL, na_percentage)
}

# based on PsyCorona-DataCleaning merge_file_by_countryiso3
merge_file_by_countryiso3_variables <- function(df, file_path, vars_spec, na_percentage){
  message("\n*** Merging ", file_path, " with percentage ", na_percentage)
  
  df_dat <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # handle variable names to be merged
  if (!is.null(vars_spec)) {
    # select specified columns only
    df_dat <-
      df_dat[c("country", "countryiso3", as.vector(vars_spec$var))]
    # rename columns
    names(df_dat) <- c("country", "countryiso3", as.vector(vars_spec$name))
  }
  
  # add XKV as current Kosovo ISO3 code 
  if (length(which(df_dat$country=="Kosovo")) > 0)
  df_dat[which(df_dat$country=="Kosovo"),]$countryiso3 <- "XKV"
  df_dat <- df_dat[!is.na(df_dat$countryiso3), ]
  if(!all(df$countryiso3 %in% df_dat$countryiso3)){
    message("Not all countries in df are available in ", file_path, ":")
    message(paste(unique(df_dat$countryiso3[!(df$countryiso3 %in% df_dat$countryiso3)]), collapse = " "))
  }
  if(any(duplicated(df_dat$countryiso3))){
    message("There are some duplicated country names in ", file_path, ".")
    if(!is.null(df_dat[["region"]])){
      #message("Dropping region to see if this solves the problem.")
      df_dat <- df_dat[is.na(df_dat$region), ]
    }
    if(any(duplicated(df_dat$countryiso3))){
      message("There are still duplicated country names in ", file_path, 
              ". Using the row with most available data. Manual merging might be required. The duplicates are:\n  ", 
              paste(df_dat$countryiso3[which(duplicated(df_dat$countryiso3))], collapse = "  "))
      the_dups <- which(duplicated(df_dat$countryiso3) | duplicated(df_dat$countryiso3, fromLast = T))
      df_dups <- df_dat[the_dups, ]
      df_dat <- df_dat[-the_dups, ]
      df_dups$miss <- rowSums(is.na(df_dups))
      for(countr in unique(df_dups$countryiso3)){
        df_dups <- df_dups[-which(df_dups$countryiso3 == countr)[-which.min(df_dups$miss[df_dups$countryiso3 == countr])], ]
      }
      df_dat <- rbind(df_dat, df_dups[, -ncol(df_dups)])
      #message("For duplicates, used row with most complete information.")
    }
  }
  
  # remove 'country' variable from df_dat
  df_dat <- df_dat[,!(names(df_dat) %in% c("country"))]
  browser()
  # From here on, df_dat will be a data.table
  
  if(!is.data.table(df_dat)){
    df_dat <- as.data.table(df_dat)
  }
  
  # scale by 1,000,000 population
  if (!is.null(vars_spec)){
    cols <- vars_spec$name[vars_spec$scale]
    if(length(cols) > 0){
      df_dat[ , (cols) := lapply(.SD, `/`, df$population.2119), .SDcols = cols]
    }
  }

  # drop df_dat columns with more than na_percentage of NAs
  keep_columns <- colSums(is.na(df_dat))/nrow(df_dat) <= na_percentage
  if (sum(!keep_columns) > 0) {
    message(
      "The following columns are dropped because of too many NAs (percentage is ",
      na_percentage,
      "): ",
      paste(names(df_dat)[!keep_columns],  collapse = "  ")
    )
    cols <- names(df_dat)[!keep_columns]
    df_dat[, (cols) := NULL]
  }
  

  # merge
  if(!is.data.table(df)){
    df <- as.data.table(df)
  }

  #df <- merge(df, df_dat, all.x = T, by = "countryiso3")

  setkey(df, countryiso3)
  setkey(df_dat, countryiso3)
  return(df_dat[df])
}

calc_dayno <- function(posixdate){
  as.integer(as.numeric(posixdate - as.POSIXct("2020-01-01 00:00:00")))
}

#
get_column_specifications_from_recent_file <- function(file_path, scale_pattern){
  df_temp <- read.csv(file_path, stringsAsFactors = FALSE)
  var <- names(df_temp)[grepl("^latest[.]value", names(df_temp))]
  name <- sub("latest.value_", "", var)
  scale <- grepl(scale_pattern, var)
  return (list("var" = var, "name" = name, "scale" = scale))
}

#
add_dict_entries <- function(dict, df, dict_path, source, vars_spec){
  dict_temp <- read.csv(dict_path, stringsAsFactors = FALSE)
  dict_temp$source <- source
  dict_temp$calc.per.capita <- NA
  dict_temp <-
    merge(dict_temp, vars_spec, by.x = "variable", by.y = "name")
  dict_temp$calc.per.capita <- dict_temp$scale
  dict_temp <- dict_temp[, !names(dict_temp) %in% c("scale", "var")]
  # only include existing columns
  dict_temp <- dict_temp[dict_temp$variable %in% names(df),]
  rbind(dict, dict_temp)
}

#
add_dict_entries_for_timed_variable <- function(dict, df, var_regexp, source){
  dict_temp <- as.data.frame(matrix(names(df)[grepl(var_regexp, names(df))], ncol = 1, byrow =T))
  colnames(dict_temp)[1] <- "variable"
  dict_temp$description <- NA
  dict_temp$source <- basename(file_path)
  dict_temp$calc.per.capita <- F
  rbind(dict, dict_temp)
}

#
add_dict_entries_from_vars_spec <- function(dict, df, source, vars_spec){
  dict_temp <- as.data.frame(matrix(vars_spec$name, ncol = 1, byrow =T))
  colnames(dict_temp)[1] <- "variable"
  dict_temp$description <- NA
  dict_temp$source <- source
  dict_temp$calc.per.capita <- NA
  dict_temp <-
    merge(dict_temp, vars_spec, by.x = "variable", by.y = "name")
  dict_temp$calc.per.capita <- dict_temp$scale
  dict_temp <- dict_temp[, !names(dict_temp) %in% c("scale", "var")]
  # only include existing columns
  dict_temp <- dict_temp[dict_temp$variable %in% names(df),]
  rbind(dict, dict_temp)
}

#
add_dict_entries_from_dictionary_and_vars_spec <- function(dict, df, dict_path, source, vars_spec){
  dict_temp1 <- read.csv(dict_path, stringsAsFactors = FALSE)
  dict_temp <- as.data.frame(matrix(vars_spec$name, ncol = 1, byrow =T))
  colnames(dict_temp)[1] <- "variable"
  dict_temp <- merge(dict_temp, dict_temp1, by.x = "variable", by.y = "variable")
  dict_temp$source <- source
  dict_temp$calc.per.capita <- NA
  dict_temp <-
    merge(dict_temp, vars_spec, by.x = "variable", by.y = "name")
  dict_temp$calc.per.capita <- dict_temp$scale
  dict_temp <- dict_temp[, !names(dict_temp) %in% c("scale", "var")]
  # only include existing columns
  dict_temp <- dict_temp[dict_temp$variable %in% names(df),]
  rbind(dict, dict_temp)
}

########## PREPARATION ##########

na_percentage <- .2

data_path <- "data"
input_filename <- "RMD30_Caspar van Lissa_2020-05-27 20-33 CEST.csv"
#input_filename <- "RMD30_small.csv"
output_filename <- "df_raw_merged.csv"
#output_filename <- "df_raw_merged_small.csv"
df <- suppressWarnings(read.csv(paste(data_path, input_filename, sep = "/"), stringsAsFactors = FALSE))
names(df) <- tolower(names(df)) 
df$startdate <- as.POSIXct(df$startdate)
df$date <- format(df$startdate, "%d_%m_%Y")
df$dayresponse <- calc_dayno(as.POSIXct(df$startdate))
df$rownum <- 1:nrow(df)
dict <- data.frame(variable=character(), description=character(), source=character(), calc.per.capita=integer())


# Originally in merging function: -----------------------------------------

# ISO3 country code (incl XKV for Kosovo)
if(is.null(df[["countryiso3"]])){
  df$countryiso3 <- ifelse(df$coded_country == "Kosovo", 
                           "XKV", 
                           countrycode::countrycode(df$coded_country, origin = "country.name", destination = "iso3c", warn = FALSE))
}

# remove rows witout countryiso3
missing_countries_df <- unique(subset(df,is.na(df$countryiso3))$coded_country) 
message("Removed rows from df for the following countries without ISO3 code:")
message(subset(df,is.na(df$countryiso3))$coded_country)
df <- df[!is.na(df$countryiso3), ]


########## MERGE DATA ###########################################################################
scale_these <- vector("character")
df_dat <- read.csv(file.path(data_path, "FCTB_POPULATION/recent_fctb_population.csv"), stringsAsFactors = FALSE)
names(df_dat)[4] <- "population"
df <- merge_cj(df, df_dat[, c("countryiso3", "population")])

df_dat <- read.csv(paste(data_path, "CSSE/CSSE.csv", sep = "/"), stringsAsFactors = FALSE)
names(df_dat)
df <- merge_cj(df, df_dat)

df_dat <- read.csv(paste(data_path,
                         "OxCGRT/OxCGRT_Oxford_regulation_policies.csv",
                         sep = "/"), stringsAsFactors = FALSE)
df_dat <- df_dat[, !(startsWith(names(df_dat), "legacy") |
                       startsWith(names(df_dat), "confirmedcases")|
                       startsWith(names(df_dat), "confirmeddeaths")|
                       startsWith(names(df_dat), "stringencyindexfordisplay"))]
vars <- unique(gsub("_\\d{4}\\.\\d{2}\\.\\d{2}$", "", names(df_dat)))
vars <- vars[!vars %in% gsub("_flag", "", vars[endsWith(vars, "flag")])]
keep_these <- apply(sapply(vars, startsWith, x = names(df_dat)), 1, any)
df_dat <- df_dat[, keep_these]
names(df_dat)[1000:1010]
names(df_dat) <- gsub("^(.+?)_(\\d{4})\\.(\\d{2})\\.(\\d{2})$", "\\1\\.\\4_\\3_\\2", names(df_dat))
names(df_dat) <- gsub("\\.(?!\\d)", "", names(df_dat), perl = TRUE)
df <- merge_cj(df, df_dat)

# CSSE
df_dat <- read.csv(file.path(data_path, "GHS/preparedness.csv"), stringsAsFactors = FALSE)[1:3]
names(df_dat)[3] <- "ghsscore"
scale_these <- c(scale_these, "ghsscore")
df <- merge_cj(df, df_dat)

# Google mobility

#df_dat <- read.csv(file.path(data_path, "google_mobility/mobility_March_29_2020.csv"), stringsAsFactors = FALSE)

# Mobility
# WB_ISAIRDPRT # Airport departures
df_dat <- read.csv(file.path(data_path, "WB_ISAIRDPRT/recent_air_departures.csv"), stringsAsFactors = FALSE)[c(2,4)]
names(df_dat)[2] <- "airdepartures"
scale_these <- c(scale_these, "airdepartures")
df <- merge_cj(df, df_dat)

# WB_STINTXPNDMPZS # Tourism expenditures
df_dat <- read.csv(file.path(data_path, "WB_STINTXPNDMPZS/recent_intl_tourism_expenditures.csv"), stringsAsFactors = FALSE)[c(2,4)]
names(df_dat)[2] <- "tourismexpenditures"
scale_these <- c(scale_these, "tourismexpenditures")
df <- merge_cj(df, df_dat)

# WHO_OECD
df_dat <- read.csv(file.path(data_path, "WHO_OECD/WHO_OECD_health_infrastructure.csv"), stringsAsFactors = FALSE)[, c("countryiso3", "hospital_beds_per_1000", "doctors_per_10k", "nurses_and_midwifery_per_10k", "che_perc_of_gdp_2017")]
df <- merge_cj(df, df_dat)

# WB Governance indicators
df_dat <- read.csv(file.path(data_path, "WB_GOV/recent_wb_government_effectiveness.csv"), stringsAsFactors = FALSE)
df_dat <- df_dat[, c("countryiso3", grep("^latest.value_estimate", names(df_dat), value = TRUE))]
names(df_dat)[-1] <- c("controlcorruption", "ruleoflaw", "politicalstability", "voiceaccountability", "govteffectiveness", "regulatoryquality")
df <- merge_cj(df, df_dat)

########## WRITE_RESULT ###################################################################
write.csv(df, paste(data_path, output_filename, sep = "/"), row.names = FALSE)
write.csv(dict, paste(data_path, "data_dictionary_merged.csv", sep ="/"), row.names = FALSE)



if(FALSE){
  ########## FCTB_POPULATION #################################################################
  # Population estimates.
  # Required also for calculating values per capita for other data sources
  
  if (T) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_POPULATION/recent_fctb_population.csv",
            sep = "/")
    df_dat <- read.csv(file_path, stringsAsFactors = FALSE)
    names(df_dat)[4] <- "population"
    df <- merge_cj(df, df_dat[, c("countryiso3", "population")])
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_population.2119")
    name <- c("population.2119")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## CSSE ############################################################################
  # Data repository for the 2019 Novel Coronavirus Visual Dashboard operated by the JHU CSSE.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <- paste(data_path, "CSSE/CSSE.csv", sep = "/")
    
    
    
    df %>%
      merge_file_by_countryiso3(file_path, na_percentage) -> tmp %>%
      replace_date_variables_with_dayno(
        date_regexp = "[0-9][0-9]_[0-9][0-9]_[0-9][0-9][0-9][0-9]",
        date_regexp_w_groups = "[^.]*[.]([0-9][0-9])_([0-9][0-9])_([0-9][0-9][0-9][0-9])",
        date_subst_posix = "\\3/\\1/\\2",
        prefix_regexp_w_group = "([^.]*)[.].*"
      ) %>%
      replace_dated_variables_based_on_response_date() -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_for_timed_variable(df, "^confirmed", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^deaths", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^recovered", basename(file_path)) -> dict
  }
  
  ########## OxCGRT ##########################################################################
  # The Oxford COVID-19 Government Response Tracker.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "OxCGRT/OxCGRT_Oxford_regulation_policies.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    df %>%
      merge_file_by_countryiso3(file_path, na_percentage) %>%
      replace_date_variables_with_dayno(
        date_regexp = "_[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]",
        date_regexp_w_groups = "^.*_([0-9][0-9][0-9][0-9]).([0-9][0-9]).([0-9][0-9])",
        date_subst_posix = "\\1/\\2/\\3",
        prefix_regexp_w_group = "(^.*)_.*"
      ) %>%
      replace_dated_variables_based_on_response_date() -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_for_timed_variable(df, "^school.closing", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^workplace.closing", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^cancel.public.events", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^restrictions.on.gatherings", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^close.public.transport", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^stay.at.home.requirements", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^restrictions.on.internal.movement", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^international.travel.controls", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^income.support", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^debt.contract.relief", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^fiscal.measures", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^international.support", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^public.information.campaigns", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^testing.policy", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^contact.tracing", basename(file_path)) %>%
      add_dict_entries_for_timed_variable(df, "^emergency.investment.in.healthcare", basename(file_path)) -> dict
  }
  
  ########## FCTB_AIRPORTS ###################################################################
  # This entry gives the total number of airports or airfields recognizable from the air.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "FCTB_AIRPORTS/recent_fctb_airports.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_AIRPORTS/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_airports.2053")
    name <- c("airports.2053")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_BIRTH_RATE #################################################################
  # Birth rate compares the average annual number of births during a year per 1,000 persons.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_BIRTH_RATE/recent_fctb_birth_rate.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_BIRTH_RATE/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_birthrate.2054")
    name <- c("birthrate.2054")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_CHILDREN_UNDERWEIGHT #########################################################
  # Children under the age of 5 years underweight gives the percent of children under five considered to be underweight.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(
        data_path,
        "FCTB_CHILDREN_UNDERWEIGHT/recent_fctb_children_underweight.csv",
        sep = "/"
      )
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_CHILDREN_UNDERWEIGHT/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_childrenunderweight.2224")
    name <- c("childrenunderweight.2224")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_DEATH_RATE #########################################################
  # Death rate compares the average annual number of deaths during a year per 1,000 population at midyear,
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_DEATH_RATE/recent_fctb_death_rate.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_DEATH_RATE/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_deathrate.2066")
    name <- c("deathrate.2066")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_EDUCATION_EXPENDITURES #########################################################
  # Education expenditures compares the public expenditure on education as a percent of GDP.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(
        data_path,
        "FCTB_EDUCATION_EXPENDITURES/recent_fctb_education_expenditures.csv",
        sep = "/"
      )
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_EDUCATION_EXPENDITURES/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_educationexpenditures.2206")
    name <- c("educationexpenditures.2206")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_FERTILITY_RATE #########################################################
  # Total fertility rate (TFR) compares figures for the average number of children that would be born per woman.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_FERTILITY_RATE/recent_fctb_fertility_rate.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_FERTILITY_RATE/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_fertilityrate.2127")
    name <- c("fertilityrate.2127")
    scale <- c(FALSE)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_HEALTH_EXPENDITURES #########################################################
  # Health expenditures provides the total expenditure on health as a percentage of GDP.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(
        data_path,
        "FCTB_HEALTH_EXPENDITURES/recent_fctb_health_expenditures.csv",
        sep = "/"
      )
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_HEALTH_EXPENDITURES/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_healthexpenditures.2225")
    name <- c("healthexpenditures.2225")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_HIV_DEATHS #########################################################
  # HIV/AIDS - deaths compares the number of adults and children who died of AIDS during a given calendar year.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_HIV_DEATHS/recent_fctb_hiv_deaths.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_HIV_DEATHS/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_hivdeaths.2157")
    name <- c("hivdeaths.2157")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_HIV_RATE_ADULT #########################################################
  # HIV/AIDS - adult prevalence rate compares the percentage of adults (aged 15-49) living with HIV/AIDS.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_HIV_RATE_ADULT/recent_fctb_hiv_rate_adult.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_HIV_RATE_ADULT/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_hivrateadult.2155")
    name <- c("hivrateadult.2155")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_HIV_RATE_ALL #########################################################
  # HIV/AIDS - adult prevalence rate compares the percentage of adults (aged 15-49) living with HIV/AIDS.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_HIV_RATE_ALL/recent_fctb_hiv_rate_all.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_HIV_RATE_ALL/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_hivrateall.2156")
    name <- c("hivrateall.2156")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_INFANT_MORTALITY #########################################################
  # Infant mortality rate compares the number of deaths of infants under one year old in a
  # given year per 1,000 live births in the same year.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_INFANT_MORTALITY/recent_fctb_infant_mortality.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_INFANT_MORTALITY/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_infantmortality.2091")
    name <- c("infantmortality.2091")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_LIFE_EXPECTANCY #########################################################
  # This entry contains the average number of years to be lived.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_LIFE_EXPECTANCY/recent_fctb_life_expectancy.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_LIFE_EXPECTANCY/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_lifeexpectancy.2102")
    name <- c("lifeexpectancy.2102")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_MATERNAL_MORTALITY #########################################################
  # The annual number of female deaths per 100,000 live births.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_MATERNAL_MORTALITY/recent_fctb_maternal_mortality.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_MATERNAL_MORTALITY/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_maternalmortality.2223")
    name <- c("maternalmortality.2223")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_MEDIAN #########################################################
  # Median age.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "FCTB_MEDIAN/recent_fctb_median.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_MEDIAN/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_median.2177")
    name <- c("median.2177")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_MERCHANT_MARINE #################################################
  # Merchant marine compares all ships engaged in the carriage of goods; or all
  # commercial vessels (as opposed to all nonmilitary ships), which excludes tugs, fishing vessels, offshore oil rigs, etc.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_MERCHANT_MARINE/recent_fctb_merchant_marine.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_MERCHANT_MARINE/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_merchantmarine.2108")
    name <- c("merchantmarine.2108")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_MIGRATION_RATE #################################################
  # Net migration rate compares the difference between the number of persons
  # entering and leaving a country during the year per 1,000 persons.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_MIGRATION_RATE/recent_fctb_migration_rate.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_MIGRATION_RATE/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_migrationrate.2112")
    name <- c("migrationrate.2112")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_OBESITY ########################################################
  # Obesity - adult prevalence rate gives the percent of a country's population
  # considered to be obese.
  
  # activate F set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "FCTB_OBESITY/recent_fctb_obesity.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_OBESITY/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_obesity.2228")
    name <- c("obesity.2228")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_POPULATION_GROWTH ########################################################
  # Population growth rate compares the average annual percent change in populations, resulting
  # from a surplus (or deficit) of births over deaths and the balance of migrants entering and
  # leaving a country. The rate may be positive or negative.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_POPULATION_GROWTH/recent_fctb_population_growth.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION_GROWTH/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_populationgrowth.2002")
    name <- c("populationgrowth.2002")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_RAILWAYS ########################################################
  # Total route length of the railway network.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "FCTB_RAILWAYS/recent_fctb_railways.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_RAILWAYS/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_railways.2121")
    name <- c("railways.2121")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_ROADWAYS ########################################################
  # Roadways compares the total length of the road network.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "FCTB_ROADWAYS/recent_fctb_roadways.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_ROADWAYS/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_roadways.2085")
    name <- c("roadways.2085")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_UNEMPLOYMENT_YOUTH ########################################################
  # Unemployment, youth ages 15-24 gives the percent of the total labor force ages 15-24 unemployed.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_UNEMPLOYMENT_YOUTH/recent_fctb_unemployment_youth.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_UNEMPLOYMENT_YOUTH/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.value_unemploymentyouth.2229")
    name <- c("unemploymentyouth.2229")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## FCTB_WATERWAYS ########################################################
  # Waterways compares the total length of navigable rivers, canals, and other inland bodies of water.
  # Per 1,000,000 people.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "FCTB_WATERWAYS/recent_fctb_waterways.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_WATERWAYS/data_dictionary.csv",
            sep = "/")
    
    # column specifications
    var <- c("latest.year_waterways.2093")
    name <- c("waterways.2093")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## GHS ##################################################################
  # Global health security capabilities in 195 countries.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "GHS/preparedness.csv", sep = "/")
    
    # column specifications
    var <- c("score")
    name <- c("ghs.score")
    scale <- c(F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_vars_spec(df, basename(file_path), vars_spec) -> dict
  }
  
  ########## google_mobility ##################################################################
  # Retail & recreation: Mobility trends for places like restaurants, cafes, shopping centers,
  # theme parks, museums, libraries, and movie theaters.
  # Grocery & pharmacy: Mobility trends for places like grocery markets, food warehouses,
  # farmers markets, specialty food shops, drug stores, and pharmacies.
  # Parks: Mobility trends for places like local parks, national parks, public beaches,
  # marinas, dog parks, plazas, and public gardens.
  # Transit stations: Mobility trends for places like public transport hubs such as subway,
  # bus, and train stations.
  # Residential: Mobility trends for places of residence.
  # Workplaces: Mobility trends for places of work.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 20
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "google_mobility/mobility_March_29_2020.csv",
            sep = "/")
    
    # column specifications
    var <-
      c(
        "retail_recreation",
        "grocery_pharmacy",
        "parks",
        "transit_stations",
        "workplaces",
        "residential"
      )
    name <-
      c(
        "retail_recreation",
        "grocery_pharmacy",
        "parks",
        "transit_stations",
        "workplaces",
        "residential"
      )
    scale <- c(F, F, F, F, F, F)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_vars_spec(df, basename(file_path), vars_spec) -> dict
  }
  
  ########## OWID_Tests ##################################################################
  # Global health security capabilities in 195 countries.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "OWID_Tests/OurWorldInData_Tests.csv", sep = "/")
    
    # column specifications
    var <- c("total_tests")
    name <- c("total_tests")
    scale <- c(T)
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_vars_spec(df, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_BUREAUCRACY ##################################################################
  # The Worldwide Bureaucracy Indicators (WWBI) is a dataset on public sector employment and
  # wages that can help researchers and development practitioners gain a better understanding
  # of the personnel dimensions of state capability, the footprint of the public sector on the
  # overall labor market, and the fiscal implications of the government wage bill.
  
  # activate data set?
  active <- T
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_BUREAUCRACY/recent_bureaucracy_indicators.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "WB_BUREAUCRACY/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "Number.of.employees")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_BUSINESS ##################################################################
  # The Doing Business project provides objective measures of business regulations and their
  # enforcement across 190 economies. Economies are ranked on their ease of doing business,
  # from 1-190. The rankings are determined by sorting the aggregate scores (formerly called
  # distance to frontier) on 10 topics, each consisting of several indicators, giving equal
  # weight to each topic.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "WB_BUSINESS/recent_doing_business.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_DEV ##################################################################
  # World development indicators
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_DEV/recent_world_development_indicators.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <- get_column_specifications_from_recent_file(file_path,
                                                       "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_EDUCATION ##################################################################
  # Education indicators
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_EDUCATION/recent_education_statistics.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_ENPOPDNST ##################################################################
  # Population density is midyear population divided by land area in square kilometers.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_ENPOPDNST/recent_population_density.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_ENPOPSLUMURZS ##################################################################
  # Population living in slums (% of urban population).
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_ENPOPSLUMURZS/recent_population_slums.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_FAILED ##################################################################
  # Worldbank failed state index.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_FAILED/recent_failed_states_index.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_FPCPITOTLZG ##################################################################
  # Consumer price index reflects changes in the cost to the average consumer of acquiring a basket of goods and services.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_FPCPITOTLZG/recent_consumer_prices.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_FREEDOM ##################################################################
  # Political rights and civil liberties.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "WB_FREEDOM/recent_freedom_house.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_GENDER ##################################################################
  # The Gender Statistics database is a comprehensive source for the latest sex-disaggregated
  # data and gender statistics covering demography, education, health, access to economic
  # opportunities, public life and decision-making, and agency.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "WB_GENDER/recent_gender_statistics.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_GOV ##################################################################
  # Governance consists of the traditions and institutions by which authority in a country is exercised.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_GOV/recent_wb_government_effectiveness.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_GOVERNANCE ##################################################################
  # The Global Indicators of Regulatory Governance present new measures of transparency, civic
  # participation and government accountability across the life cycle of regulations.
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_GOVERNANCE/recent_regulatory_governance.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_INSTITUTIONAL ##################################################################
  # The CPIA rates countries against a set of 16 criteria grouped in four clusters: (i) economic
  # management; (ii) structural policies; (iii) policies for social inclusion and equity; and (iv)
  # public sector management and institutions. The criteria are focused on balancing the capture
  # of the key factors that foster growth and poverty reduction
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_INSTITUTIONAL/recent_institutional_profiles.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_ISAIRDPRT ##################################################################
  # Air transport, registered carrier departures worldwide
  # Per 1,000,000 people
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path, "WB_ISAIRDPRT/recent_air_departures.csv", sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <- get_column_specifications_from_recent_file(file_path, ".")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_ITCELSETSP2 ##################################################################
  # Mobile cellular subscriptions (per 100 people)
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_ITCELSETSP2/recent_cellular_subscriptions.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  
  ########## WB_LOGISTICS ##################################################################
  # The Logistics Performance Index overall score reflects perceptions of a country's logistics
  # performance based on the efficiency of the customs clearance process, quality of trade-
  # and transport-related infrastructure, ease of arranging competitively priced international
  # shipments, quality of logistics services, ability to track and trace consignments, and
  # frequency with which shipments reach the consignee within the scheduled time.
  # The index ranges from 1 to 5, with a higher score More...
  
  # activate data set?
  active <- F
  
  # drop merged columns with more na values than na_percentage of rows (specify 100 to ignore)
  na_percentage <- 100
  
  if (active) {
    # data path
    file_path <-
      paste(data_path,
            "WB_LOGISTICS/recent_logistics_performance_index.csv",
            sep = "/")
    
    # dict path
    dict_path <-
      paste(data_path,
            "FCTB_POPULATION/data_dictionary.csv",
            sep = "/")
    
    # get column specifications
    spec <-
      get_column_specifications_from_recent_file(file_path, "xxxxx")
    var <- spec$var
    name <- spec$name
    scale <- spec$scale
    vars_spec <- data.frame(var, name, scale)
    
    # merge data
    df %>%
      merge_file_by_countryiso3_variables(file_path,
                                          vars_spec,
                                          na_percentage) -> df
    
    # add dict entries
    dict %>%
      add_dict_entries_from_dictionary_and_vars_spec(df, dict_path, basename(file_path), vars_spec) -> dict
  }
  ########## WRITE_RESULT ###################################################################
  write.csv(df, paste(data_path, output_filename, sep = "/"), row.names = FALSE)
  write.csv(dict, paste(data_path, "data_dictionary_merged.csv", sep ="/"), row.names = FALSE)
  
}


