########## PREPARATION ##########
library(glmnet)
library(caTools)
library(tidyverse)
library(ggplot2)
library(worcs)
library(tidySEM)
library(dplyr)
library(countrycode)
library(tidyr)
library(data.table)
########## FUNCTIONS ##########

merge_dat <- function(df, df_dat, date_regexp = "\\d{2}_\\d{2}_\\d{4}$"){
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

########## READ AND PREPARE DATA ##########
df <- read.csv("data/RMD30_Caspar van Lissa_2020-05-27 20-33 CEST.csv", stringsAsFactors = FALSE) # read in raw data

# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
names(df) <- tolower(names(df))        # all names to lower
long <- grep("^w\\d", names(df), value = TRUE)
long <- table(gsub("^w\\d_", "", long))
wave_all <- names(long)[long == 8]
wave_one <- names(long)[long == 1]

#df <- df[, !grepl("^w\\d", names(df))] # main data file we will use; only baseline measurements
df <- df[!is.na(df$startdate), ]

# Create training/testing split (training will be split into (cross-) validation samples)
set.seed(953007)
if(file.exists("split_sample.csv")){
  the_split <- read.csv("split_sample.csv")
  not_split <- which(!df$responseid %in% the_split$responseid)
  df <- merge(df, the_split, by = "responseid", all.x = TRUE)
  if(length(not_split) > 0){
    new <- as.logical(rbinom(length(not_split), 1, .7))
    df$train[not_split] <- new
    write.csv(df[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
  }
} else {
  df$train <- as.logical(rbinom(nrow(df), 1, .7))
  write.csv(df[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
}

# getting percentags of countries' counts
country_percentages <- df %>% group_by(coded_country) %>% 
  summarise(perc_of_resp = 100*(n()/nrow(df))) 

# Select only countries with > 1%?
retain_countries <- country_percentages$coded_country[country_percentages$perc_of_resp > 1]
df <- df[which(df$coded_country %in% retain_countries),] # removing countries that are < 1% of data

# Descriptive stats
table_descriptives <- tidySEM::descriptives(df)
# 
# # Some variables might be spread out over several columns; check codebook
# vars <- grep("_\\d$", names(df), value = TRUE)
# # For example, coronaclose:
df$coronaclose <- apply(df[, grep("^coronaclose_\\d", names(df))], 1, function(i){which(i == 1)[1]})
df[, grep("^coronaclose_\\d", names(df))] <- NULL

# Recode: 1 = unemployed, 2 = 1-23 hours, 3 = 24-39 etc
df$employstatus <- apply(df[, c(grep("^employstatus_[45]$", names(df)), grep("^employstatus_[123]$", names(df)))], 1, function(i){which(i == 1)[1]})
# Who is still missing
still_missing <- is.na(df$employstatus)
# Classify as not working: Homemaker, Retired, Disabled, Student, or Volunteering
is_notworking <- apply(df[still_missing, grep("^employstatus_([6789]|10)$", names(df))], 1, function(i){any(i == 1)})
df$employstatus[still_missing][is_notworking] <- 1
df[, grep("^employstatus_\\d{0,}$", names(df))] <- NULL

# House leave
# Recode to dummies: If people filled out any answers about houseleave, then NAs should really be 0.
not_all_missing <- rowSums(is.na(df[, startsWith(names(df), "houseleave")])) < 6
df[not_all_missing, startsWith(names(df), "houseleavewhy")] <- matrix(as.numeric(!is.na(df[not_all_missing, startsWith(names(df), "houseleavewhy")])), ncol = 5)
# Keep all house leave variables, because they are dummies

# here I remove all of the columns that are not needed for analysis. I also remove the "ranking" features as I am not 
# sure how to use them in analysis. (MAYBE RE-ADD LATER)
df <- df %>% select(-enddate, -recordeddate, -polorx, -polory, -polorcat, -language, -contains("rank"))

# Create scales -----------------------------------------------------------

vars <- unique(gsub("\\d+$", "", grep("(?<!_)\\d$", names(df), value = TRUE, perl = TRUE)))
scales_list <- lapply(vars, function(i){ grep(paste0("^", i, "\\d+$"), names(df), value = TRUE)})
names(scales_list) <- vars
scales_list[sapply(scales_list, length) < 2] <- NULL
# Do we need to check other reverse coded variables?
rev <- c("jbinsec02", "disc03", "bor03")
#descriptives(df[, rev])
df$bor03 <- 4-df$bor03
df$jbinsec02 <- -1*df$jbinsec02
df$disc03 <- -1*df$disc03
# Create the scales
scales <- create_scales(df, keys.list = scales_list)
write.csv(scales$descriptives, "scale_descriptives.csv", row.names = FALSE)
# View(scales$descriptives) # commenting this out so it doesn't open every time we run the modelling script
# Add to df
df <- cbind(df, scales$scores[scales$descriptives$Reliability > .65])
# Remove items
df[unlist(scales_list)] <- NULL

# Startdate to numeric
df$startdate <- as.POSIXct(df$startdate)
#df$time_ago <- as.numeric(df$startdate-Sys.time())
#df$startdate <- NULL

# Change the type to integer where appropriate
# first get the list of columns that already contain integer values 
int_columns_list = c()
for (col_n in 1:dim(df)[2]) {
  
  current_column_values <-  df[col_n]
  current_column_name <- colnames(df[col_n])
  
  if (is.numeric(current_column_values[1, ])) {
    if (all((current_column_values%%1 == 0) == TRUE, na.rm=T)) {
      int_columns_list = c(int_columns_list, current_column_name)}
  }
}
# change the type to integer for the appropriate columns 
df <- df %>% 
  mutate_at(int_columns_list, as.integer) %>% 
  dplyr::select(-x)

output_filename <- "df_raw_merged.csv"
#output_filename <- "df_raw_merged_small.csv"

df$date <- format(df$startdate, "%d_%m_%Y")
df$rownum <- 1:nrow(df)

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
df_dat <- read.csv(file.path("data", "FCTB_POPULATION/recent_fctb_population.csv"), stringsAsFactors = FALSE)
names(df_dat)[4] <- "population"
df <- merge_dat(df, df_dat[, c("countryiso3", "population")])

# CSSE
df_dat <- read.csv(paste("data", "CSSE/CSSE.csv", sep = "/"), stringsAsFactors = FALSE)
scale_these <- c(scale_these, "confirmed", "deaths", "recovered")
df <- merge_dat(df, df_dat)

df_dat <- read.csv(file.path("data",
                         "OxCGRT/OxCGRT_Oxford_regulation_policies.csv"), stringsAsFactors = FALSE)
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
df <- merge_dat(df, df_dat)

# GHS
df_dat <- read.csv(file.path("data", "GHS/preparedness.csv"), stringsAsFactors = FALSE)[1:3]
names(df_dat)[3] <- "ghsscore"
df <- merge_dat(df, df_dat)

# Mobility
# WB_ISAIRDPRT # Airport departures
df_dat <- read.csv(file.path("data", "WB_ISAIRDPRT/recent_air_departures.csv"), stringsAsFactors = FALSE)[c(2,4)]
names(df_dat)[2] <- "airdepartures"
scale_these <- c(scale_these, "airdepartures")
df <- merge_dat(df, df_dat)

# WB_STINTXPNDMPZS # Tourism expenditures
df_dat <- read.csv(file.path("data", "WB_STINTXPNDMPZS/recent_intl_tourism_expenditures.csv"), stringsAsFactors = FALSE)[c(2,4)]
names(df_dat)[2] <- "tourismexpenditures"
df <- merge_dat(df, df_dat)

# WHO_OECD
df_dat <- read.csv(file.path("data", "WHO_OECD/WHO_OECD_health_infrastructure.csv"), stringsAsFactors = FALSE)[, c("countryiso3", "hospital_beds_per_1000", "doctors_per_10k", "nurses_and_midwifery_per_10k", "che_perc_of_gdp_2017")]
df <- merge_dat(df, df_dat)

# WB Governance indicators
df_dat <- read.csv(file.path("data", "WB_GOV/recent_wb_government_effectiveness.csv"), stringsAsFactors = FALSE)
df_dat <- df_dat[, c("countryiso3", grep("^latest.value_estimate", names(df_dat), value = TRUE))]
names(df_dat)[-1] <- c("controlcorruption", "ruleoflaw", "politicalstability", "voiceaccountability", "govteffectiveness", "regulatoryquality")
df <- merge_dat(df, df_dat)

df[, (scale_these) := .SD/population, .SDcols = scale_these]


########## PREPARE DATA ##########

# Removing NA participants & vars
miss                    <- is.na(df)
thres_miss_col      <- .2 # remove features/variables that have more than X% NAs

df                      <- df[, which((colSums(miss)/nrow(df)) > thres_miss_col) := NULL]
thres_miss_row     <- 40/ncol(df) # remove participants that have more than X% NAs
df                      <- df[!(rowSums(miss)/ncol(df)) > thres_miss_row, ]

# Drop testing cases
df_test <- df[!df$train, ]
df_train <- df[df$train, ]

# We're doing this in the modelling script
# library(missRanger)
# set.seed(4639)
# imp <- missRanger(df_train)
# write.csv(imp, "df_train_imputed.csv", row.names = FALSE)

write.csv(df_test, "testing.csv", row.names = FALSE)
write.csv(df_train, "training.csv", row.names = FALSE)
