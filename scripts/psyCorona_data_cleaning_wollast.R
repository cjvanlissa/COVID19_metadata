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
#df <- foreign::read.spss("PsyCorona_Data_Wollast_4.sav", to.data.frame = TRUE) # read in raw data
df <- read.csv("data/PsyCorona_Data_Wollast.csv", stringsAsFactors = FALSE, row.names = 1)
# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
df$startdate <- df$RecordedDate
df$date <- format(as.POSIXct(df$startdate), "%d_%m_%Y")

names(df) <- tolower(names(df))        # all names to lower
df$rownum <- 1:nrow(df)
full_df <- df

# Remove cases with missing date or country
missing_dates <- which(is.na(df$startdate))
df <- df[-missing_dates, ]

# remove rows witout countryiso3

# ISO3 country code (incl XKV for Kosovo)
if(is.null(df[["countryiso3"]])){
  df$countryiso3 <- ifelse(df$coded_country == "Kosovo", 
                           "XKV", 
                           countrycode::countrycode(df$coded_country, origin = "country.name", destination = "iso3c", warn = FALSE))
}

missing_countries_df <- unique(subset(df,is.na(df$countryiso3))$coded_country)
message("Removed rows from df for the following countries without ISO3 code:")
message(subset(df,is.na(df$countryiso3))$coded_country)
df <- df[!is.na(df$countryiso3), ]
orig_names <- names(df)


########## MERGE DATA ###########################################################################
scale_these <- vector("character")
df_dat <- read.csv(file.path("data", "FCTB_POPULATION/recent_fctb_population.csv"), stringsAsFactors = FALSE)
names(df_dat)[4] <- "population"
df <- merge_dat(df, df_dat[, c("countryiso3", "population")])

# CSSE
df_dat <- read.csv(paste("data", "CSSE/CSSE.csv", sep = "/"), stringsAsFactors = FALSE)
all_miss <- rowSums(is.na(df_dat)) == (ncol(df_dat)-3)
names(df_dat) <- gsub("\\.(\\d+)_(\\d+)", "\\.\\2_\\1", names(df_dat))

scale_these <- c(scale_these, "confirmed", "deaths", "recovered")
df <- merge_dat(df, df_dat)

# Oxford policy tracker
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

# Drop some duplicate / legacy variables
drop_these <- names(df)[!names(df) %in% c(orig_names, "population", 
  "confirmed", "deaths", "recovered",
  "cancelpublicevents_flag", "closepublictransport_flag", 
  "contacttracing", "containmenthealthindex", 
  "debtcontractrelief", "economicsupportindex",
  "emergencyinvestmentinhealthcare", "fiscalmeasures", "governmentresponseindex", 
  "internationalsupport", 
  "internationaltravelcontrols", "investmentinvaccines", "publicinformationcampaigns_flag", 
  "restrictionsongatherings_flag", "restrictionsoninternalmovement_flag", 
  "schoolclosing_flag", "stayathomerequirements_flag", "stringencyindex", 
  "testingpolicy", 
  "workplaceclosing_flag", "ghsscore", "airdepartures", "tourismexpenditures", 
  "doctors_per_10k", "nurses_and_midwifery_per_10k", "che_perc_of_gdp_2017", 
  "controlcorruption", "ruleoflaw", "politicalstability", "voiceaccountability", 
  "govteffectiveness", "regulatoryquality")]
df[, (drop_these) := NULL]
drop_these <- names(df)[names(df) %in% orig_names[!orig_names == "rownum"]]
df[, (drop_these) := NULL]
df[, (paste0(scale_these, "_pop")) := .SD/population, .SDcols = scale_these]
full_df <- data.table(full_df)

tmp <- merge(full_df, df, by = "rownum", all.x = TRUE)
########## PREPARE DATA ##########

write.csv(tmp, "merged_data_wollast.csv", row.names = FALSE)
haven::write_sav(tmp, "merged_data_wollast.sav")