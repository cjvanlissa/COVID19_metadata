########## TO DO ##########

# TODO: think about what to do with NAs
# TODO: add ways of measurement (yes/no, Likert, etc.) to comments above variables
# TODO: merge with metadata
# TODO: think about other DVs
# TODO: think about the unbalance in DV

########## PREPARATION ##########
library(glmnet)
library(caTools)
library(tidyverse)
library(ggplot2)
if(!require("worcs")){
  library(remotes)
  install_github("cjvanlissa/worcs", upgrade ="never")
  library(worcs)
}
if(!require("tidySEM")){
  install_github("cjvanlissa/tidySEM", upgrade ="never")
  library(tidySEM)
}

########## READ AND PREPARE DATA ##########
df <- read.csv("data/RMD30_Caspar van Lissa_2020-05-27 20-33 CEST.csv", stringsAsFactors = FALSE) # read in raw data
#df <- read_csv("data/RMD30_Caspar van Lissa_2020-05-13 17-24 CEST.csv") # read in raw data

# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
names(df) <- tolower(names(df))        # all names to lower
df <- df[, !grepl("^w\\d", names(df))] # main data file we will use; only baseline measurements
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
table_descriptives <- descriptives(df)

# Some variables might be spread out over several columns; check codebook
vars <- grep("_\\d$", table_descriptives$name[table_descriptives$unique < 4], value = TRUE)
# For example, coronaclose:
df$coronaclose <- apply(df[, grep("^coronaclose_\\d", names(df))], 1, function(i){which(i == 1)[1]})
df[, grep("^coronaclose_\\d", names(df))] <- NULL

# Recode: 1 = unemployed, 2 = 1-23 hours, 3 = 24-39 etc
df$employstatus <- apply(df[, c(grep("^employstatus_[45]$", names(df)), grep("^employstatus_[123]$", names(df)))], 1, function(i){which(i == 1)[1]})
# Who is still missing
still_missing <- is.na(df$employstatus)
# Classify as not working: Homemaker, Retired, Disabled, Student, or Volunteering
is_notworking <- apply(df[still_missing, grep("^employstatus_([6789]|10)$", names(df))], 1, function(i){any(i == 1)})
df$employstatus[still_missing][is_notworking] <- 1
df[, grep("^employstatus_\\d+$", names(df))] <- NULL

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
df <- cbind(df, scales$scores)
# Remove items
df[unlist(scales_list)] <- NULL

# Startdate to numeric
df$startdate <- as.POSIXct(df$startdate)
df$time_ago <- as.numeric(df$startdate-Sys.time())
df$startdate <- NULL

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

########## PREPARE DATA ##########

# Removing NA participants & vars
partic_NA_tres_perc     <- .2 # remove participants that have more than X% NAs
partic_NA_tres_var      <- .2 # remove features/variables that have more than X% NAs

miss       <- is.na(df)
df <- df[!(rowSums(miss)/ncol(df)) > partic_NA_tres_perc,
         -which((colSums(miss)/nrow(df)) > partic_NA_tres_var)]

desc <- descriptives(df)

# Drop testing cases
df_test <- df[!df$train, ]
df_train <- df[df$train, ]

library(missRanger)
set.seed(4639)
imp <- missRanger(df_train)
write.csv(imp, "df_train_imputed.csv", row.names = FALSE)

write.csv(df_test, "testing.csv", row.names = FALSE)
write.csv(df_train, "training.csv", row.names = FALSE)
