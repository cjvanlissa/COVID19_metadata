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
library(missRanger)
if(!require("worcs")){
  library(remotes)
  install_github("cjvanlissa/worcs", upgrade ="never")
  library(worcs)
}
if(!require("tidySEM")){
  install_github("cjvanlissa/tidySEM", upgrade ="never")
  library(tidySEM)
}
source("scripts/psyCorona_EDA_plot_function.R")

########## READ AND PREPARE DATA ##########

df_raw <- read_csv("data/RMD30_Caspar van Lissa_2020-05-17 11-46 CEST.csv") # read in raw data
df_raw[df_raw == -99] <- NA
# There's some missing values not coded :(


# All names to lower, to prevent problems with name matching. Please use only
# lowercase (capitalization is all over the place in the original data)
names(df_raw) <- tolower(names(df_raw))        # all names to lower
df_raw <- df_raw[, !grepl("^w\\d", names(df_raw))] # main data file we will use; only baseline measurements


# Create training/testing split (training will be split into (cross-) validation samples)
set.seed(953007)
if(file.exists("split_sample.csv")){
  the_split <- read.csv("split_sample.csv")
  not_split <- which(!df_raw$responseid %in% the_split$responseid)
  df_raw <- merge(df_raw, the_split, by = "responseid", all.x = TRUE)
  if(length(not_split) > 0){
    new <- as.logical(rbinom(length(not_split), 1, .7))
    df_raw$train[not_split] <- new
    write.csv(df_raw[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
  }
} else {
  df_raw$train <- as.logical(rbinom(nrow(df_raw), 1, .7))
  write.csv(df_raw[, c("responseid", "train")], "split_sample.csv", row.names = FALSE)
}

# Drop testing cases; drop longitudinal waves
df <- df_raw[df_raw$train, !grepl("^w\\d", names(df_raw))]
rm(df_raw) # Remove from environment to prevent accidental peaking

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
df <- df %>% select(-enddate, -polorx, -polory, -polorcat, -language, -contains("rank"))

# Create scales -----------------------------------------------------------

vars <- unique(gsub("\\d+$", "", grep("(?<!_)\\d$", names(df), value = TRUE, perl = TRUE)))
scales_list <- lapply(vars, function(i){ grep(paste0("^", i, "\\d+$"), names(df), value = TRUE)})
names(scales_list) <- vars
scales_list[sapply(scales_list, length) < 2] <- NULL
# Do we need to check other reverse coded variables?
rev <- c("jbinsec02", "disc03", "bor03")
descriptives(df[, rev])
df$bor03 <- 4-df$bor03
df$jbinsec02 <- -1*df$jbinsec02
df$disc03 <- -1*df$disc03
# Create the scales
scales <- create_scales(df, keys.list = scales_list)
View(scales$descriptives)
# Add to df
df <- cbind(df, scales$scores)
# Remove items
df[unlist(scales_list)] <- NULL

########## VARIABLE EXPLORATION ##########
hist(df$affanx) 


########## DATAWIDE EXPLORATION ##########
df_analyse <- df

# getting percentags of countries' counts
country_percentages <- df_analyse %>% group_by(coded_country) %>% 
  summarise(perc_of_resp = 100*(n()/nrow(df))) 

# plotting all perentages of countries' counts in the data
ggplot(data=country_percentages,aes(x=coded_country, y=perc_of_resp)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plotting all perentages for countries with over 1% of total responses
country_percentages %>% filter(perc_of_resp >1) %>% 
ggplot(aes(x=coded_country, y=perc_of_resp)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Select only countries with > 1%?
retain_countries <- country_percentages$coded_country[country_percentages$perc_of_resp > 1]

########## EXPLORING NAs ##########

### Exploring NAs per variable ###
tab_desc <- descriptives(df_analyse)
tab_desc <- tab_desc[order(tab_desc$name), ]
missings <- is.na(df_analyse)
by_var <- colSums(missings)


# getting NAs for categorical variables (employstatus_xx, coronaClose_xx, houseLeaveWhy_xx)
df_onlycat <- df_analyse %>% select(x1, contains("employstatus"), contains("coronaClose"), contains("houseLeaveWhy"))

df_onlycat_long <- pivot_longer(data=df_onlycat, cols = colnames(df_onlycat[-1])) %>% 
  mutate(var_name = case_when(
    startsWith(name, "employstatus") ~ "employstatus",
    startsWith(name, "coronaclose") ~ "coronaclose",
    startsWith(name, "houseleavewhy") ~ "houseleavewhy"))

# sanity check, indeed only the needed variables are kept
unique(df_onlycat_long$var_name)

# should only count as an NA if a participant did not pick any of the options in a given category group (across 
# employstatus for example)
df_onlycat_NAs <- df_onlycat_long %>% group_by(x1, var_name) %>% 
  summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
  group_by(var_name) %>% 
  summarise(percentage_NAs= 100*sum(responses_per_cat==0)/nrow(df))

# houseLeaveWhy accounts for by far the largest amount of NAs amongs the 3 cat vars
df_onlycat_NAs

# getting NAs for non categorical vars
df_non_cat <- df %>% 
  select(-contains("employstatus"), -contains("coronaClose"), -contains("houseLeaveWhy"))

df_non_cat_NAs <- tibble(var_name = colnames(df_non_cat), 
                         percentage_NAs = 100*colSums(is.na(df_non_cat))/nrow(df_non_cat))

# combining the non_cat and cat NA data
df_NAs <- bind_rows(df_non_cat_NAs, df_onlycat_NAs)

# histogram of NAs per variable 
ggplot(df_NAs, aes(x=percentage_NAs)) +
  geom_histogram(bins=100)

# we see that there are some variables that quite high percentage of NAs 
ggplot(df_NAs, aes(x=var_name, y=percentage_NAs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plotting variables with missing percentage over 10% (remove them?)
df_NAs %>% filter(percentage_NAs > 10) %>% 
ggplot(aes(x=var_name, y=percentage_NAs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Exploring NAs per participant ###

# for categorical vars
df_onlycat_NAs_participants <- df_onlycat_long %>% group_by(x1, var_name) %>% 
  summarise(responses_per_cat = n() -sum(is.na(value))) %>% 
  group_by(x1) %>% 
  summarise(percentage_NAs_cat= 100*sum(responses_per_cat==0)/ncol(df))

# for non categorical vars
df_non_cat_NAs_participants <- tibble(x1 = df_non_cat$x1, 
                                      percentage_NAs_non_cat = 100*rowSums(is.na(df_non_cat))/ncol(df_non_cat))

# combining the categorical and non-categorical NAs percentages
NAs_per_participant_df <- left_join(df_non_cat_NAs_participants, df_onlycat_NAs_participants, by = "x1") %>% 
  transmute(x1 = x1, total_NAs_perc = percentage_NAs_non_cat+percentage_NAs_cat)

# histogram of NAs percentages on participant level
ggplot(NAs_per_participant_df,aes(total_NAs_perc)) + geom_histogram(bins=100)

# 1,526 participants with percentage of missing responses above 10%
NAs_per_participant_df %>% dplyr::filter(total_NAs_perc>10) %>% nrow()
100*(1526 / nrow(NAs_per_participant_df)) # that is only 2.7% of participants







