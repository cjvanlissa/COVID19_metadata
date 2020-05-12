rm(list = ls())

library(tidyverse)

########## READ DATA ##########
df_raw     <- read_csv("~/Documents/Projects/Data_vs_Corona/psycorona_data/psycorona_data.csv") # raw data
df_baseline <- df_raw[,which(is.na(str_extract(colnames(df_raw), "w1_|w2_|w3_|w4_")))]          # only baseline resp.

# Parameters
# TODO: add parameters on which participants / countries to exclude based on NAs or number of observations

########## ALLOCATING VARIABLES ##########

# Outcome variables (self-containment, social distancing, and attitudes toward government policies)
handwashing  <- "c19perBeh01" # wash hands more often to lower chances of getting corona [7-point Likert; -3:3]
avoid_crowds <- "c19perBeh02" # avoid crowds to lower chances of getting corona [7-point Likert; -3:3]
self_quarant <- "c19perBeh03" # self-quarantine to lower chances of getting corona [7-point Likert; -3:3]

# TODO: other outcome variables in the data?
# TODO: list predictors

########## PREPARE DATA ##########

# TODO: clean data according to parameters above; only exclude potential predictors and outcome variables
# TODO: make sure that all predictors are in the right format: factors for categorical variables, otherwise doubles
# TODO: make test / train / validation sets using random set.seed(953007)
# TODO: make different versions of those sets to fit the various packages: data frames, matrices...

########## MODELLING ##########

# TODO: add models: Lasso, Elastic Net, RandomForest, KNN?

########## EVALUATION ##########

# TODO: add different evaluation metrics, evaluate each model on each metric
# TODO: add plots to visualize variable importance (LARS plot Lasso?)
