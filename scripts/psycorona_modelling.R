########## TO DO's ##########

# TODO: PRIORITY: implement missRanger to impute NAs (only ~10 rows without NAs, need complete rows for models)

# TODO: list all outcome variables that we may want to use; organise script (modelling, evaluation) accordingly
# TODO: clean datafile for multiple entries for a single participant (see exploration script TK SHOULD BE CLEANED)
# TODO: clean data according to parameters above; only exclude potential predictors and outcome variables
# TODO: make sure that all predictors are in the right format: factors for categorical variables, otherwise doubles
# TODO: make test / train / validation sets using random set.seed(953007)
# TODO: make different versions of those sets to fit the various packages: data frames, matrices...
# TODO: add models: Lasso, Elastic Net, RandomForest, KNN?
# TODO: add different evaluation metrics, evaluate each model on each metric
# TODO: add plots to visualize variable importance (LARS plot Lasso?)

########## PREPARATION ##########

#library(tidyverse)
  
source("scripts/psyCorona_exploration.R") # get (subsets of) raw data from exploration script

df_analyse <- df_analyse[!is.na(df_analyse$startdate), ]
# Parameters
partic_NA_tres_perc     <- .2 # remove participants that have more than X% NAs
partic_NA_tres_var      <- .2 # remove features/variables that have more than X% NAs
country_remove_treshold <- .5 # remove countries that account for less than X% of all rows # CJ: ALready did this in the exploration script, right?
p_train                 <- .7 # percentage of data in training set # CJ: Training set has already been selected at beginning of the exploration script
p_test                  <- .2 # percentage of data in test set # CJ: Test set has already been selected at beginning of the exploration script
p_validation            <- .1 # percentage of data in validation set # CJ: No longer using a validation set

miss <- is.na(df_analyse)
df_analyse <- df_analyse[!(rowSums(miss)/ncol(df_analyse)) > partic_NA_tres_perc,
                         -which((colSums(miss)/nrow(df_analyse)) > partic_NA_tres_var)]

########## ALLOCATING VARIABLES ##########

# Outcome variables (self-containment, social distancing, and attitudes toward government policies)
handwashing  <- "c19perBeh01" # wash hands more often to lower chances of getting corona [7-point Likert; -3:3]
avoid_crowds <- "c19perBeh02" # avoid crowds to lower chances of getting corona [7-point Likert; -3:3]
self_quarant <- "c19perBeh03" # self-quarantine to lower chances of getting corona [7-point Likert; -3:3]
houseleave   <- "houseleave"  # how often participant left house in the past week
# tk make this nicer, add coronaclose etc.

########## PREPARE DATA ##########

# change the type to integer where appropriate
# first get the list of columns that already contain integer values 
int_columns_list = c()
for (col_n in 1:dim(df_analyse)[2]) {
  
  current_column_values <-  df_analyse[col_n]
  current_column_name <- colnames(df_analyse[col_n])
  
  if (is.numeric(current_column_values[1, ])) {
    if (all((current_column_values%%1 == 0) == TRUE, na.rm=T)) {
      int_columns_list = c(int_columns_list, current_column_name)}
  }
}
# change the type to integer for the appropriate columns 
df_analyse <- df_analyse %>% 
  mutate_at(int_columns_list, as.integer) %>% 
  dplyr::select(-x)

desc <- descriptives(df_analyse)

# Median/mode substitution until we implement a better single imputation method
df_analyse[, desc$type == "integer"] <- lapply(desc$name[desc$type == "integer"], function(varname){
  out <- df_analyse[[varname]]
  out[is.na(out)] <- desc$mode[desc$name == varname]
  out
})

df_analyse[, desc$type == "numeric"] <- lapply(desc$name[desc$type == "numeric"], function(varname){
  out <- df_analyse[[varname]]
  out[is.na(out)] <- desc$median[desc$name == varname]
  out
})


# Impute NAs with missranger - this doesn't work now
# get number of non NAs per row to use as case.weights
#non_miss <- rowSums(!is.na(df_analyse))
# fitting missRanger
# set.seed(3609)
# df_analyse_NA_imputed <- missRanger(df_analyse)
# quick check to see if any NAs are left over
if(any(is.na(df_analyse))) stop("Still NAs in df_analyse.")


########## ADD OUTCOME VARIABLES ##########

# Adding outcome variables and getting data ready for modelling
# OUTCOME 1: Predicting whether people know anyone who has COVID (including themselves)

# I chose to set a "coronaKnow" variable which is 1 if the participants knows anyone with corona (including themselves)
# and is 0 if they do not. It's arguable if this is best, but gives us the most positive cases to work with.
df_mod_final <- df_analyse %>% mutate(knowcorona = factor(coronaclose == 6, labels = c("Yes", "No")))

# create final data set for modelling and assign train, test, or validation set
# CJ: We're already working with the training data.
# set.seed(953007)
# df_mod_final <- df_mod %>% 
#   # dplyr::select(-coronaclose_1,-coronaclose_2,  -coronaclose_3,  -coronaclose_4,  -coronaclose_5, -coronaclose_6) %>% 
#   mutate_at(vars_to_encode[12:17], funs(factor(.))) %>% 
#   mutate(set = sample.int(3, nrow(df_mod), replace = TRUE, prob = c(p_train, p_test, p_validation)))

#df_mod_final$coronaclose_1 # check

# put startDate into bins. The range is 5 weeks, so I will use 5 bins
df_mod_final$startdate <- as.POSIXct(df_mod_final$startdate)
df_mod_final$startdate <- cut(df_mod_final$startdate, breaks = 5, 
                               labels = c("week_1", "week_2", "week_3", "week_4", "week_5"))

# Splitting the data into train, test, and validation set
# df_train <- df_mod_final %>% filter(set == 1) %>% select(-set)
# df_test  <- df_mod_final %>% filter(set == 2) %>% select(-set)
# df_val   <- df_mod_final %>% filter(set == 3) %>% select(-set)

# Making model matrices of training set for each outcome variable (needed for e.g. Lasso)
# CJ: Why not startdate? Shouldn't that be one of the most important predictors?
#m_train <- model.matrix(knowcorona ~ .-startdate, data = select(df_train, -x1))
library(Matrix)
env_obj <- ls()
env_obj <- env_obj[!env_obj == "df_mod_final"]
do.call(rm, as.list(env_obj))
gc()
m_train <- sparse.model.matrix(knowcorona ~ .-train-coronaclose, data = df_mod_final)
# Extract outcome variables of
y_train = df_mod_final$knowcorona

########## MODELLING ##########

# Outcome 1: knowcorona
knowcorona_mod_lasso <- cv.glmnet(x = m_train, y = y_train, family = "binomial", type.measure = 'auc', alpha = 1, nfolds = 5)

########## EVALUATION ##########

### Outcome 1: knowcorona

# Plots
plot(knowcorona_mod_lasso)

# ...
mean(predict(knowcorona_mod_lasso, newx = m_train, type="class", 
             s = knowcorona_mod_lasso$lambda.1se) == y_train) # train set

lasso_preds <- predict(knowcorona_mod_lasso, newx=m_train, type="class",
                       s=knowcorona_mod_lasso$lambda.1se)

lasso_preds_processed <- as.factor(unname(lasso_preds[,1]))

# Accuracy, precision, recall, F1...
precision <- posPredValue(lasso_preds_processed, y_train, positive="1")
precision
recall <- sensitivity(lasso_preds_processed, y_train, positive="1")
recall
F1 <- (2 * precision * recall) / (precision + recall)
F1

# get the coefficients and make them into a tibble
lasso_coefs <- coef.glmnet(knowcorona_mod_lasso, s=knowcorona_mod_lasso$lambda.min)
coefs_df <- tibble(var_name = names(lasso_coefs[,1]), coef = unname(lasso_coefs[,1]))

# plot coefficients above .3
coefs_df %>% filter(abs(coef) > 0.3) %>% 
  ggplot(aes(x=var_name, y=coef)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
