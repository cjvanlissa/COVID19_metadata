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

library(tidyverse)

source("scripts/psyCorona_exploration.R") # get (subsets of) raw data from exploration script

# Parameters
partic_NA_tres_perc     <- 10 # remove participants that have more than X% NAs
partic_NA_tres_var      <- 15 # remove features/variables that have more than X% NAs
country_remove_treshold <- .5 # remove countries that account for less than X% of all rows
p_train                 <- .7 # percentage of data in training set
p_test                  <- .2 # percentage of data in test set
p_validation            <- .1 # percentage of data in validation set

########## ALLOCATING VARIABLES ##########

# Outcome variables (self-containment, social distancing, and attitudes toward government policies)
handwashing  <- "c19perBeh01" # wash hands more often to lower chances of getting corona [7-point Likert; -3:3]
avoid_crowds <- "c19perBeh02" # avoid crowds to lower chances of getting corona [7-point Likert; -3:3]
self_quarant <- "c19perBeh03" # self-quarantine to lower chances of getting corona [7-point Likert; -3:3]
houseleave   <- "houseleave"  # how often participant left house in the past week
# tk make this nicer, add coronaclose etc.

########## PREPARE DATA ##########

# Remove participants that have more than X% NAs
partic_above_missing_tres <- NAs_per_participant_df %>% dplyr::filter(total_NAs_perc > partic_NA_tres_perc) 
partic_above_missing_tres # these participants will be excluded
df_clean_partic <- df_analyse %>% dplyr::filter(!(x1 %in% partic_above_missing_tres$x1)) # exclude participants

# Remove features/variables that have more than X% NAs
vars_above_missing_tres <- df_NAs %>% filter(percentage_NAs > partic_NA_tres_var) 
vars_above_missing_tres # these variables will be excluded
df_clean_vars <- df_clean_partic[ ,!(colnames(df_clean_partic) %in% # exclude NA variables
                                       c(vars_above_missing_tres$var_name,
                                         "houseLeaveWhy_1",
                                         "houseLeaveWhy_2",
                                         "houseLeaveWhy_4",
                                         "houseLeaveWhy_6",
                                         "houseLeaveWhy_7"))]

# Removing variables JBINSEC and GODYESNO because they have a "-99" response which is unclear
df_cleaned <- df_cleaned_countries %>% select(-contains("jbinsec"), -godyesno)

# Remove countries that account for less than X% of all rows
countr_incl <- country_percentages %>% filter(perc_of_resp > country_remove_treshold) %>% .$coded_country
df_cleaned_countries <- df_clean_vars %>% filter(coded_country %in% countr_incl)

# Factorizing all categorical variables
df_cleaned <- df_cleaned_countries %>% 
  mutate_if(is.character, as.factor) %>% # this includes e.g. coded_country
  mutate_at(.vars = vars(gender, edu, age, immigrant, citizen, countrycitizen, relyesno, crt1, crt2, crt3), as.factor)

# one hot encode with houseleavewhy, coronaclose and employstatus, (add houseleave here potentially)
vars_to_encode     <- dplyr::select(df_cleaned, contains("employstatus"), contains("coronaclose")) %>% colnames()
df_cleaned_encoded <- df_cleaned %>% 
  mutate_at(vars_to_encode, funs(ifelse(is.na(.), 0,1))) %>%
  mutate_at(vars_to_encode[0:10], funs(factor(.)))

# remove participants who did not answer any coronaclose options. This is will be the DV, so we don't want NAs here
df_cleaned_encoded <- df_cleaned_encoded %>% filter_at(vars_to_encode[12:17], any_vars(. != 0))

# sanity check, checking if the NAs were replaced to 0 for correct variables/participants 
temp1 <- df_cleaned[1:10, ]
temp2 <- df_cleaned_encoded[1:10, ]

# sanity check, check that every participant picked at least one option on coronaclose measure
df_cleaned_encoded %>% filter(coronaclose_1 == 0 & coronaclose_2 == 0 & coronaclose_3 == 0 & coronaclose_4 == 0 & 
                                coronaclose_5 == 0 & coronaclose_6 == 0) %>% nrow()


# remove participants who answered they dont know anyone with corona but also that they do (unreliable)
df_cleaned_reliable <- df_cleaned_encoded %>% 
  filter(!(coronaclose_6==1 & (coronaclose_1==1|coronaclose_2==1|coronaclose_3==1|coronaclose_4==1|coronaclose_5==1)))

# check that indeed every response coronaclose_6 (dont know anyone with covid) means that there is not a response in 
# one of the rest of coronaclose_xx. 
df_cleaned_reliable %>% filter((coronaclose_6==0 & (coronaclose_1==1|coronaclose_2==1|coronaclose_3==1|
                                                      coronaclose_4==1|coronaclose_5==1))) %>% nrow()

df_cleaned_reliable %>% filter((coronaclose_6==1 & (coronaclose_1==0|coronaclose_2==0|coronaclose_3==0|
                                                      coronaclose_4==0|coronaclose_5==0))) %>% nrow()
# check
(12818 + 25532) == nrow(df_cleaned_reliable)

# plotting coronaclose after cleaning and only keeping reliable responses
temp4 <- df_cleaned_reliable %>% dplyr::select(contains("coronaclose")) %>% 
  pivot_longer(cols=c("coronaclose_6", "coronaclose_1", "coronaclose_2", "coronaclose_3",
                      "coronaclose_4", "coronaclose_5")) 

# distribution of coronaclose after cleaning and only keeping reliable responses
temp4 %>% 
  group_by(name) %>% 
  summarise(status_count = sum(as.numeric(value), na.rm=T)) %>% 
  ggplot(aes(x=reorder(name, -status_count), y=status_count)) +
  geom_bar(stat = "identity")

### Adding outcome variables and getting data ready for modelling

# OUTCOME 1: Predicting whether people know anyone who has COVID (including themselves)

# I chose to set a "coronaKnow" variable which is 1 if the participants knows anyone with corona (including themselves)
# and is 0 if they do not. It's arguable if this is best, but gives us the most positive cases to work with.
df_mod <- df_cleaned_reliable %>% mutate(knowcorona = as.factor(ifelse(coronaclose_6 == 1, 0, 1)))

# create final data set for modelling and assign train, test, or validation set
set.seed(953007)
df_mod_final <- df_mod %>% 
  # dplyr::select(-coronaclose_1,-coronaclose_2,  -coronaclose_3,  -coronaclose_4,  -coronaclose_5, -coronaclose_6) %>% 
  mutate_at(vars_to_encode[12:17], funs(factor(.))) %>% 
  mutate(set = sample.int(3, nrow(df_mod), replace = TRUE, prob = c(p_train, p_test, p_validation)))

df_mod_final$coronaclose_1 # check

# put startDate into bins. The range is 5 weeks, so I will use 5 bins
df_mod_final$startdate <- cut(df_mod_final$startdate, breaks = 5, 
                               labels = c("week_1", "week_2", "week_3", "week_4", "week_5"))

# Splitting the data into train, test, and validation set
df_train <- df_mod_final %>% filter(set == 1) %>% select(-set)
df_test  <- df_mod_final %>% filter(set == 2) %>% select(-set)
df_val   <- df_mod_final %>% filter(set == 3) %>% select(-set)

# Making model matrices of training set for each outcome variable (needed for e.g. Lasso)
m_train <- model.matrix(knowcorona ~ .-startdate, data = select(df_train, -x1))

# Extract outcome variables of
y_train = as.factor(df_train$knowcorona)

########## MODELLING ##########

# Outcome 1: knowcorona
knowcorona_mod_lasso <- cv.glmnet(x = m_train, y = y_train, family = "binomial", type.measure = 'auc', alpha = 1, 
                                  nfolds = 5)

########## EVALUATION ##########

### Outcome 1: knowcorona

# Plots
plot(knowcorona_mod_lasso)

# ...
mean(predict(knowcorona_mod_lasso, newx = train_matrix_x, type="class", 
             s = knowcorona_mod_lasso$lambda.1se) == y_train) # train set

lasso_preds <- predict(knowcorona_mod_lasso, newx=train_matrix_x, type="class",
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
