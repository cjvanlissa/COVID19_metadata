########## TO DO's ##########

# TODO: list all outcome variables that we may want to use; organise script (modelling, evaluation) accordingly
# TODO: clean datafile for multiple entries for a single participant (see exploration script TK SHOULD BE CLEANED)
# TODO: make different versions of those sets to fit the various packages: data frames, matrices...
# TODO: add models: Lasso, Elastic Net, RandomForest, KNN?
# TODO: add different evaluation metrics, evaluate each model on each metric
# TODO: add plots to visualize variable importance (LARS plot Lasso?)

########## PREPARATION ##########

# library(tidyverse)
library(Matrix)
  
#source("scripts/psyCorona_exploration.R") # get (subsets of) raw data from exploration script
df_analyse <- read.csv("training.csv", stringsAsFactors = FALSE)

########## PREPARE DATA ##########

# Removing NA participants & vars
partic_NA_tres_perc     <- .2 # remove participants that have more than X% NAs
partic_NA_tres_var      <- .2 # remove features/variables that have more than X% NAs

miss       <- is.na(df_analyse)
df_analyse <- df_analyse[!(rowSums(miss)/ncol(df_analyse)) > partic_NA_tres_perc,
                         -which((colSums(miss)/nrow(df_analyse)) > partic_NA_tres_var)]

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
if(anyNA(df_analyse)) stop("Still NAs in df_analyse.")


# Clearing environment
env_obj <- ls()
env_obj <- env_obj[!env_obj == "df_analyse"]
do.call(rm, as.list(env_obj))
gc()

########## ALLOCATING VARIABLES ##########

# Remove variables that we don't use as predictors or outcomes
df_analyse <- df_analyse[,-which(names(df_analyse) %in% c("responseid", "recordeddate", "train", "coronaclose"))]

# Let's predict self-serving behavior as an example / template ("knowcorona" code is down below)
outcome_vars <- c("c19perbeh") # hand washing, avoiding crowds, self-quarantining
preds        <- names(df_analyse)[-(which(names(df_analyse) %in% outcome_vars))]

# Explore outcome variable
hist(df_analyse$c19perbeh)  # people tend to self-serve

########## MODELLING ##########

### Outcome 1: c19perbeh
x_train_c19perbeh <- sparse.model.matrix(c19perbeh ~ ., data = df_analyse) # making model matrix
y_train_c19perbeh <- df_analyse$c19perbeh                                  # extract outcome variable

# Lasso model
m_lasso_c19perbeh <- cv.glmnet(x = x_train_c19perbeh, 
                               y = y_train_c19perbeh, 
                               family = "gaussian", 
                               type.measure = 'mse', 
                               alpha = 1, 
                               nfolds = 10)

########## EVALUATION ##########

### Outcome 1: c19perbeh

# Exploring Lasso model
plot(m_lasso_c19perbeh)      # choosing the optimal Lambda
m_lasso_c19perbeh$lambda.min # chosen lambda

# Evaluating model on training set (cross-validation)
m_lasso_c19perbeh$cvm[which(m_lasso_c19perbeh$lambda == m_lasso_c19perbeh$lambda.min)] # mean squared error on train set

# Get the coefficients and make them into a data frame
lasso_coefs        <- coef.glmnet(m_lasso_c19perbeh, s = m_lasso_c19perbeh$lambda.min)
lasso_coefs_excl   <- lasso_coefs[which(lasso_coefs[,1] == 0), 1]    # these features have been excluded
lasso_coefs_select <- lasso_coefs[-(which(lasso_coefs[,1] == 0)), 1] # these features have been selected (= meaningful)
sort(abs(lasso_coefs_select), decreasing = TRUE) # coefs sorted according to importance

# Coef plot
coefs <- lasso_coefs_select[!names(lasso_coefs_select) == "(Intercept)"]
plotdat <- data.frame(var = ordered(names(coefs), levels = names(coefs)[order(abs(coefs))]),
           val = abs(coefs),
           rank = seq_along(coefs))

ggplot(plotdat,
       aes(x = 0, xend = val, y = var, yend = var)) + geom_segment() + geom_point(aes(x = val, y = var))  +theme_bw()

source("scripts/model_accuracy.R")
model_accuracy(m_lasso_c19perbeh, 
               observed = y_train_c19perbeh,
               olddata = x_train_c19perbeh)



library(ranger)
library(metaforest)

res <- ranger(y = y_train_c19perbeh, x = x_train_c19perbeh, importance = "permutation")
VarImpPlot(res)




# CJ: Don't run this code yet, BUT it fails because different variables are selected in train and test. We should make ALL variable selection decisions in the exploration script, not in the modeling script.
# df_testing <- read.csv("testing.csv", stringsAsFactors = FALSE)
# ndat <- sparse.model.matrix(c19perbeh ~ ., data = df_testing)
# all(colnames(ndat) %in% colnames(x_train_c19perbeh))
# colnames(ndat)[!colnames(ndat) %in% colnames(x_train_c19perbeh)]
# ndat <- ndat[, match(colnames(x_train_c19perbeh), colnames(ndat))]

model_accuracy(m_lasso_c19perbeh, 
               newdata = ndat,
               observed = y_train_c19perbeh,
               olddata = x_train_c19perbeh)
# coefs_df <- data.frame(var_name = names(lasso_coefs[,1]), coef = unname(lasso_coefs[,1]))
# 
# # plot coefficients above .3
# coefs_df %>% filter(abs(coef) > 0.3) %>% 
#   ggplot(aes(x=var_name, y=coef)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

### ARCHIVED CODE

# # I chose to set a "coronaKnow" variable which is 1 if the participants knows anyone with corona (including oneself)
# # and is 0 if they do not. It's arguable if this is best, but gives us the most positive cases to work with.
# df_mod_final <- df_analyse %>% mutate(knowcorona = factor(coronaclose == 6, labels = c("Yes", "No")))

# create final data set for modelling and assign train, test, or validation set
# CJ: We're already working with the training data.
# set.seed(953007)
# df_mod_final <- df_mod %>% 
#   plyr::select(-coronaclose_1,-coronaclose_2,  -coronaclose_3,  -coronaclose_4,  -coronaclose_5, -coronaclose_6) %>% 
#   mutate_at(vars_to_encode[12:17], funs(factor(.))) %>% 
#   mutate(set = sample.int(3, nrow(df_mod), replace = TRUE, prob = c(p_train, p_test, p_validation)))

# # ...
# mean(predict(knowcorona_mod_lasso, newx = m_train, type="class", 
#              s = knowcorona_mod_lasso$lambda.1se) == y_train) # train set
# 
# lasso_preds <- predict(knowcorona_mod_lasso, newx=m_train, type="class",
#                        s=knowcorona_mod_lasso$lambda.1se)
# 
# lasso_preds_processed <- as.factor(unname(lasso_preds[,1]))
# 
# # Accuracy, precision, recall, F1...
# precision <- posPredValue(lasso_preds_processed, y_train, positive="1")
# precision
# recall <- sensitivity(lasso_preds_processed, y_train, positive="1")
# recall
# F1 <- (2 * precision * recall) / (precision + recall)
# F1
