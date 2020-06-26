########## TO DO's ##########

# TODO: update evaluation, try different eval metrics
# TODO: come up with good solution on deciding which predictors are most important based on Lasso and RF

########## PREPARATION ##########

# library(tidyverse)
library(metaforest)
library(mlbench)
library(ranger)
library(Matrix)
library(caret)
library(worcs)
library(doParallel)
library(missRanger)
  


source("scripts/psyCorona_data_cleaning.R") # get (subsets of) raw data from exploration script
df_analyse <- read.csv("training.csv", stringsAsFactors = FALSE)

# To efficiently test whether the code works for all dep. variables, we here take a random subset of the data
# (CHANGE THIS TO THE FULL df_analyse SET LATER)
df_analyse <- df_analyse[sample(1:nrow(df_analyse), 1000), ]


########## PREPARE DATA ##########
# 
# # Removing NA participants & vars
# partic_NA_tres_perc     <- .2 # remove participants that have more than X% NAs
# partic_NA_tres_var      <- .2 # remove features/variables that have more than X% NAs
# 
# miss       <- is.na(df_analyse)
# df_analyse <- df_analyse[!(rowSums(miss)/ncol(df_analyse)) > partic_NA_tres_perc,
#                           which((colSums(miss)/nrow(df_analyse)) <= partic_NA_tres_var)]
# 
# desc <- descriptives(df_analyse)
# 
#  # Median/mode substitution until we implement a better single imputation method
# df_analyse[, desc$type == "integer"] <- lapply(desc$name[desc$type == "integer"], function(varname){
#    out <- df_analyse[[varname]]
#    out[is.na(out)] <- desc$mode[desc$name == varname]
#    out
# })
# 
# df_analyse[, desc$type == "numeric"] <- lapply(desc$name[desc$type == "numeric"], function(varname){
#    out <- df_analyse[[varname]]
#    out[is.na(out)] <- desc$median[desc$name == varname]
#    out
# })


# Impute NAs with missranger - this doesn't work now
# get number of non NAs per row to use as case.weights
# fitting missRanger
set.seed(953007)
df_analyse <- missRanger(df_analyse)
# # quick check to see if any NAs are left over
if(anyNA(df_analyse)) stop("Still NAs in df_analyse.")

# Clearing environment
env_obj <- ls()
env_obj <- env_obj[!env_obj == "df_analyse"]
do.call(rm, as.list(env_obj))
gc()


########## ALLOCATING VARIABLES ##########

# Remove variables that we don't use as predictors or dependent variables
df_analyse <- df_analyse[,-which(names(df_analyse) %in% c("responseid", "recordeddate", "train", 
                                                          "source", "region"))]

# Making all character variables factors
df_analyse[, which(sapply(df_analyse, is.character))] <- 
  sapply(df_analyse[, which(sapply(df_analyse, is.character))], factor)

# Making all variables with 5 or less unique values factors
df_analyse[, which(lapply(sapply(df_analyse, unique), length) <= 5)] <- 
  lapply(df_analyse[, which(lapply(sapply(df_analyse, unique), length) <= 5)], factor)

# Structure overview
str(df_analyse)

# Defining dependent variables
dep_vars <- c("c19eff", "ecorca", "c19proso", "ecoproso", "isofriends_inperson", "c19perbeh")

# Exploring dependent variables (data frame x, dep. variable y)
expl_dv <- function(y){
  res <- list()
  
  res[["type"]]      <- typeof(df_analyse[, which(names(df_analyse) == y)])
  res[["unique"]]    <- unique(df_analyse[, which(names(df_analyse) == y)])
  # res[["histogram"]] <- hist(df_analyse$c19eff)
  
  return(res)
}
lapply(dep_vars, expl_dv) 

########## CARET MODEL COMPARING SETUP ########## 
set.seed(953007)

# Model parameters
cv_split <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE) # cv split
lambda   <- seq(0.001, 0.1, by = 0.001)                                  # lambda sequence for Lasso
ntree    <- 50                                                           # number of trees in each RF model

# One function that trains both models (Lasso & Random Forest) on df_analyse and dep. variable (y)
modelsPsycorona <- function(y,  run_in_parallel = TRUE, n_cores){
  
  if (run_in_parallel) {
    cl <- makePSOCKcluster(n_cores) # set number of cores
    registerDoParallel(cl) # register cluster
  }
  
  modelRes <- list() # object to store results in
  seed     <- 953007 # seed used throughout this function
  
  ### Model 1/2: Lasso
  lassoRes <- list() # object to store Lasso results in
  
  set.seed(seed)
  lassoRes[["lassoModel"]] <- caret::train(formula(paste(y, "~.", sep = "")), data = df_analyse, method = "glmnet",
                                           trControl = cv_split, family = "gaussian",
                                           tuneGrid = expand.grid(alpha = 1, lambda = lambda), verbose = TRUE)
  
  lassoRes[["optLambda"]]  <- lassoRes$lassoModel$bestTune$lambda # optimal lambda
  
  # Lasso coefficients
  lasso_coef_names    <- rownames(coef(lassoRes$lassoModel$finalModel, lassoRes$lassoModel$bestTune$lambda))[-1]
  lasso_coef_values   <- abs(as.vector(coef(lassoRes$lassoModel$finalModel, lassoRes$lassoModel$bestTune$lambda))[-1])
  lassoRes[["coefs"]] <- data.frame(coef_names = lasso_coef_names[order(abs(lasso_coef_values), decreasing=TRUE)],
                                    coef_values = lasso_coef_values[order(abs(lasso_coef_values), decreasing=TRUE)])
  
  lassoRes[["exclPreds"]] <- lassoRes$coefs[which(lassoRes$coefs[,2] == 0), 1]   # these features have been excluded
  lassoRes[["selPreds"]]  <- lassoRes$coefs[-(which(lassoRes$coefs[,2] == 0)), ] # these features have been retained
  
  # Coef plot
  lassoRes[["coefPlot"]] <- 
    ggplot(lassoRes$coefs, aes(x = 0, xend = coef_values, y = reorder(coef_names, coef_values), yend = coef_names)) +
    geom_segment() +
    geom_point(aes(x = coef_values, y = coef_names)) +
    xlab("Coefficient") +
    ylab("") +
    theme_bw()
  
  # Coef plot with only selected (meaningful) variables
  lassoRes[["coefPlotRelevant"]] <- 
    ggplot(lassoRes$coefs[which(lassoRes$coefs$coef_names %in% lassoRes$selPreds$coef_names),], 
           aes(x = 0, xend = coef_values, y = reorder(coef_names, coef_values), yend = coef_names)) +
    geom_segment() +
    geom_point(aes(x = coef_values, y = coef_names)) +
    xlab("Coefficient") +
    ylab("") +
    theme_bw()
  
  modelRes[["lasso"]] <- lassoRes
  
  ### Model 2/2: Random Forest
  rfRes <- list()                        # list to store Random Forest results in
  mtry  <- round(sqrt(ncol(df_analyse))) # number of variables to try in each tree

  set.seed(seed)
  rfRes[["rfModel"]] <- caret::train(formula(paste(y, "~.", sep = "")), data = df_analyse, method = "rf", ntree = ntree,
                                     tuneGrid = data.frame(mtry = mtry), trControl = cv_split, verbose = T)
  
  caret_rf_imp      <- varImp(rfRes$rfModel, scale = FALSE)
  rf_varImp_names   <- rownames(caret_rf_imp$importance)       # coef names
  rf_varImp_values  <- unname(unlist(caret_rf_imp$importance)) # coef values 
  rfRes[["varImp"]] <- data.frame(coef_names = rf_varImp_names[order(abs(rf_varImp_values), decreasing=TRUE)],
                                  coef_values = rf_varImp_values[order(abs(rf_varImp_values), decreasing=TRUE)])
  
  # Coef plot
  rfRes[["varImpPlot"]] <- 
    ggplot(rfRes$varImp, aes(x = 0, xend = coef_values, y = reorder(coef_names, coef_values), yend = coef_names)) + 
    geom_segment() +
    geom_point(aes(x = coef_values, y = coef_names)) +
    xlab("Variable Importance") +
    ylab("") +
    theme_bw()

  # Coef plot top 25
  rfRes[["varImpPlotTop25"]] <- 
    ggplot(rfRes$varImp[1:25,], aes(x = 0, xend = coef_values, y = reorder(coef_names,coef_values), yend=coef_names)) + 
    geom_segment() +
    geom_point(aes(x = coef_values, y = coef_names)) +
    xlab("Variable Importance") +
    ylab("") +
    theme_bw()
  
  modelRes[["rf"]] <- rfRes
  
  # if running in parallel, stop the cluster now
  if (run_in_parallel) {
    stopCluster(cl) # stop cluster
    registerDoSEQ() # get back to sequential 
  }
  
  # Error checks regarding models
  # (1) compare the indices for one of the folds between each models to check if the folds were indeed the same
  if(any(modelRes$lasso$lassoModel$control$index$Fold05 != modelRes$rf$rfModel$control$index$Fold05)) {
    break ("Lasso and RF cv folds not equal")
  }
  
  ### Model comparison
  comparison_results <- resamples(list(Lasso = lassoRes$lassoModel, RandomForest = rfRes$rfModel)) # collect resamples
  modelRes[["modelComparison"]] <- summary(comparison_results) # get the summary of the comparison
  
  return(modelRes)
  
} 


# start counting time, to check if parallelisation is working
ptm <- proc.time()
# Apply modelsPsycorona to all dependent variables
models <- lapply(dep_vars, modelsPsycorona, run_in_parallel=TRUE, n_cores = 3)
# calculate elapsed time
proc.time() - ptm

########## EVALUATION ##########

# Looking at the models (here one example for the first in dep_vars)
models[[1]]$lasso$coefPlotRelevant
models[[1]]$rf$varImpPlotTop25
models[[1]]$modelComparison

### FROM HERE THE SCRIPT HAS TO BE UPDATED

# ### Outcome 1: c19perbeh
# 
# # Exploring Lasso model
# plot(m_lasso_c19perbeh)      # choosing the optimal Lambda
# m_lasso_c19perbeh$lambda.min # chosen lambda
# 
# # Evaluating model on training set (cross-validation)
# m_lasso_c19perbeh$cvm[which(m_lasso_c19perbeh$lambda == m_lasso_c19perbeh$lambda.min)] # mean squared error on train set
# 
# # Get the coefficients and make them into a data frame
# lasso_coefs        <- coef.glmnet(m_lasso_c19perbeh, s = m_lasso_c19perbeh$lambda.min)
# lasso_coefs_excl   <- lasso_coefs[which(lasso_coefs[,1] == 0), 1]    # these features have been excluded
# lasso_coefs_select <- lasso_coefs[-(which(lasso_coefs[,1] == 0)), 1] # these features have been selected (= meaningful)
# sort(abs(lasso_coefs_select), decreasing = TRUE) # coefs sorted according to importance
# 
# # Coef plot
# coefs <- lasso_coefs_select[!names(lasso_coefs_select) == "(Intercept)"]
# plotdat <- data.frame(var = ordered(names(coefs), levels = names(coefs)[order(abs(coefs))]),
#            val = abs(coefs),
#            rank = seq_along(coefs))
# 
# ggplot(plotdat,
#        aes(x = 0, xend = val, y = var, yend = var)) + geom_segment() + geom_point(aes(x = val, y = var))  +theme_bw()
# 
# source("scripts/model_accuracy.R")
# model_accuracy(m_lasso_c19perbeh, 
#                observed = y_train_c19perbeh,
#                olddata = x_train_c19perbeh)
# 
# 
# res <- ranger(y = y_train_c19perbeh, x = x_train_c19perbeh, importance = "permutation")
# VarImpPlot(res)
# 
# 
# # CJ: Don't run this code yet, BUT it fails because different variables are selected in train and test. 
# # We should make ALL variable selection decisions in the exploration script, not in the modeling script.
# # df_testing <- read.csv("testing.csv", stringsAsFactors = FALSE)
# # ndat <- sparse.model.matrix(c19perbeh ~ ., data = df_testing)
# # all(colnames(ndat) %in% colnames(x_train_c19perbeh))
# # colnames(ndat)[!colnames(ndat) %in% colnames(x_train_c19perbeh)]
# # ndat <- ndat[, match(colnames(x_train_c19perbeh), colnames(ndat))]
# 
# model_accuracy(m_lasso_c19perbeh, 
#                newdata = ndat,
#                observed = y_train_c19perbeh,
#                olddata = x_train_c19perbeh)
# # coefs_df <- data.frame(var_name = names(lasso_coefs[,1]), coef = unname(lasso_coefs[,1]))
# # 
# # # plot coefficients above .3
# # coefs_df %>% filter(abs(coef) > 0.3) %>% 
# #   ggplot(aes(x=var_name, y=coef)) +
# #   geom_bar(stat="identity") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# ### ARCHIVED CODE
# 
# # # I chose to set a "coronaKnow" variable which is 1 if the participants knows anyone with corona (including oneself)
# # # and is 0 if they do not. It's arguable if this is best, but gives us the most positive cases to work with.
# # df_mod_final <- df_analyse %>% mutate(knowcorona = factor(coronaclose == 6, labels = c("Yes", "No")))
# 
# # create final data set for modelling and assign train, test, or validation set
# # CJ: We're already working with the training data.
# # set.seed(953007)
# # df_mod_final <- df_mod %>% 
# #   plyr::select(-coronaclose_1,-coronaclose_2,  -coronaclose_3,  -coronaclose_4,  -coronaclose_5, -coronaclose_6) %>% 
# #   mutate_at(vars_to_encode[12:17], funs(factor(.))) %>% 
# #   mutate(set = sample.int(3, nrow(df_mod), replace = TRUE, prob = c(p_train, p_test, p_validation)))
# 
# # # ...
# # mean(predict(knowcorona_mod_lasso, newx = m_train, type="class", 
# #              s = knowcorona_mod_lasso$lambda.1se) == y_train) # train set
# # 
# # lasso_preds <- predict(knowcorona_mod_lasso, newx=m_train, type="class",
# #                        s=knowcorona_mod_lasso$lambda.1se)
# # 
# # lasso_preds_processed <- as.factor(unname(lasso_preds[,1]))
# # 
# # # Accuracy, precision, recall, F1...
# # precision <- posPredValue(lasso_preds_processed, y_train, positive="1")
# # precision
# # recall <- sensitivity(lasso_preds_processed, y_train, positive="1")
# # recall
# # F1 <- (2 * precision * recall) / (precision + recall)
# # F1
