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

#source("scripts/psyCorona_data_cleaning.R") # get (subsets of) raw data from exploration script
df_analyse <- read.csv("training.csv", stringsAsFactors = FALSE)
# df_analyse <- df_analyse[sample(1:nrow(df_analyse), 1e3), ] # random subset for testing

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

vars <- c("countryiso3", "date", "affanx", 
          "affbor", "affcalm", "affcontent", "affdepr", "affenerg", "affexc", 
          "affnerv", "affexh", "affinsp", "affrel", "plrac19", "plraeco", 
          "isofriends_inperson", "isoothppl_inperson", "isoimmi_inperson", 
          "isofriends_online", "isoothppl_online", "isoimmi_online", "persrelsat", 
          "discpers", "happy", "lifesat", "mlq", "tightnorms", "tightloose", 
          "tighttreat", "c19know", "c19hope", "c19eff", "c19normshould", 
          "c19normdo", "c19isstrict", "c19ispunish", "c19isorg", "extc19msg", 
          "csqc19contract", "csqecosuffer", "csqcancpln", "csqlife", "ecoknow", 
          "ecohope", "ecoeff", "houseleave", "houseleavewhy_1", "houseleavewhy_2", 
          "houseleavewhy_4", "houseleavewhy_7", "houseleavewhy_6", "tempfocpast", 
          "tempfocpres", "tempfocfut", "feelingtherm", "idoverlap", "relyesno", 
          "countrycitizen", "citizen", "immigrant", "gender", "age", "edu", 
          "source", "coronaclose", 
          "employstatus", "disc", "jbinsec", "pfs", "fail", "lone", "probsolving", 
          "posrefocus", "c19proso", "c19perbeh", "c19rca", "ecoproso", 
          "ecorca", "migrantthreat", "neuro", "para", "consp", 
          "population", "cancelpublicevents_flag", "closepublictransport_flag", 
          "contacttracing", "containmenthealthindex", "containmenthealthindexfordisplay", 
          "debtcontractrelief", "economicsupportindex", "economicsupportindexfordisplay", 
          "emergencyinvestmentinhealthcare", "fiscalmeasures", "governmentresponseindex", 
          "governmentresponseindexfordisplay", "internationalsupport", 
          "internationaltravelcontrols", "investmentinvaccines", "publicinformationcampaigns_flag", 
          "restrictionsongatherings_flag", "restrictionsoninternalmovement_flag", 
          "schoolclosing_flag", "stayathomerequirements_flag", "stringencyindex", 
          "stringencylegacyindex", "stringencylegacyindexfordisplay", "testingpolicy", 
          "workplaceclosing_flag", "ghsscore", "airdepartures", "tourismexpenditures", 
          "doctors_per_10k", "nurses_and_midwifery_per_10k", "che_perc_of_gdp_2017", 
          "controlcorruption", "ruleoflaw", "politicalstability", "voiceaccountability", 
          "govteffectiveness", "regulatoryquality")
df_analyse$startdate <- as.POSIXct(df_analyse$startdate)
df_analyse$date <- as.integer(as.numeric(df_analyse$startdate - as.POSIXct("2020-01-01 00:00:00")))

df_analyse <- df_analyse[, vars]
# Impute NAs with missranger - this doesn't work now
# get number of non NAs per row to use as case.weights
# fitting missRanger
set.seed(953007)
imp <- missRanger(df_analyse)
write.csv(imp, "df_analyse_imputed.csv", row.names = FALSE)
# # quick check to see if any NAs are left over
if(anyNA(imp)) stop("Still NAs in df_analyse.")
df_analyse <- imp

########## ALLOCATING VARIABLES ##########

# Remove variables that we don't use as predictors or dependent variables
df_analyse <- df_analyse[,-which(names(df_analyse) %in% c("responseid", "recordeddate", "train", 
                                                          "source", "region"))]

df_analyse <- df_analyse[, -which(names(df_analyse) %in% as.character(desc$name[desc$unique< 2]))]

#desc <- descriptives(df_analyse)
#sapply(df_analyse[, which(names(df_analyse) %in% as.character(desc$name[desc$unique< 3]))], table)


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
ntree    <- 1000                                                           # number of trees in each RF model

# One function that trains both models (Lasso & Random Forest) on df_analyse and dep. variable (y)
modelsPsycorona <- function(y, data, run_in_parallel = TRUE, n_cores){
  browser()
  X <- data[, !names(data) == y]
  Y <- data[[y]]
  
  if (run_in_parallel) {
    cl <- makePSOCKcluster(n_cores) # set number of cores
    # cl <- parallel::makeCluster(n_cores, setup_strategy = "sequential") # workaround for MacOS issues
    registerDoParallel(cl) # register cluster
  }
  
  modelRes <- list() # object to store results in
  seed     <- 953007 # seed used throughout this function
  
  ### Model 1/2: Lasso
  lassoRes <- list() # object to store Lasso results in
  
  set.seed(seed)
  X_las <- model.matrix(~., X)[, -1]
  lassoRes[["lassoModel"]] <- caret::train(x = X_las, y = Y,
                                           method = "glmnet",
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
  mlr::makeRegrTask()
  tuneRanger::tuneRanger()
  tmp <- ranger(paste0(dep_vars[1], "~", paste0(names(df_analyse[!names(df_analyse) %in% dep_vars]), collapse = "+")), data = df_analyse, importance = "permutation")
  tmp$r.squared
  
  VarImpPlot(tmp)
  
  rfRes[["rfModel"]] <- caret::train(formula(paste(y, "~.", sep = "")), data = df_analyse, method = "ranger", ntree = ntree,
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
models <- lapply(dep_vars[1], modelsPsycorona, df_analyse, run_in_parallel = TRUE, n_cores = 44) # using 48 cores
# calculate elapsed time
proc.time() - ptm

########## EVALUATION ##########

# Looking at the models (here one example for the first in dep_vars)
models[[1]]$lasso$coefPlotRelevant
models[[1]]$rf$varImpPlotTop25
models[[1]]$modelComparison
