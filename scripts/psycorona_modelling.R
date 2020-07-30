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
source("scripts/varimpplot_lasso.R")
source("scripts/model_accuracy.R")
run_everything <- FALSE

if(run_everything){
  #source("scripts/psyCorona_data_cleaning.R") # get (subsets of) raw data from exploration script
  df_training <- read.csv("training.csv", stringsAsFactors = FALSE)
  df_testing <- read.csv("testing.csv", stringsAsFactors = FALSE)
  representative <- df_testing$source %in% c(6:8)
  yaml::write_yaml(representative, "results/representative.yml")
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
            "population", 
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
            "govteffectiveness", "regulatoryquality")
  df_training$startdate <- as.POSIXct(df_training$startdate)
  df_testing$startdate <- as.POSIXct(df_testing$startdate)
  df_training$date <- as.integer(as.numeric(df_training$startdate - as.POSIXct("2020-01-01 00:00:00")))
  df_testing$date <- as.integer(as.numeric(df_testing$startdate - as.POSIXct("2020-01-01 00:00:00")))
  
  df_training <- df_training[, vars]
  df_testing <- df_testing[, vars]
  
  desc <- descriptives(df_training)
  write.csv(desc, "results/descriptives_df_analyze.csv", row.names = FALSE)
  if(any(desc$unique < 2)){
    drop_vars <- which(names(df_training) %in% as.character(desc$name[desc$unique< 2]))
    df_training <- df_training[, -drop_vars]
    df_testing <- df_testing[, -drop_vars]
  }
  
  desc <- descriptives(df_testing)
  write.csv(desc, "results/descriptives_df_testing.csv", row.names = FALSE)
  
  # Impute NAs with missranger
  set.seed(953007)
  imp <- missRanger(df_training)
  # Remove variables that we don't use as predictors or dependent variables
  imp <- imp[,-which(names(imp) %in% c("responseid", "recordeddate", "train", "source", "region", "publicinformationcampaigns_flag"))]
  write.csv(imp, "df_training_imputed.csv", row.names = FALSE)
  df_training <- imp
  imp <- missRanger(df_testing)
  # Remove variables that we don't use as predictors or dependent variables
  imp <- imp[,-which(names(imp) %in% c("responseid", "recordeddate", "train", "source", "region", "publicinformationcampaigns_flag"))]
  write.csv(imp, "df_testing_imputed.csv", row.names = FALSE)
  df_testing <- imp
  
} else {
  df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
  df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)
}


########## ALLOCATING VARIABLES ##########

# Making all character variables factors
df_training[which(sapply(df_training, is.character))] <- 
  lapply(df_training[which(sapply(df_training, is.character))], factor)

df_testing[which(sapply(df_testing, is.character))] <- 
  lapply(df_testing[which(sapply(df_testing, is.character))], factor)


# Making all variables with 5 or less unique values factors
df_training[which(lapply(sapply(df_training, unique), length) <= 5)] <- 
  lapply(df_training[which(lapply(sapply(df_training, unique), length) <= 5)], factor)
df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)] <- 
  lapply(df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)], factor)
# for(i in 1:118){
#   df_testing[[i]] <- do.call(paste0("as.", class(df_training[[i]])[1]), list(df_testing[[i]]))
# }
names(df_testing)[!(mapply(function(x,y){class(x)[1] == class(y)[1]}, x = df_training, y = df_testing[1:116]))]

# Structure overview
#str(df_training)

# Defining dependent variables
dep_vars <- c("c19eff", "ecorca", "consp", "c19proso", "ecoproso", "isofriends_inperson", "c19perbeh")


########## CARET MODEL COMPARING SETUP ########## 
set.seed(953007)

# Model parameters
#cv_split <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE) # cv split
cv_folds <- sample.int(10, size = nrow(df_training), replace = TRUE)
cv_split <- trainControl(method = "cv", number = 10, index = lapply(1:max(cv_folds), function(i){which(cv_folds == i)}), verboseIter = TRUE, allowParallel = TRUE) # cv split
ntree    <- 1000                                                           # number of trees in each RF model

# One function that trains both models (Lasso & Random Forest) on df_training and dep. variable (y)
model_crosssectional <- function(y, data = df_training, run_in_parallel = TRUE, n_cores){
  X <- df_training[, !names(df_training) == y]
  X_las <- model.matrix(~., X)[, -1]
  Y <- df_training[[y]]
  
  X_test <- df_testing[, !names(df_testing) == y]
  X_las_test <- model.matrix(~., X_test)[, -1]
  Y_test <- df_testing[[y]]
  
  modelRes <- list() # object to store results in

  ### Model 1/2: Lasso
  lassoRes <- list() # object to store Lasso results in
  
  if (run_in_parallel) {
    cl <- makePSOCKcluster(n_cores) # set number of cores
    # cl <- parallel::makeCluster(n_cores, setup_strategy = "sequential") # workaround for MacOS issues
    registerDoParallel(cl) # register cluster
  }
  lassoRes[["lasso_model"]] <- glmnet::cv.glmnet(X_las, Y, parallel = TRUE, foldid = cv_folds)
  # if running in parallel, stop the cluster now
  if (run_in_parallel) {
    stopCluster(cl) # stop cluster
    registerDoSEQ() # get back to sequential 
  }
  lassoRes[["final_model"]] <- glmnet::glmnet(X_las, Y, lambda = lassoRes$lasso_model$lambda.1se)
  
  lassoRes[["optLambda"]]  <- lassoRes$lasso_model$lambda.1se
  
  # Lasso coefficients
  lassoRes[["varimp"]] <- as.matrix(lassoRes[["final_model"]]$beta)
  lassoRes[["varimp"]] <- data.frame(Variable = rownames(lassoRes[["varimp"]]), beta = lassoRes[["varimp"]][,1])
  rownames(lassoRes[["varimp"]]) <- NULL
  lassoRes[["varimp"]] <- lassoRes[["varimp"]][order(abs(lassoRes[["varimp"]]$beta), decreasing = TRUE), ]
  
  lassoRes[["exclPreds"]] <- lassoRes$varimp[which(lassoRes$varimp[,2] == 0), 1]   # these features have been excluded
  lassoRes[["selPreds"]]  <- lassoRes$varimp[-(which(lassoRes$varimp[,2] == 0)), ] # these features have been retained
  
  lassoRes[["fit"]] <- c(lassoRes$lasso_model$lambda.1se,
                         lassoRes$lasso_model$cvm[which(lassoRes$lasso_model$lambda == lassoRes$lasso_model$lambda.1se)],
                         model_accuracy(lassoRes$final_model,
                                               olddata = X_las,
                                               observed = Y,
                                               ymean = mean(Y, na.rm = TRUE), s = lassoRes$lasso_model$lambda.1se),
                         model_accuracy(lassoRes$final_model,
                                               newdata = X_las_test,
                                               observed = Y_test,
                                               ymean = mean(Y, na.rm = TRUE), s = lassoRes$lasso_model$lambda.1se))

  names(lassoRes$fit) <- c("lambda.1se", "mse", paste0("train_", c("r2", "mse", "r_actual_pred")), paste0("test_", c("r2", "mse", "r_actual_pred")))  
  modelRes[["lasso"]] <- lassoRes
  
  ### Model 2/2: Random Forest
  rfRes <- list()                        # list to store Random Forest results in
  mtry  <- round(sqrt(ncol(df_training))) # number of variables to try in each tree

  task <- mlr::makeRegrTask(data = df_training, target = y) # Has a "blocking" argument for grouped CV
  res <- tuneRanger::tuneRanger(task, num.trees = ntree, tune.parameters = c("mtry", "min.node.size"))
  rfRes[["rf_model"]] <- res
  
  res_final <- ranger(paste0(y, "~."), num.trees = ntree,
                      min.node.size = res$recommended.pars$min.node.size,
                      mtry = res$recommended.pars$mtry,
                      data = df_training, importance = "permutation")
  
  rfRes[["final_model"]] <- res_final
  rfRes[["cv_model"]] <- caret::train(formula(paste(y, "~.", sep = "")),
                                      data = df_training,
                                      method = "ranger",
                                      num.trees = ntree,
                                      tuneGrid = data.frame(mtry = res$recommended.pars$mtry,
                                                            splitrule = "variance",
                                                            min.node.size = res$recommended.pars$min.node.size
                                                            ),
                                      trControl = cv_split, verbose = T)
  
  
  rfRes[["varimp"]] <- data.frame(Variable = names(res_final$variable.importance),
                                  importance = res_final$variable.importance)
  rfRes[["varimp"]] <- rfRes[["varimp"]][order(rfRes[["varimp"]]$importance, decreasing = TRUE), ]
  
  rfRes[["fit"]] <- c(unlist(res$recommended.pars[c("mtry", "min.node.size", "mse")]),
                         model_accuracy(rfRes$final_model,
                                        olddata = df_training,
                                        observed = Y,
                                        ymean = mean(Y, na.rm = TRUE)),
                         model_accuracy(rfRes$final_model,
                                        newdata = df_testing,
                                        observed = Y_test,
                                        ymean = mean(Y, na.rm = TRUE)))
  
  names(rfRes$fit) <- c("mtry", "min.node.size", "mse", paste0("train_", c("r2", "mse", "r_actual_pred")), paste0("test_", c("r2", "mse", "r_actual_pred")))  
  
  modelRes[["rf"]] <- rfRes
  

  saveRDS(modelRes, paste0("results/res_", y, ".RData"))
  modelRes$lasso[c("lasso_model", "optLambda", "exclPreds", "selPreds")] <- NULL
  modelRes$rf[c("rf_model")] <- NULL
  return(modelRes)
  
} 


# start counting time, to check if parallelisation is working
ptm <- proc.time()

# Apply modelsPsycorona to all dependent variables
models <- lapply(dep_vars, model_crosssectional, df_training, run_in_parallel = TRUE, n_cores = 44) # using 48 cores
# calculate elapsed time
proc.time() - ptm


# WATCH OUT: CLEARS THE WORKING ENVIRONMENT -------------------------------
rm(ls())
# WATCH OUT: CLEARS THE WORKING ENVIRONMENT -------------------------------

library(ggplot2)
library(metaforest)
source("scripts/varimpplot_lasso.R")
source("scripts/model_accuracy.R")

# Prepare training data
df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
f <- list.files("results", "res.+RData", full.names = TRUE)
df_vars <- read.csv("scripts/df_training_labs.csv", stringsAsFactors = F)
var_rename <- tolower(df_vars$lab)
names(var_rename) <- df_vars$X

# Prepare (representative) testing data
representative <- yaml::read_yaml("results/representative.yml")
df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)
df_testing[which(sapply(df_testing, is.character))] <- 
  lapply(df_testing[which(sapply(df_testing, is.character))], factor)

df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)] <- 
  lapply(df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)], factor)
df_representative <- df_testing[representative, ]

# Prepare empty fits table
write.table(
  t(c("DV", "lasso_lambda.1se", "lasso_mse", "lasso_train_r2", "lasso_train_mse", 
    "lasso_train_r_actual_pred", "lasso_test_r2", "lasso_test_mse", 
    "lasso_test_r_actual_pred", "rf_mtry", "rf_min.node.size", "rf_mse", 
    "rf_train_r2", "rf_train_mse", "rf_train_r_actual_pred", "rf_test_r2", 
    "rf_test_mse", "rf_test_r_actual_pred", "representative_r2", 
    "representative_mse", "representative_r_actual_pred")),
  "results/fit_table.csv",
  sep = ",",
  append = FALSE, row.names = FALSE, col.names = FALSE)

# Renamed plots
for(thisfile in f){
  dvname <- gsub(".+_(.+)\\.RData", "\\1", thisfile)
  tmp <- readRDS(thisfile)
  res <- tmp$rf$final_model
  
  # Lasso plot
  p <- VarImpPlot(tmp$lasso$final_model)
  ggsave(
    filename = paste0("results/lasso_", dvname, ".png"),
    p,
    device = "png")
  
  # Fit table

  # Fits in RData
  names(tmp$lasso$fit) <- paste0("lasso_", names(tmp$lasso$fit))
  names(tmp$rf$fit) <- paste0("rf_", names(tmp$rf$fit))
  
  # Representative fits
  out <- tryCatch({model_accuracy(tmp$rf$final_model,
                                  newdata = df_representative[, -which(names(df_representative) == dvname)],
                                  observed = df_representative[[dvname]],
                                  ymean = mean(df_training[[dvname]], na.rm = TRUE))}, error = function(e){
                                    c(r2 = NA, mse = NA, r_actual_pred = NA)
                                  })
  names(out) <- paste0("representative_", names(out))
  
  
  fits <- c(DV = gsub(".+_(.+)\\.RData", "\\1", thisfile), tmp$lasso$fit, tmp$rf$fit, out)
  
  # Write to file  
  write.table(t(fits), "results/fit_table.csv", append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")

  # cur_env <- ls()
  # cur_env <- cur_env[!cur_env %in% c("res", "df_training", "dvname", "df_representative", "f", "thisfile", "var_rename", "VarImpPlot", "VarImpPlot.numeric")]
  # rm(list = cur_env)
  
  cur_env <- ls()
  cur_env <- cur_env[!cur_env %in% c("res", "df_training", "dvname", "df_representative", "f", "thisfile", "var_rename", "VarImpPlot", "VarImpPlot.numeric")]
  rm(list = cur_env)
  vars <- names(head(res$variable.importance[order(res$variable.importance, decreasing = TRUE)], 30))
  gc()
  
  # Partial dependence plot
  p <- metaforest::PartialDependence(res, vars = vars, data = df_training, label_elements = var_rename, resolution = c(27, 100), save_direction = "directions.csv")
  ggsave(
    filename = paste0("results/rf_partialdependence_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".png"),
    p,
    device = "png")
  svg(paste0("results/rf_partialdependence_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".svg"))
  eval(p)
  dev.off()
  # Variable importance plot
  dirs <- read.csv("directions.csv", header = FALSE, stringsAsFactors = FALSE)
  dirs$V5 <- apply(dirs[, c(2,4)], 1, function(i){
    if(sum(i == 0) == 2){
      "Other"
    } else {
      if(sum(i == 0) == 1){
        c("Negative", "Positive")[which(!i == 0)]
      } else {
        if(any(prop.table(i) > .6)){
          c("Negative", "Positive")[(i[1] < i[2])+1]
        } else {
          "Other"
        }
      }
    }
  })

  VI <- sort(res$variable.importance, decreasing = TRUE)[1:30]
  VI <- data.frame(Variable = var_rename[names(VI)], Importance = VI, Direction = dirs$V5[match(names(VI), dirs$V1)])
  VI$Variable <- ordered(VI$Variable, levels = rev(VI$Variable))
  p <- ggplot(VI, aes_string(y = "Variable", x = "Importance")) + 
    geom_segment(aes_string(x = 0, xend = "Importance", 
                            y = "Variable", yend = "Variable"), colour = "grey50", 
                 linetype = 2) + geom_vline(xintercept = 0, colour = "grey50", 
                                            linetype = 1) + xlab("Permutation Importance") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank()) + 
    geom_point(aes_string(fill = "Direction"), shape = 21, size = 2) + 
      scale_fill_manual(values = c("Positive" = "white", "Negative" = "black", "Other" = "gray70")) + 
      theme(legend.position = c(0.92, 0.13))
  ggsave(
    filename = paste0("results/varimp_rf_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".png"),
    p,
    device = "png")
  svg(paste0("results/varimp_rf_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".svg"))
  eval(p)
  dev.off()
  
  cur_env <- ls()
  cur_env <- cur_env[!cur_env %in% c("res", "df_training", "dvname", "df_representative", "f", "thisfile", "var_rename", "VarImpPlot", "VarImpPlot.numeric")]
  rm(list = cur_env)
  vars <- names(head(res$variable.importance[order(res$variable.importance, decreasing = TRUE)], 30))
  gc()
  for(thisvar in vars){
    cur_env <- ls()
    cur_env <- cur_env[!cur_env %in% c("vars", "thisvar", "dvname", "res", "df_training", "df_representative", "f", "thisfile", "var_rename", "VarImpPlot", "VarImpPlot.numeric")]
    rm(list = cur_env)
    gc()
    
    p <- metaforest::PartialDependence(res, vars = thisvar, data = df_training, label_elements = var_rename, resolution = c(27, 100))
    ggsave(
      filename = paste0("results/pdp_individual/rf_pdp_", thisvar, "_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".png"),
      p,
      device = "png")
  }
}

# representative <- yaml::read_yaml(representative, "results/representative.yml")
# df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)
# df_testing[which(sapply(df_testing, is.character))] <- 
#   lapply(df_testing[which(sapply(df_testing, is.character))], factor)
# 
# df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)] <- 
#   lapply(df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)], factor)
# df_representative <- df_testing[representative, ]
# 
# source("scripts/model_accuracy.R")
# 
# repr_table <- sapply(f, function(thisfile){
#   tmp <- readRDS(thisfile)
#   y <- gsub(".+?_(.+)\\.RData", "\\1", thisfile)
#   out <- tryCatch({model_accuracy(tmp$rf$final_model,
#                    newdata = df_representative[, -which(names(df_representative) == y)],
#                    observed = df_representative[[y]],
#                    ymean = mean(df_training[[y]], na.rm = TRUE))}, error = function(e){
#                      browser()
#                      c(r2 = NA, mse = NA, r_actual_pred = NA)
#                    })
#   names(out) <- paste0("representative_", names(out))
#   out
# })
# 
# write.csv(repr_table, "results/representative_fit.csv")