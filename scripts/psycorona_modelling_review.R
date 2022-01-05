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

df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)

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
dep_vars <- c("c19perbeh")

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
  X_las_train <- data.frame(df_training[y], model.matrix(as.formula(paste0(y, "~.")), df_training)[,-1])
  
  X_las_test <- data.frame(df_testing[y], model.matrix(as.formula(paste0(y, "~.")), df_testing)[,-1])
  
  all(names(X_las_test) %in% names(X_las))
  all(names(X_las) %in% names(X_las_test))
  
  representative <- yaml::read_yaml("results/representative.yml")
  df_representative <- df_testing[representative, ]
  X_las_rep <- X_las_test[representative, ]
  

  res_tune <- caret::train(formula(paste(y, "~.", sep = "")),
                                      data = X_las_train,
                                      method = "lm",
                                      trControl = cv_split, verbose = T)
  
  res_tune
  "countryiso3AUS" %in% names(df_testing)
  table(df_testing$countryiso3)
  res_fit <- c(
                         model_accuracy(res_tune$finalModel,
                                        olddata = X_las_train,
                                        observed = X_las_train$c19perbeh,
                                        ymean = mean(X_las_train$c19perbeh, na.rm = TRUE)),
                         model_accuracy(res_tune$finalModel,
                                        newdata = X_las_test,
                                        observed = X_las_test$c19perbeh,
                                        ymean = mean(X_las_train$c19perbeh, na.rm = TRUE)),
                         model_accuracy(res_tune$finalModel,
                                        newdata = X_las_rep,
                                        observed = X_las_rep$c19perbeh,
                                        ymean = mean(X_las_train$c19perbeh, na.rm = TRUE)))
  
  names(res_fit) <- c(paste0("train_", c("r2", "mse", "r_actual_pred")), paste0("test_", c("r2", "mse", "r_actual_pred")))  

  return(res_fit)
  
} 


# start counting time, to check if parallelisation is working
ptm <- proc.time()

# Apply modelsPsycorona to all dependent variables
models <- lapply(dep_vars, model_crosssectional, df_training, run_in_parallel = TRUE, n_cores = 44) # using 48 cores
# calculate elapsed time
proc.time() - ptm


# WATCH OUT: CLEARS THE WORKING ENVIRONMENT -------------------------------
do.call(rm, as.list(ls()))
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


# Renamed plots
for(thisfile in f){
  dvname <- gsub(".+_(.+)\\.RData", "\\1", thisfile)
  tmp <- readRDS(thisfile)
  res <- tmp$rf$final_model
  
  # Lasso plot
  source("scripts/varimpplot_lasso.R")
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
  cur_env <- cur_env[!cur_env %in% c("res", "df_training", "dvname", "df_representative", "f", "thisfile", "var_rename", "VarImpPlot", "VarImpPlot.numeric", "run_everything")]
  rm(list = cur_env)
  vars <- names(head(res$variable.importance[order(res$variable.importance, decreasing = TRUE)], 30))
  gc()
  
  # Partial dependence plot
  if(run_everything){
    p <- PartialDependence(res, vars = vars, data = df_training, output = "list", label_elements = var_rename, resolution = c(27, 100))
    saveRDS(p, paste0("results/pdp_", dvname, ".RData"))
    source("scripts/get_signs.R")
    the_signs <- t(get_signs(p))
    write.csv(the_signs, paste0("results/signs_", dvname, ".csv"), row.names = FALSE)
  } else {
    p <- readRDS(paste0("results/pdp_", dvname, ".RData"))
    the_signs <- read.csv(paste0("results/signs_", dvname, ".csv"), stringsAsFactors = FALSE)
  }
  
  # Number variables
  p <- lapply(1:length(p), function(i){
    p[[i]]+facet_grid(.~Variable, labeller = labeller(
      Variable = setNames(paste0(i, ". ", var_rename[vars[i]]), var_rename[vars[i]])
    ))
  })
  # Reduce font size
  p <- lapply(p, function(x){ x + theme(strip.text.x = element_text(size = 5),
                                        axis.text.x = element_text(size = 5),
                                        axis.text.y = element_text(size = 5))})
  if("countryiso3" %in% vars){
    p[[which(vars == "countryiso3")]] <- p[[which(vars == "countryiso3")]] + theme(axis.text.x = element_text(angle=90, size = 3))
  }  
  p <- metaforest:::merge_plots(p)
  ggsave(
    filename = paste0("results/rf_partialdependence_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".png"),
    p,
    device = "png")
  svg(paste0("results/rf_partialdependence_", gsub(".+_(.+)\\.RData", "\\1", thisfile), ".svg"))
  eval(p)
  dev.off()
  # Variable importance plot
  dirs <- read.csv(paste0("results/signs_", dvname, ".csv"), header = FALSE, stringsAsFactors = FALSE)
  dirs$V5[dirs$V5 %in% c("Positive monotonous", "Mostly positive")] <- "Positive"
  dirs$V5[dirs$V5 %in% c("Negative monotonous", "Mostly negative")] <- "Negative"

  VI <- sort(res$variable.importance, decreasing = TRUE)[1:30]
  
  VI <- data.frame(Variable = paste0(1:30, ". ", var_rename[names(VI)]), Importance = VI, Direction = dirs$V5[match(names(VI), dirs$V1)])
  VI$Variable <- ordered(VI$Variable, levels = rev(VI$Variable))
  p <- ggplot(VI, aes_string(y = "Variable", x = "Importance")) + 
    geom_segment(aes_string(x = 0, xend = "Importance", 
                            y = "Variable", yend = "Variable"), colour = "grey50", 
                 linetype = 2) + geom_vline(xintercept = 0, colour = "grey50", 
                                            linetype = 1) + xlab("Permutation Importance") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(),
                       axis.title.y = element_blank(),
                       axis.text.y = element_text(hjust=0)) + 
    geom_point(aes_string(fill = "Direction"), shape = 21, size = 2) + 
      scale_fill_manual(values = c("Positive" = "white", "Negative" = "black", "Other" = "gray70")) + 
      theme(legend.position = c(0.92, 0.2))
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


f <- list.files("results", "res.+RData", full.names = TRUE)
representative <- yaml::read_yaml("results/representative.yml")
df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)
df_testing[which(sapply(df_testing, is.character))] <-
  lapply(df_testing[which(sapply(df_testing, is.character))], factor)

df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)] <-
  lapply(df_testing[which(lapply(sapply(df_testing, unique), length) <= 5)], factor)
df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
df_training[which(sapply(df_training, is.character))] <-
  lapply(df_training[which(sapply(df_training, is.character))], factor)

df_training[which(lapply(sapply(df_training, unique), length) <= 5)] <-
  lapply(df_training[which(lapply(sapply(df_training, unique), length) <= 5)], factor)
df_representative <- df_testing[representative, ]

source("scripts/model_accuracy.R")

repr_table <- sapply(f, function(thisfile){
  #thisfile = f[1]
  tmp <- readRDS(thisfile)
  y <- gsub(".+?_(.+)\\.RData", "\\1", thisfile)
  out <- tryCatch({model_accuracy(tmp$rf$final_model,
                   newdata = df_representative[, -which(names(df_representative) == y)],
                   observed = df_representative[[y]],
                   ymean = mean(df_training[[y]], na.rm = TRUE))}, error = function(e){
                     browser()
                     c(r2 = NA, mse = NA, r_actual_pred = NA)
                   })
  names(out) <- paste0("representative_", names(out))
  out
})

write.csv(repr_table, "results/representative_fit.csv")

tab <- read.csv("results/fit_table.csv", stringsAsFactors = FALSE)
repr_table <- data.frame(t(repr_table))
tab[names(repr_table)] <- NULL
repr_table$DV <- gsub(".+_(.+)\\.RData", "\\1", rownames(repr_table))
tab <- merge(tab, repr_table, by = "DV")

write.csv(tab, "results/fit_table.csv", row.names = FALSE)
