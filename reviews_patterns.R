# VIFS
df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)

df_training[which(sapply(df_training, is.character))] <- 
  lapply(df_training[which(sapply(df_training, is.character))], factor)

df_training[which(lapply(sapply(df_training, unique), length) <= 5)] <- 
  lapply(df_training[which(lapply(sapply(df_training, unique), length) <= 5)], factor)

tmp <- df_training[sapply(df_training, is.factor)]

df2 <- df_training

desc <- read.csv("descriptives_itemscales.csv", stringsAsFactors = FALSE)
df2 <- df2[, names(df2) %in% desc$name]

df2[names(tmp)] <- NULL
df2[["date"]] <- NULL
df2[["countryiso3"]] <- NULL

res <- lm(c19perbeh ~.,data = df2)
vifs <- car::vif(res)

range(vifs)


library(ranger)
source("scripts/varimpplot_lasso.R")
source("scripts/model_accuracy.R")

df_training <- read.csv("df_training_imputed.csv", stringsAsFactors = FALSE)
df_testing <- read.csv("df_testing_imputed.csv", stringsAsFactors = FALSE)

remove <- c("c19normshould",
  "c19proso",
  "isoimmi_inperson",
  "houseleave",
  "isoothppl_inperson",
  "c19normdo",
  "ecoproso")

df_training[remove] <- NULL
df_testing[remove] <- NULL
y = "c19perbeh"
X <- df_training[, !names(df_training) == y]
X_las <- model.matrix(~., X)[, -1]
Y <- df_training[[y]]

X_test <- df_testing[, !names(df_testing) == y]
X_las_test <- model.matrix(~., X_test)[, -1]
Y_test <- df_testing[[y]]

set.seed(953007)
res <- ranger(c19perbeh~., data = df_training, num.trees = 1000,
              min.node.size = 6,
              mtry = 31,
              importance = "permutation")


fits <- c(
                    model_accuracy(res,
                                   olddata = df_training,
                                   observed = Y,
                                   ymean = mean(Y, na.rm = TRUE)),
                    model_accuracy(res,
                                   newdata = df_testing,
                                   observed = Y_test,
                                   ymean = mean(Y, na.rm = TRUE)))

names(fits) <- c(paste0("train_", c("r2", "mse", "r_actual_pred")), paste0("test_", c("r2", "mse", "r_actual_pred")))  
fits


# Separate PDP ------------------------------------------------------------

p <- readRDS("results/pdp_c19perbeh.RData")
df_vars <- read.csv("scripts/df_training_labs.csv", stringsAsFactors = F)
var_rename <- tolower(df_vars$lab)
names(var_rename) <- df_vars$X

p <- for(i in 1:length(p)){
  thisp <- p[[i]]+facet_grid(.~Variable, labeller = labeller(
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

