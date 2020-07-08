VarImpPlot.character <- function (mf, n.var = 30, sort = TRUE, ...) 
{
  if(is.null(names(character))){
    stop("VarImpPlot.character requires a named vector.", call. = FALSE)
  }
  var_importance <- data.frame(Variable = names(mf), 
                               importance = abs(mf))
  rownames(var_importance) <- NULL
  if (sort) {
    var_importance <- var_importance[order(-var_importance$importance), 
    ]
  }
  n.var <- min(n.var, nrow(var_importance))
  var_importance <- var_importance[1:n.var, ]
  var_importance <- var_importance[rev(rownames(var_importance)), 
  ]
  var_importance$Variable <- factor(var_importance$Variable, 
                                    levels = var_importance$Variable)
  p <- ggplot(var_importance, aes_string(y = "Variable", x = "importance")) + 
    geom_segment(aes_string(x = 0, xend = "importance", 
                            y = "Variable", yend = "Variable"), colour = "grey50", 
                 linetype = 2) + geom_vline(xintercept = 0, colour = "grey50", 
                                            linetype = 1) + geom_point(shape = 1, size = 2) + xlab("Variable Importance (absolute regression coefficient)") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())
  if (hasArg("label_elements")) {
    label_elements <- eval(match.call()[["label_elements"]])
    levels(p$data$Variable) <- rename_fun(levels(p$data$Variable), 
                                          names(label_elements), label_elements)
  }
  p
}

VarImpPlot.glmnet <- function (mf, n.var = 30, sort = TRUE, ...) 
{
  #mf <- lassoRes$finalModel
  var_importance <- as.matrix(mf$beta)
  var_importance <- data.frame(Variable = rownames(var_importance), 
                               importance = abs(var_importance[, 1]))
  rownames(var_importance) <- NULL
  if (sort) {
    var_importance <- var_importance[order(-var_importance$importance), 
                                     ]
  }
  n.var <- min(n.var, nrow(var_importance))
  var_importance <- var_importance[1:n.var, ]
  var_importance <- var_importance[rev(rownames(var_importance)), 
                                   ]
  var_importance$Variable <- factor(var_importance$Variable, 
                                    levels = var_importance$Variable)
  p <- ggplot(var_importance, aes_string(y = "Variable", x = "importance")) + 
    geom_segment(aes_string(x = 0, xend = "importance", 
                            y = "Variable", yend = "Variable"), colour = "grey50", 
                 linetype = 2) + geom_vline(xintercept = 0, colour = "grey50", 
                                            linetype = 1) + geom_point(shape = 1, size = 2) + xlab("Variable Importance (absolute regression coefficient)") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())
  if (hasArg("label_elements")) {
    label_elements <- eval(match.call()[["label_elements"]])
    levels(p$data$Variable) <- rename_fun(levels(p$data$Variable), 
                                          names(label_elements), label_elements)
  }
  p
}