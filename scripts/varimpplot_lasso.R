VarImpPlot <- function (mf, n.var = 30, sort = TRUE, ...){
  UseMethod("VarImpPlot", mf)
}

VarImpPlot.numeric <- function (mf, n.var = 30, sort = TRUE, ...) 
{
  if(is.null(names(mf))){
    stop("VarImpPlot.numeric requires a named vector.", call. = FALSE)
  }
  var_importance <- data.frame(Variable = names(mf), 
                               importance = abs(mf))
  if(any(mf < 0)){
    var_importance$Sign <- c("-", NA, "+")[sign(mf)+2]
  }
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
                                            linetype = 1) + xlab("Variable Importance (absolute value)") + 
    theme_bw() + theme(panel.grid.major.x = element_blank(), 
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())
  if(ncol(var_importance) == 3){
    p <- p + geom_point(aes_string(fill = "Sign"), shape = 21) + 
      scale_fill_manual(values = c("+" = "white", "-" = "black")) + 
      theme(legend.position = c(0.96, 0.09))
  } else {
    p <- p + geom_point(shape = 1, size = 2)
  }
  
  if (hasArg("label_elements")) {
    label_elements <- eval(match.call()[["label_elements"]])
    levels(p$data$Variable) <- rename_fun(levels(p$data$Variable), 
                                          names(label_elements), label_elements)
  }
  return(p)
}

VarImpPlot.glmnet <- function (mf, n.var = 30, sort = TRUE, ...) 
{
  cl <- as.list(match.call()[-1])
  cl$mf <- as.vector(mf[["beta"]])
  names(cl$mf) <- rownames(mf[["beta"]])
  p <- do.call("VarImpPlot", cl) + xlab("Variable Importance (absolute regression coefficient)")
  return(p)
}