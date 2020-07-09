#' @title Compute model accuracy
#' @description Compute model accurary
#' @param fit A fit model object.
#' @param newdata New predictor data to evaluate the model fit, default: NULL.
#' @param observed Numeric vector of the dependent variable, default: NULL.
#' @param ymean Numeric. Mean to compare predicted values to, default: NULL.
#' @param olddata Original predictor data used to fit the model, default: NULL.
#' @param ... Additional arguments passed to 'predict' functions.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ModelAccuracy
#' @keywords internal
#' @export
#' @importFrom methods hasArg
model_accuracy <-
  function(fit,
           newdata = NULL,
           observed = NULL,
           ymean = NULL,
           olddata = NULL,
           ...) {

    switch(
      class(fit)[1],
      rma.uni = {
        if (is.null(newdata)) {
          predicted	  <- predict(fit)$pred
          if (length(predicted) == 1) {
            predicted <- rep(predicted, length(observed))
          }
        } else {
          colnames(newdata) <- NULL
          predicted <- predict(fit, newmods = as.matrix(newdata))$pred
        }
      },
      MetaForest = {
        if (is.null(newdata)) {
          predicted <- fit$predictions
        } else {
          predicted <- predict(fit, data = newdata)$predictions
        }
      },
      ranger = {
        if (is.null(newdata)) {
          predicted <- fit$predictions
        } else {
          predicted <- predict(fit, data = newdata)$predictions
        }
      },
      FEmrt = {
        if (is.null(newdata)) {
          predicted	  <- suppressWarnings(predict(fit, newdata = fit$data)$g)
        } else {
          predicted <- suppressWarnings(predict(fit,
                                                newdata = newdata)$g)
        }
      },
      
      lma_it = {
        if (is.null(newdata)) {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(olddata), s= fit[[fit$use_lambda]], ...)
          } else {
            predicted	<- predict(fit, newx = as.matrix(olddata), ...)
          }
        } else {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(newdata), s= fit[[fit$use_lambda]], ...)
          } else {
            predicted	<- predict(fit, newx = as.matrix(newdata), ...)
          }
        }
      }
      ,
      cv.glmnet = {
        if (is.null(newdata)) {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(olddata), s= fit[["lambda.1se"]], ...)
          } else {
            predicted	<- predict(fit, newx = as.matrix(olddata), ...)
          }
        } else {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(newdata), s= fit[["lambda.1se"]], ...)
          } else {
            predicted	<- predict(fit, newx = as.matrix(newdata), ...)
          }
        }
      },
      elnet = {
        if (is.null(newdata)) {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(olddata), s= fit[["lambda.1se"]], ...)[,1]
          } else {
            predicted	<- predict(fit, newx = as.matrix(olddata), ...)[, 1]
          }
        } else {
          if(!hasArg("s")){
            predicted	<- predict(fit, newx = as.matrix(newdata), s= fit[["lambda.1se"]], ...)[, 1]
          } else {
            predicted	<- predict(fit, newx = as.matrix(newdata), ...)[, 1]
          }
        }
      },
      {
        if (is.null(newdata)) {
          predicted	<- predict(fit, olddata, ...)
        } else {
          predicted	<- predict(fit, newdata, ...)
        }
      }
    )
    
    if (is.null(ymean)) ymean <- mean(observed)
    
    if (anyNA(predicted))
      message("Predictions for model of class ", class(fit)[1], " contained NAs. These were replaced with the value of 'ymean'.")
    predicted[is.na(predicted)] <- 0
    
    SS.total    <- sum((observed - ymean) ^ 2)
    SS.residual <- sum((observed - predicted) ^ 2)
    SS.regression <- sum((predicted - ymean) ^ 2)
    
    r.2	<- 1 - SS.residual / SS.total
    mse	<- SS.residual / length(observed)
    
    if (sd(predicted) == 0) {
      r.actual.pred <- 0
    } else{
      r.actual.pred <- cor(observed, predicted)
    }
    
    c(r2 = r.2, mse = mse, r_actual_pred = r.actual.pred)
  }
