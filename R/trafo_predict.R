#' Predict method for linear models with transformed dependent variable
#'
#' The function returns predicted values based on the linear model. The 
#' predicted values are back-transformed corresponding to the transformation 
#' used in the model. Note that the back-transformation can induce a bias.
#' 
#' @param object an object of type \code{trafo_lm}.
#' @param newdata an optional data frame in which to look for variables with 
#' which to predict. If omitted, the fitted values are used. 
#' @param interval type of interval calculation. Possible values: "none", "confidence",
#' "prediction". Defaults to "none".
#' @param level tolerance/confidence level. 
#' @return A vector of predictions or a matrix of predictions and bounds with 
#' column names fit, lwr and upr if interval is set. 
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Compare untransformed and transformed model
#' trafo_cars <- trafo_lm(object = lm_cars, trafo = "bickeldoksum", method = "skew", 
#' lambdarange = c(1e-11, 2))
#' 
#' # Get predictions in the back-transformed scale
#' trafo_predict(trafo_cars)
#' @importFrom stats .checkMFClasses delete.response na.pass napredict qt terms
#' cor
#' @importFrom graphics strwidth text
#' @export

trafo_predict <- function(object, newdata = NULL, 
                          interval = "none", level = 0.95) {
  
  
  check_trafo_predict(object = object, trafo = object$trafo)
  
  # Get the information from object of type trafo_lm
  std <- object$std
  shift <- with_shift(y = object$orig_mod$model[, paste0(formula(object$orig_mod)[2])], 
                      shift = 0)
  
  # Overwrite object with transformed model
  mod_trafo <- object$trafo_mod
  mod_orig <- object$orig_mod
  
  predict_trafo <- predict(mod_trafo, newdata = newdata, interval = interval, 
                           level = level)
  
  if (interval == "none") {
    predict_trafo <- back_transformed(predict_trafo, trafo = object$trafo, 
                                      lambda = object$lambdahat, shift = shift)
  } else if (interval != "none") {
    predict_trafo[, "fit"] <- back_transformed(predict_trafo[, "fit"], trafo = object$trafo, 
                                               lambda = object$lambdahat, shift = shift)
    predict_trafo[, "lwr"] <- back_transformed(predict_trafo[, "lwr"], trafo = object$trafo, 
                                               lambda = object$lambdahat, shift = shift)
    predict_trafo[, "upr"] <- back_transformed(predict_trafo[, "upr"], trafo = object$trafo, 
                                               lambda = object$lambdahat, shift = shift)
  }
  
  predict_orig <- predict(mod_orig, newdata = newdata, interval = interval, 
                          level = level)
  
  warning("Expected mean and interval limits are naively backtransformed.")
  return(predict_trafo)
}
  