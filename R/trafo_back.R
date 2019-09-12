#' Back-transforms vectors with used transformation in trafo_lm
#'
#' The function transforms vectors as the prediction or confidence intervals 
#' back to the original scale of the used transformation in trafo_lm.
#' 
#' @param object an object of type \code{trafo_lm}.
#' @param prediction the return of the predict.lm method. 
#' @return The backtransformed prediction.
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
#' # Back-transform prediction and confidence interval
#' trafo_back(trafo_cars, predict(trafo_cars$trafo_mod, interval = "confidence"))
#' 
#' @importFrom stats .checkMFClasses delete.response na.pass napredict qt terms
#' cor
#' @importFrom graphics strwidth text
#' @export

trafo_back <- function(object, prediction) {

  # Get the information from object of type trafo_lm
  std <- object$std
  shift <- with_shift(y = object$orig_mod$model[, paste0(formula(object$orig_mod)[2])], 
                      shift = 0)
 
  if (is.vector(prediction)) {
    backtransformed <- back_transformed(prediction, trafo = object$trafo, 
                                               lambda = object$lambdahat, shift = shift)
  } else if (is.matrix(prediction) | is.data.frame(prediction)) {
    backtransformed <- NULL
    for (i in 1:dim(prediction)[2]) {
      backtransformed <- cbind(backtransformed, back_transformed(prediction[, i], 
                                                                 trafo = object$trafo, 
                                                                 lambda = object$lambdahat, 
                                                                 shift = shift))
    }
    colnames(backtransformed) <- colnames(prediction)
  }
  warning("Vectors are naively backtransformed.")
  return(backtransformed)
}
  