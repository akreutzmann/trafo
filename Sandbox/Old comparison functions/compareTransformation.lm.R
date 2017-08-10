#' Compare Transformation lm
#'
##' Compares families of transformations with respect to  sense of AIC, R.Squared, Skewness or Kurtosis; For positive y compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum  
#' @param object a object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see docompareTransformation()
#' @return an object of class \code{transformation}; see docompareTransformation()
#' @keywords internal
#' @export
compareTransformation.lm <- function(object, ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  data <- data.frame(y = y, x = x)
  docompareTransformation(data,...)
}