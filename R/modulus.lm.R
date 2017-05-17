#' Modulus lm
#'
#' Modulus estimation 
#' @param object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see modulusEst()
#' @return an object of class \code{transformation}; see modulusEst()
#' @keywords internal
#' @export
modulus.lm <- function(object, ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  modulusEst(y, x, ...)
}