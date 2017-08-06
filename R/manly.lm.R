#' Manly lme
#'
#' Manly estimation 
#' @param lm object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see manlyEst()
#' @return an object of class \code{transformation}; see manlyEst()
#' @keywords internal
#' @export
manly.lm <- function(object, method, lambdarange = c(-2, 2), tol = 0.0001,
                     ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  #manlyEst(y, x, ...)
  est_manly <- est_lm(y = y, x = x, transfor = "t_mnl", method = method, 
         lambdarange = lambdarange, tol = tol, ...)
  est_manly$model <- object
  est_manly
}