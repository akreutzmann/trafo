#' Dual lm
#'
#' Dual estimation 
#' @param object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see dualEst()
#' @return an object of class \code{transformation}; see dualEst()
#' @keywords internal
#' @export
dual.lm <- function(object, method, lambdarange = c(0, 2), tol = 0.0001,
                    ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  # dualEst(y, x, ...)
  est_lm(y = y, x = x, transfor = "t_dl", method = method, 
         lambdarange = lambdarange, tol = tol, ...)
}
