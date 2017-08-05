#' Yeo-Johnson lm
#'
#' Yeo-Johnson estimation 
#' @param lm object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see yeojohnsonEst()
#' @return an object of class \code{transformation}; see yeojohnsonEst()
#' @keywords internal
#' @export
yeojohnson.lm <- function(object, method, lambdarange = c(-2, 2), ...) {
  
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  # yeojohnsonEst(y,x, ...)
  est_lm(y = y, x = x, transfor = "t_y_jhnsn", method = method, lambdarange = lambdarange, ...)
}