#' Box-Cox lm
#'
#' Box-Cox estimation 
#' @param object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see bcxEst()
#' @return an object of class \code{transformation}; see bcxEst()
#' @keywords internal
#' @export
bx_cx.lm <- function(object, method, lambdarange = c(-2, 2), ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  if (any(y <= 0)) 
   stop("response variable y must be positive")
  #bcxEst(y, x, ...)
  
  bx_cxEst_lm(y = y, x = x, method = method, lambdarange = lambdarange, ...)
}