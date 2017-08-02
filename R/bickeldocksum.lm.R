#' Bickel-Docksum lm
#'
#' Bickel-Docksum estimation 
#' @param object of type lm with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see bickeldocksumEst()
#' @return an object of class \code{transformation}; see bickeldocksumEst()
#' @keywords internal
#' @export
bickeldoksum.lm <- function(object, method, lambdarange = c(1e-11, 2), ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  # bickeldoksumEst(y, x, ...)
  est_lm(y = y, x = x, transfor = "t_bck_dk", method = method, lambdarange = lambdarange, ...)
}