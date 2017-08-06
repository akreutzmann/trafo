#' Modulus lme
#'
#' Modulus estimation 
#' @param object of type lme with the model to transform
#' @param \dots additional arguments to be passed to the estimation function; see modulusEst()
#' @return an object of class \code{transformation}; see modulusEst()
#' @keywords internal
#' @export
modulus.lme <- function(object, method, lambdarange = c(-2,2), tol = 0.0001, 
                        ...) {
  formula <- formula(object)
  rand_eff <- names(object$coefficients$random)
  data <- object$data
  x <- model.matrix(formula, data = object$data)
  #x <- cbind(object$data[paste(formula[3][[1]][[2]])], 
  #               object$data[paste(formula[3][[1]][[3]])])
  y <- as.matrix(object$data[paste(formula[2])])
  #if (is.null(y <- model.response(model_frame))) 
  #  stop("Dependent variable y must not be empty")
  #if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
  #  stop("Matrix of covariates X must not be empty")
  #if (any(y <= 0)) 
  #  stop("response variable y must be positive")
  #bcxEst(y, x, ...)
  est_modulus <- est_lme(y, x, formula, rand_eff = rand_eff, data = data, transfor = "t_mdls",  
          method, lambdarange, tol = tol, ...)
  est_modulus$model <- object
  est_modulus
}