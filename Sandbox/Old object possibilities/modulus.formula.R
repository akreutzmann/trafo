#' #' Modulus Formula
#' #'
#' #' Modulus estimation 
#' #' @param formula object of type formula with the model to transform
#' #' @param data an optional data frame containing the variables in the model
#' #' @param \dots additional arguments to be passed to the estimation function;  see  modulusEst()
#' #' @return an object of class \code{transformation}; see modulusEst()
#' #' @keywords internal
#' #' @export
#' modulus.formula <- function(formula, data = list(), ...){
#'   model_frame <- model.frame(formula = formula, data = data)
#'   if (is.null(y <- model.response(model_frame)))
#'     stop("Dependent variable y must not be empty")
#'   if (is.null(x <- model.matrix(attr(model_frame,"terms"), data = model_frame))) 
#'     stop("Matrix of covariates X must not be empty")
#'   modulusEst(y, x, ...)
#' }