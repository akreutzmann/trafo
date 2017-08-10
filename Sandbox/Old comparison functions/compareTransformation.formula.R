#' Compare transformartion formula
#'
#' Chooses the 'best' transformation in the sense of AIC, R.Squared, Skewness or Kurtosis; For positive y compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum  
#' @param formula object of type formula with the model to transform
#' @param data an optional data frame containing the variables in the model 
#' @param \dots additional arguments to be passed to the estimation function;  see doselectTransformation()
#' @return an object of class \code{transformation}; see doselectTransformation()
#' @keywords internal
#' @export
compareTransformation.formula <- function(formula, data = list(), ...){
  model_frame <- model.frame(formula = formula, data = data)
  if (is.null(y <- model.response(model_frame)))
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame,"terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  data <- data.frame(y = y, x = x)
  docompareTransformation(data, ...)
}
