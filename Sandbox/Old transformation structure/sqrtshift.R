#' sqrtshift Transformation
#'
#' Transforms the dependent variable of a regression model using the sqrtshift
#' transformation. The regression model can be a linear or a linear mixed model 
#' with one random intercept. 
#' 
#' @param object an object of type lm or lme. 
#' @param ... other parameters that can be passed to the function.
#' @return The return depends on the class of its argument. The 
#' documentation of particular methods gives detailed information about the 
#' return of that method.
#' @seealso  \code{\link{sqrtshift.lm}},  \code{\link{sqrtshift.lme}}
#' @keywords Sqrt shift
#' @export
sqrtshift <- function(object,...) UseMethod("sqrtshift")

