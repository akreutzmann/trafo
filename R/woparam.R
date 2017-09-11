#' Transformations without transformation parameter
#'
#' Transforms the dependent variable of a regression model using
#' transformations without transformation parameter.  
#' 
#' @param object an object of type lm or lme. 
#' @param ... other parameters that can be passed to the function.
#' @return The return depends on the class of its argument. The 
#' documentation of particular methods gives detailed information about the 
#' return of that method.
#' @seealso  \code{\link{oneparam.lm}}
# #' ,  \code{\link{yeojohnson.lme}}
#' @keywords woparam
#' @export
woparam <- function(object,...) UseMethod("woparam")




