#' Diagnostics for fitted models
#'
#' Returns information about the transformation and selected diagnostics. 
#' 
#' @param object an object of type trafo_mod. 
#' @param ... other parameters that can be passed to the function.
#' @return The return depends on the class of its argument. The 
#' documentation of particular methods gives detailed information about the 
#' return of that method.
#' @seealso  \code{\link{diagnostics.trafo_mod}}
# #' ,  \code{\link{yeojohnson.lme}}
#' @keywords diagnostics
#' @export
diagnostics <- function(object,...) UseMethod("diagnostics")




