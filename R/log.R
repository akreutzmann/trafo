#' Log transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Log transformation. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @return an object of class \code{trafo}.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable 
#' logtrafo(object = lm_cars)
#' @export

logtrafo <- function(object) {
  
  trafo <- "log"
  woparam(object = object, trafo = trafo)
}