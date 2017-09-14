#' Neg log transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Neg log transformation. The transformation parameter can either be 
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
#' neglog(object = lm_cars)
#' @export

neglog <- function(object) {
  
  trafo <- "neglog"
  woparam(object = object, trafo = trafo)
}