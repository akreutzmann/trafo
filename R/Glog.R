#' Glog transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Glog transformation. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @return an object of class \code{trafo}.
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401, 28-36. \cr \cr
#' Gonzalez-Manteiga, W. et al. (2008). Bootstrap mean squared error of
#' a small-area EBLUP. Journal of Statistical Computation and Simulation,
#' 78:5, 443-462.
#' @examples
#' @examples
#' # Load data
#' data("cars", package = "caret")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform dependent variable 
#' glog(object = lm_cars)
#' @export

glog <- function(object) {
  
  trafo <- "glog"
  woparam(object = object, trafo = trafo)
}