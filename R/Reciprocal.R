#' Reciprocal transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Reciprocal transformation. The transformation parameter can either be 
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
#' # Load data
#' data("eusilcA_Vienna")
#' 
#' # Fit linear model
#' lm_Vienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben + 
#' rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
#' fam_allow + house_allow, data = eusilcA_Vienna)
#' 
#' # Transform dependent variable using a maximum likelihood approach
#' bickeldoksum(object = lm_Vienna, lambda = "estim", method = "ml",
#' plotit = FALSE)
#' @export

reciprocal <- function(object) {
  
  trafo <- "reciprocal"
  woparam(object = object, trafo = trafo)
}