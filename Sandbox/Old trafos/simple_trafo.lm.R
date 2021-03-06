#' Transformations without parameter for linear models
#'
#' The function transforms the dependent variable of a linear model using 
#' different transformations without parameter. 
#'
#' @param object an object of type lm. 
#' @param trafo character that determines the transformation.
#' @param custom_trafo a function that specifies a transformation without 
#' paramater that needs to be estimated or given.
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


woparam <- function(object, trafo, custom_trafo = NULL) {
  
  
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  
  # For saving returns
  ans <- list()
  
  if (trafo == "reciprocal") {
    ans$yt <- reciprocal(y = y)
    ans$family <- "Reciprocal"
  } else if (trafo == "neglog") {
    ans$yt <- neg_log(y = y)
    ans$family <- "Neglog"
  } else if (trafo == "custom") {
    ans$yt <- custom_trafo(y = y)
    ans$family <- "Custom"
  }
  
  ans$lambdavector <- NULL
  ans$measvector <- NULL   
  ans$zt <- NULL         
  ans$method <- NULL      
  ans$lambdahat <- NULL 
  ans$measoptim <- NULL    
  
  # Save estimation method
  ans$method <- NULL
  
  # Save optimal transformation parameter and corresponding statistics depending
  # on the estimation method
  ans$lambdahat <- NULL
  ans$measoptim <- NULL
  
  # Get transformed model
  ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  
  # New class trafo
  class(ans) <- c("trafo", "woparam")
  ans
  
}