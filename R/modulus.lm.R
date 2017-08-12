#' Modulus transformation for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' Modulus transformation. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter. 
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),  
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to \code{c(-2, 2)}.
#' @param plotit logical. If TRUE, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' @param ... other parameters that can be passed to the function.
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
#' # Transform dependent variable using skewness minimization
#' modulus(object = lm_Vienna, lambda = "estim", method = "skew",
#' plotit = FALSE)
#' @export

modulus.lm <- function(object, lambda, method, lambdarange = c(-2, 2),
                       plotit = TRUE, ...) {
  
  transfor <- "t_mdls"
  
  # Get model variables: dependent variable y and explanatory variables x
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")

  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    bx_cxOptim <- est_lm(y = y, x = x, transfor = transfor, method = method, 
                         lambdarange = lambdarange) 
    
    lambdaoptim <- bx_cxOptim$lambdaoptim
    measoptim <- bx_cxOptim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lm(lambda = lambdaoptim, y = y, x = x, 
                          transfor = transfor, method = method)
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    plot_meas <- plot_trafolm(lambdarange = lambdarange, lambdaoptim = lambdaoptim, 
                              measoptim = measoptim, y = y, x = x, 
                              transfor = transfor, method = method)
    
    # Get plot measures
    ans$lambdavector <- plot_meas$lambdavector
    ans$measvector <- plot_meas$measvector
  } else if (plotit == FALSE) {
    ans$lambdavector <- NULL
    ans$measvector <- NULL
  }

  
  # Get vector of transformed and standardized transformed variable
  ans$yt <- modul(y = y, lambda = lambdaoptim)
  ans$zt <- modul_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  ans$family <- "Modulus"
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Get transformed model
  ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  
  # New class trafo
  class(ans) <- "trafo"
  ans
}