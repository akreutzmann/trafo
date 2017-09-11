#' One parameter transformations for linear models
#'
#' The function transforms the dependent variable of a linear model using the 
#' one parameter transformations. The transformation parameter can either be 
#' estimated using different estimation methods or given. 
#'
#' @param object an object of type lm. 
#' @param trafo character that determines the selected transformation.
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter. Defaults to "estim".
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
#' @param custom_trafo a list that determines a one parameter transformation and
#' the standardized one parameter transformation.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{trafo}.
#' @references
#' Battese, G.E., Harter, R.M. and Fuller, W.A. (1988). An Error-Components
#' Model for Predictions of County Crop Areas Using Survey and Satellite Data.
#' Journal of the American Statistical Association, Vol.83, No. 401, 28-36. \cr \cr
#' Gonzalez-Manteiga, W. et al. (2008). Bootstrap mean squared error of
#' a small-area EBLUP. Journal of Statistical Computation and Simulation,
#' 78:5, 443-462.
#' @export

oneparam.lm <- function(object, trafo, lambda = "estim", method = "ml", 
                          lambdarange, plotit = TRUE, custom_trafo, ...) {
  
  # Get model variables: dependent variable y and explanatory variables x
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")


  if (trafo == "custom") {
    custom_func <- custom_trafo[[1]]
    custom_func_std <- custom_trafo[[2]]
    custom_family <- names(custom_trafo)[[1]]
  }
  
  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    optim <- est_lm(y = y, x = x, trafo = trafo, method = method, 
                         lambdarange = lambdarange, custom_func = custom_func, 
                         custom_func_std = custom_func_std) 
    
    lambdaoptim <- optim$lambdaoptim
    measoptim <- optim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lm(lambda = lambdaoptim, y = y, x = x, 
                          trafo = trafo, method = method)
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    plot_meas <- plot_trafolm(lambdarange = lambdarange, lambdaoptim = lambdaoptim, 
                              measoptim = measoptim, y = y, x = x, 
                              trafo = trafo, method = method, 
                              custom_func = custom_func, 
                              custom_func_std = custom_func_std)
    
    # Get plot measures
    ans$lambdavector <- plot_meas$lambdavector
    ans$measvector <- plot_meas$measvector
  } else if (plotit == FALSE) {
    ans$lambdavector <- NULL
    ans$measvector <- NULL
  }
  
  # Get vector of transformed and standardized transformed variable
  #ans$yt <- Yeo_john(y = y, lambda = lambdaoptim)
  #ans$zt <- Yeo_john_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  #ans$family <- "Yeo-Johnson"
  
  ans <- get_transformed(trafo = trafo, ans = ans, y = y, lambda = lambdaoptim,
                         custom_func = custom_func,
                         custom_func_std = custom_func_std, 
                         custom_family = custom_family)
  
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Get transformed model
  ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  
  # New class trafo
  class(ans) <- c("trafo", "oneparam")
  ans
}