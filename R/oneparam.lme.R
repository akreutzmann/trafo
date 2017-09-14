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

oneparam.lme <- function(object, trafo, lambda = "estim", method = "ml", 
                          lambdarange, plotit = TRUE, ...) {
  
  check_oneparam(trafo = trafo, lambda = lambda, method = method, 
                 lambdarange = lambdarange, plotit = plotit, 
                 custom_trafo = custom_trafo)
  
  # Get model variables: dependent variable y and explanatory variables x
  formula <- formula(object)
  rand_eff <- names(object$coefficients$random)
  data <- object$data
  x <- model.matrix(formula, data = object$data)
  y <- as.matrix(object$data[paste(formula[2])])
  
  if (trafo == "custom") {
    custom_func <- custom_trafo[[1]]
    custom_func_std <- custom_trafo[[2]]
    custom_family <- names(custom_trafo)[[1]]
  }
  
  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    optim <- est_lme(y = y, x = x, formula = formula, data = data, 
                     rand_eff = rand_eff, method = method, 
                     lambdarange = lambdarange, trafo = trafo, 
                     custom_func = custom_func, 
                     custom_func_std = custom_func_std) 
    
    lambdaoptim <- optim$lambdaoptim
    measoptim <- optim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lme(lambda = lambda, y = y, formula = formula, 
                           data = data, rand_eff = rand_eff, method = method, 
                           trafo = trafo)
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    plot_meas <- plot_trafolme(lambdarange = lambdarange, lambdaoptim = lambdaoptim,
                               measoptim = measoptim, y = y, formula = formula, 
                               data = data, rand_eff = rand_eff, trafo = trafo, 
                               method = method)
    
    if (!is.character(plot_meas)) {
      # Get plot measures
      ans$lambdavector <- plot_meas$lambdavector
      ans$measvector <- plot_meas$measvector 
    } else {
      ans$lambdavector <- NULL
      ans$measvector <- NULL
    }
  } else if (plotit == FALSE) {
    ans$lambdavector <- NULL
    ans$measvector <- NULL
  }
  
  
  # Get vector of transformed and standardized transformed variable
  #ans$yt <- Bick_dok(y = y, lambda = lambdaoptim)
  #ans$zt <- Bick_dok_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  #ans$family <- "Bickel-Doksum"
  
  ans <- get_transformed(trafo = trafo, ans = ans, y = y, lambda = lambdaoptim)
  
  # Save estimation method
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Get transformed model
  # ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  ans$object <- object
  
  # New class trafo
  class(ans) <- c("trafo", "oneparam")
  ans
}