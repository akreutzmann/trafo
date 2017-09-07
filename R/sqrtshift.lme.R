#' Log shift opt transformation for linear mixed models
#'
#' The function transforms the dependent variable of a linear mixed model with 
#' one random intercept using the Log shift opt transformation. The 
#' transformation parameter can either be estimated using different estimation 
#' methods or given. 
#'
#' @param object an object of type lme. 
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter. Defaults to "estim".
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Restricted maximum likelihood approach ("reml"), 
#' (ii) Skewness minimization ("skew") and pooled skewness minimization ("pskew"), 
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to \code{c(0, 2)}.
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
#' # Fit linear mixed model
#' require(nlme)
#' lme_Vienna <- lme(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
#' rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + fam_allow + 
#' house_allow, random = ~ 1 | county, data = eusilcA_Vienna, 
#' na.action = na.omit)
#' 
#' # Transform dependent variable using pooled skewness minimization
#' boxcox(object = lme_Vienna, lambda = "estim", method = "pskew",
#' plotit = FALSE)
#' @export

sqrtshift.lme <- function(object, lambda = "estim", method = "reml", 
                       lambdarange = c(0,2), plotit = TRUE, ...) {
  
  trafo <- "sqrtshift"
  
  # Get model variables: dependent variable y and explanatory variables x
  formula <- formula(object)
  rand_eff <- names(object$coefficients$random)
  data <- object$data
  x <- model.matrix(formula, data = object$data)
  y <- as.matrix(object$data[paste(formula[2])])

  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    Optim <- est_lme(y = y, x = x, formula = formula, data = data, 
                     rand_eff = rand_eff, method = method, 
                     lambdarange = lambdarange, trafo = trafo) 
    
    lambdaoptim <- Optim$lambdaoptim
    measoptim <- Optim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lme(lambda = lambda, y = y, formula = formula, 
                           data = data, rand_eff = rand_eff, method = method, 
                           trafo =  trafo)
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
  #ans$yt <- box_cox(y = y, lambda = lambdaoptim)$y
  #ans$zt <- box_cox_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  #ans$family <- "Box-Cox"
  
  ans <- get_transformed(trafo = trafo, ans = ans, y = y, lambda = lambdaoptim)
  
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Get transformed model
  ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  
  # New class trafo
  class(ans) <- "trafo"
  ans
}