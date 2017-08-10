#' Modulus transformation for linear and linear mixed models
#'
#' Depending on the class of the first object, this function estimates the 
#' optimal transformation parameter for the Modulus transformation for 
#' the model given to the function.
#'
#' @param object an object of type lm or lme with the model to transform
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter. 
#' (i) Maximum likelihood approaches: for linear models maximum likelihood ("ML")
#' and for linear mixed models restricted maximum likelihood ("reml"); 
#' (ii) Skewness minimizations: for linear models only skewness minimization 
#' ("skew") and for linear mixed models also pooled skewness minimization; 
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl") for both 
#' model types. 
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to \code{c(-2, 2)} for the Modulus transformation.
#' @param plotit logical. If TRUE, a plot that illustrates the optimal 
#' transformation parameter is returned.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{transformation}
#' @keywords internal
#' @export
modulus.lme <- function(object, lambda, method, lambdarange = c(-2,2), 
                        plotit = TRUE, ...) {
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
                                 lambdarange = lambdarange, transfor = "t_mdls") 
    
    lambdaoptim <- Optim$lambdaoptim
    measoptim <- Optim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lme(lambda = lambda, y = y, formula = formula, 
                           data = data, rand_eff = rand_eff, method = method, 
                           transfor =  "t_mdls")
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    lambdavector <- seq(lambdarange[1], lambdarange[2], 0.025)
    l <- length(lambdavector)
    lambdavector[l + 1]  <- lambdaoptim
    lambdavector <- sort(lambdavector)
    measvector <- sapply(lambdavector, estim_lme, y = y, formula = formula, 
                         data = data, rand_eff = rand_eff, method = method, 
                         transfor =  "t_mdls")
    vline <- lambdaoptim
    
    if (method == "ml" | method == "reml") {
      measvector <- -measvector
      data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)  
      measoptim <- -measoptim
      y_lab <- "Profile log-likelihood"
      
      
    } else if (method == "skew" | method == "pskew") {
      data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
      y_lab <- "Skewness"
    } else if (method == "div.ks" | method == "div.cvm" | method == "div.kl") {
      data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
      y_lab <- "Divergence"
    }
    
    plot <- ggplot(data1, aes(x = lambdavector,
                              y = measvector)) + geom_line() + 
      geom_vline(xintercept = vline, linetype = "dashed") + 
      geom_hline(yintercept = measoptim, color = "red", linetype = "dashed") + 
      xlab(expression(lambda)) + ylab(y_lab)
    
    print(plot)
    
    
    # Save plot measures
    ans$plot
    ans$measvector
    ans$lambdavector
  }
  
  # Get vector of transformed and standardized transformed variable
  ans$yt <- modul(y = y, lambda = lambdaoptim)
  ans$zt <- modul_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  ans$family <- "Modulus"
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # Do we want to change to class trafo??!!
  class(ans) <- "trafo"
  ans
}