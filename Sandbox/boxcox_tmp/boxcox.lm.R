#' Box-Cox transformation for linear and linear mixed models
#'
#' Depending on the class of the first object, this function estimates the 
#' optimal transformation parameter for the Box-Cox transformation for the model 
#' given to the function.
#'
#' @param object an object of type lm or lme with the model to transform
#' @param lambda either a character named "estim" if the optimal lambda should
#' be estimated or a numeric value determining a given lambda. 
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
#' Defaults to \code{c(-2, 2)} for the Box-Cox transformation.
#' @param plotit logical. If TRUE, a plot that illustrates the optimal 
#' transformation parameter is returned.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{transformation}
#' @keywords internal
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export
boxcox.lm <- function(object, lambda, method, lambdarange = c(-2, 2), 
                         plotit = TRUE, ...) {
  
  
  # Get model variables: dependent variable y and explanatory variables x
  model_frame <- object$model 
  
  # Check if arguments are as expected (for model variables)
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  if (any(y <= 0)) 
    stop("response variable y must be positive")

  
  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  if (lambda == "estim") {
    bx_cxOptim <- est_lm(y = y, x = x, transfor = "t_bx_cx", method = method, 
                         lambdarange = lambdarange) 
    
    lambdaoptim <- bx_cxOptim$lambdaoptim
    measoptim <- bx_cxOptim$measoptim
    
  } else if (is.numeric(lambda)) {
    lambdaoptim <- lambda
    measoptim <- estim_lm(lambda = lambdaoptim, y = y, x = x, 
                       transfor = "t_bx_cx", method = method)
  }
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if (plotit == TRUE) {
    lambdavector <- seq(lambdarange[1], lambdarange[2], 0.025)
    l <- length(lambdavector)
    lambdavector[l + 1]  <- lambdaoptim
    lambdavector <- sort(lambdavector)
    measvector <- sapply(lambdavector, estim_lm, y = y, x = x, transfor = "t_bx_cx",
                        method = method)
    
    #lim <- measoptim + qchisq(0.95, 1)/2
    #m <- length(measvector)
    #index <- range((1L:m)[measvector < lim])
    #cinf <- lambdavector[index[1]]
    #csup <- lambdavector[index[2]]
    #vline <- c(cinf, lambdaoptim, csup)
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
  
  ans$yt <- box_cox(y = y, lambda = lambdaoptim)$y
  ans$zt <- box_cox_std(y = y, lambda = lambdaoptim)
  
  # Save transformation family and method
  ans$family <- "Box-Cox"
  ans$method <- method
  
  ans$lambdahat <- lambdaoptim
  ans$measoptim <- measoptim
  
  # New class trafo
  class(ans) <- "trafo"
  ans
}