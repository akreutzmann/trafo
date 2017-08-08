#' Box-Cox transformation for linear and linear mixed models
#'
#' Depending on the class of the first object, this function estimates the 
#' optimal transformation parameter for the Box-Cox transformation for the model 
#' given to the function.
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
#' Defaults to \code{c(-2, 2)} for the Box-Cox transformation.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{transformation}
#' @keywords internal
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export
bx_cx_new_lm <- function(object, method, lambdarange = c(-2, 2), plotit = TRUE,
                         ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  if (any(y <= 0)) 
    stop("response variable y must be positive")
  #bcxEst(y, x, ...)
  
  # For saving returns
  ans <- list()
  
  # Get the optimal transformation parameter
  bx_cxOptim <- est_lm_new(y = y, x = x, transfor = "t_bx_cx", method = method, 
                          lambdarange = lambdarange)
  
  lambdaoptim <- bx_cxOptim$lambdaoptim
  measoptim <- bx_cxOptim$measoptim
  
  
  # Plot the curve of the measure with line at the optimal transformation 
  # parameter
  if(plotit == TRUE) {
    lambdavector <- seq(lambdarange[1], lambdarange[2], 0.025)
    l <- length(lambdavector)
    lambdavector[l + 1]  <- lambdaoptim
    lambdavector <- sort(lambdavector)
    measvector <- sapply(lambdavector, estim_lm, y = y, x = x, transfor = "t_bx_cx",
                        method = method)
    
    lim <- measoptim + qchisq(0.95, 1)/2
    m <- length(measvector)
    index <- range((1L:m)[measvector < lim])
    cinf <- lambdavector[index[1]]
    csup <- lambdavector[index[2]]
    vline <- c(cinf, lambdaoptim, csup)
    
    if(method == "ml" | method == "reml") {
      measvector <- -measvector
      data1 <- data.frame(measvector = measvector,  lambdavector= lambdavector)  
      measoptim <- -measoptim
      
      
      
    } else if (method == "skew") {
      data1 <- data.frame(logvector = logvector,  lambdavector= lambdavector)  
    }
    plot <- ggplot(data1, aes(x=lambdavector,
                            y=measvector))+ geom_line()+ 
            geom_vline(xintercept = vline,linetype="dashed") + 
            geom_abline(intercept = measoptim, color="red", linetype="dashed") + 
            xlab(expression(lambda)) + ylab("Profile log-likelihood")
    
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
  
  
  # Do we want to change to class trafo??!!
  class(ans) <- "transformation"
  ans
}