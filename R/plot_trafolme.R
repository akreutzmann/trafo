#' Plot for optimal transformation parameter - linear models
#' 
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter.
#' @param lambdaoptim optimal or given transformation parameter.
#' @param measoptim measure at the optimal transformation parameter.
#' @param y dependent variable.
#' @param formula formula object.
#' @param data data extracted from the lme object.
#' @param rand_eff random effect extracted from lme object.
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Restricted maximum likelihood approach ("reml"), 
#' (ii) Skewness minimization ("skew") and pooled skewness minimization ("pskew"), 
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' @param transfor a character string that selects the transformation.
#' @keywords internal


plot_trafolme <- function(lambdarange, lambdaoptim, measoptim, 
                         y, formula, data, rand_eff, transfor, method) {
  
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.025)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  measvector <- sapply(lambdavector, estim_lme, y = y, formula = formula, 
                       data = data, rand_eff = rand_eff, method = method, 
                       transfor =  transfor)
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
  
  #plot <- ggplot(data1, aes(x = lambdavector,
  #                          y = measvector)) + geom_line() + 
  #  geom_vline(xintercept = vline, linetype = "dashed") + 
  #  geom_hline(yintercept = measoptim, color = "red", linetype = "dashed") + 
  #  xlab(expression(lambda)) + ylab(y_lab)
  
  plot <- NULL
  abline <- NULL
  dev.flush <- NULL
  
  dev.hold()
  plot(data1$lambdavector, data1$measvector, type = "l", lwd = 1.5,
      xlab = expression(lambda), ylab = y_lab) 
  abline(h = measoptim, lty = 2, col = "red")
  abline(v = lambdaoptim, lty = 2)
  dev.flush()


  out <- list(lambdavector = lambdavector, 
              measvector = measvector)
  return(out)
  }