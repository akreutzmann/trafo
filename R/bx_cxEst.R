#' Box Cox Estimation
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2, 2)
#' @param tr logical value. if tr = TRUE warning messages for the likelihood functions are suppressed - default FALSE
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of \code{profile log-likelihood} at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector Employed family of transformations
#' @return A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
b_cxEst <- function(y, x , method="ML", lambdarange = c(-2, 2), tr = FALSE, ...) {
  
  
  

  res <-suppressWarnings( optimize(f = generic_opt, y, x, method = "ML", interval = lambdarange, tol = 0.0001, maximum = TRUE) )
  lambdaoptim <-  res$maximum
  logoptim <-res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  logvector <- sapply(lambdavector, ML, y = y, x = x)
  

  
  
  
  if (abs(lambdaoptim) > 0.05)  
    yt <- (y^lambdaoptim - 1)/lambdaoptim
  else 
    yt <- log(y) 
  zt <- yt/exp((lambdaoptim - 1)*mean(log(y)))
  suppressWarnings( modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))
  
  ans <- list()
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE) 
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Box-Cox"
  ans$yt <- yt
  ans$zt <- zt
  ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}


generic_opt <- function(lambda, y, x, method){
  
  yt <- box_cox(y = y, lambda = lambda, shift = 0)$y
  
  model_ML <- lm(formula = yt ~ x)
  res <- residuals(model_ML)
  
  optimization <- if (method == "ML") {
    ML(y, x, lambda)
  } else if (method == "skew") {
    skewness_min(res = res)
  } else if (method == "div.ks") {
    divergence_min_KS(res = res)
  } else if (method == "div.cvm") {
    divergence_min_CvM(res = res)
  } else if (method == "div.kl") {
    divergence_min_KL(res = res)
  }
  
  return(optimization)
  
  
}


ML <- function(y, x, lambda){
  qr <- qr(x)
  n <- length(y)
  k <- ncol(x)
  yt <- rep(NA, n)
  
  
  # here include wrapper
  
  
  # Here starts ML Method
  lglike <- function(lambda, ...) {
    if (abs(lambda) != 0) {
      yt <- (y^lambda - 1)/lambda
      
    }
    else {
      yt <- log(y) 
    }
    zt <- yt/exp((lambda - 1)*mean(log(y)))
    llike <- -n/2 * log((sum(qr.resid(qr, zt)^2))/n)
    llike
  }
  
  lglike(lambda = lambda)
}



