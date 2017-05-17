#' Modulus Estimation
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2,2)
#' @param tr logical value. if tr = TRUE warning messages for the likelihood functions are suppressed - default FALSE
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of \code{profile log-likelihood} at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
modulusEst <- function(y, x, lambdarange = c(-2, 2), tr = FALSE, ...) {
  qr <- qr(x) 
  k <- ncol(x)
  u <- abs(y) + 1L
  n <- length(y)
  yt <- rep(NA, n)
  lglike <- function(lambda, ...) {
    if (abs(lambda) > 0.05) 
      yt <- sign(y)*(u^lambda - 1L)/lambda 
    else 
      yt <-  sign(y)*log(u) 
      zt <- yt/exp(mean(sign(y)*(lambda - 1L)*log(u)))
    if(any(is.nan(abs(zt))) | any(is.infinite(zt))) 
      lglike <- -Inf
    else 
      llike <- suppressWarnings(-n/ 2L * log((sum(qr.resid(qr, zt)^2L))/n))
    llike
  }  
  res <-suppressWarnings( optimize(f = function(lambda) lglike(lambda), lambdarange, tol = 0.0001, maximum = TRUE) )
  lambdaoptim <-  res$maximum
  logoptim <-res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1L]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  logvector <- sapply(lambdavector, lglike)
  if(abs(lambdaoptim > 0.05))
    yt <- sign(y)*(u^lambdaoptim - 1L)/lambdaoptim
  else 
   yt <- sign(y)*log(u)
  zt <- yt/exp(mean(sign(y)*(lambdaoptim - 1L)*log(u)))
  suppressWarnings( modelt <- lm(zt ~ ., data.frame(zt, x[, 2L:k])))
  ans <- list()
  if(is.infinite(ans$llike <- res$objective ) & tr != TRUE) 
    stop("log-likelihood is infinite or not defined for components y and x")
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Modulus"
  ans$yt <- yt
  ans$zt <- zt
  ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}