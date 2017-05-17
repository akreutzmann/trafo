#' The lambda and the profile likelihood for the Bickel-Docksum Transformation
#' 
# @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda)- c(0,2)
#' @param tr logical value. if tr = TRUE warning messages for the likelihood functions are suppressed - default FALSE
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of \code{profile log-likelihood} at its maximum
#' @return lambdahat The value of lambda for which the log-likelihood is maximized
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return zt Vector of the (normalized) transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
bickeldoksumEst <-  function(y, x,  lambdarange = c(0.05, 2), tr = FALSE,...) {
  qr <- qr(x) 
  u <- abs(y) + 1
  n <- length(y)
  k <- ncol(x)
  yt <- rep(1, n)
  zt <- rep(1, n)
  lglike <- function(lambda,...){
    if (lambda > 0.05)   yt <- sign(y)*(u^lambda - 1)/lambda
    zt <- yt/exp(mean(sign(y)*(lambda-1)*log(u)))
    llike <- suppressWarnings(-n/ 2* log((sum(qr.resid(qr, zt)^2))/n))
    llike 
  }
  res <-suppressWarnings( optimize(f = function(lambda) lglike(lambda),lambdarange, tol = 0.0001, maximum = TRUE) )
  lambdaoptim <-  res$maximum
  logoptim <-res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.05)
  l <- length(lambdavector)
  lambdavector[l + 1L]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  logvector <- sapply(lambdavector, lglike)
  yt <- (sign(y)*(u^lambdaoptim) - 1)/lambdaoptim
  zt <- yt/exp(mean(sign(y)*(lambdaoptim-1)*log(u)))
  suppressWarnings( modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k])))
  ans <- list()
  if(is.infinite(ans$llike <- res$objective ) & tr !=TRUE) 
    stop("log-likelihood is infinite or not defined for components y and x")
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Bickel-Doksum"
  ans$yt <- yt
  ans$zt <- zt
  ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}