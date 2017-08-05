#' Yeo-Johnson Estimation
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2,2)
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
yeojohnsonEst <- function(y, x, lambdarange = c(-2, 2), tr = FALSE, ...) {
  qr <- qr(x) 
  n <- length(y)
  k <- ncol(x)
  u <- abs(y + 1L) 
  yt <- rep(NA, n)
  negativos <- which(y < 0)
  positivos <- which(y >= 0)
  bx <- function(lambda, u, ...) {
    if (abs(lambda) > 0.05) 
      yt <- (u^lambda - 1L)/lambda
    else  
      yt <- log(u) 
    yt
  }
  lglike <- function(lambda,...){
  yt[positivos] <- bx(lambda, u[positivos])
  yt[negativos] <- -bx(lambda = lambda-2L, u[negativos])
  zt <- yt/exp(mean(sign(y)*(lambda-1)*log(u)))
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
  yt[positivos] <- bx(lambdaoptim, u[positivos])
  yt[negativos] <- -bx(lambda = lambdaoptim - 2L, u[negativos])
  zt <- yt/exp(mean(sign(y)*(lambdaoptim-1)*log(u)))
  suppressWarnings( modelt <- lm(zt ~ ., data.frame(zt, x[, 2L:k])))
   ans <- list()
   if(is.infinite(ans$llike <- res$objective ) & tr != TRUE) 
     stop("log-likelihood is infinite or not defined for components y and x")
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Yeo-Johnson"
  ans$yt <- yt
  ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}