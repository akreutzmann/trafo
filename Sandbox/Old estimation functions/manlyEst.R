#' Manly Estimation
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
manlyEst <- function(y, x , lambdarange = c(-0.5, 0.5), tr = FALSE, ...) {
  qr <- qr(x)
  y <- as.numeric(y)
  n <- length(y)
  k <- ncol(x)
  lglike <- function(lambda, ...) {
    if (abs(lambda) > 0.05) {
      yt <- (exp(y*lambda) - 1L)/lambda
      zt <- yt/exp((mean(lambda*y)))
      }
    else {
      yt <- y
      zt <-  yt
    }
    if(any(is.nan(abs(zt))) | any(is.infinite(zt))) 
      llike <- -Inf
    else  
      llike <- suppressWarnings(-n/2L * log((sum(qr.resid(qr, zt)^2L))/n))
    llike
  }
  res <-suppressWarnings( optimize(f = function(lambda) lglike(lambda), lambdarange, tol = 0.0001, maximum = TRUE) )
  lambdaoptim <-  res$maximum
  logoptim <-res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  logvector <- sapply(lambdavector, lglike)
  ans <- list()
  if(is.infinite(ans$llike <- res$objective ) & tr != TRUE) 
    stop("log-likelihood is infinite or not defined for components y and x")
  else{
  if(abs(lambdaoptim > 0.05))
    yt <- (exp(y*lambdaoptim) - 1L)/lambdaoptim
  else 
    yt <- y
  if(is.infinite(logoptim) | is.na(logoptim)) modelt <- NULL
    else suppressWarnings( modelt <- lm(yt ~ ., data.frame(yt, x[, 2L:k] )))
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Manly"
  ans$yt <- yt
  ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
  }
}