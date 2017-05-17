#' Dual Estimation
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda)- c(0,2)
#' @param tr logical value. if tr = TRUE warning messages for the likelihood functions are suppressed - default FALSE
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of \code{profile log-likelihood} at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
dualEst <- function(y, x, lambda.dual = 0.5, tr = FALSE, ...) {
  lambda <- lambda.dual
  qr <- qr(x)
  x <- as.matrix(x)
  y <- as.numeric(y)
  n <- length(y)
  k <- ncol(x)
 if( !is.numeric(lambda)| length(lambda)>1  | !is.null(dim(lambda))) stop("lambda must of type numeric and contain only one position")
 if (abs(lambda) > 0.05)  yt <- (y^(lambda) - y^(-lambda))/2*lambda
  else  yt <- log(y)
  zt <- yt/exp((mean(log((y^(lambda-1) + y^(-lambda-1))/2))))
  sst <- var(zt)*(n-1)
  sse <- sum(qr.resid(qr, zt)^2)
  ans <- list()
  ans$lambda.user <- lambda
  ans$family <- "Dual"
  ans$yt <- yt
  ans$zt <- zt
  suppressWarnings( modelt <- lm(ans$zt ~ ., data.frame(zt, x[, 2:k])))
  ans$modelt <- modelt
  invisible(ans)
}