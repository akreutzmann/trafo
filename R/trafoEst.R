#' Estimation of optimal transformation parameter - lm
#' 
#' @param x matrix of regressors
#' @param y vector of response variables
#' @param lambdarange range for the estimation parameter expr(lambda) - default c(-2, 2)
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of \code{profile log-likelihood} at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector Employed family of transformations
#' @return A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
est_lm <- function(y, x , method, lambdarange, transfor, ...) {
  
  # Number of explanatory variables
  k <- ncol(x)
  
  # Get the optimal lambda via optimization on lambdarange
  res <- suppressWarnings(optimize(f = estim_lm, y = y, x = x, method = method,
                                   transfor = transfor, interval = lambdarange, 
                                   tol = 0.0001))
  
  if (is.infinite(res$objective)) {
    stop("For some lambda in the interval, the likelihood does not converge.
         Choose another lambdarange.")
  }
  
  # Optimal lambda and corresponding measure: likelihood, skewness or divergence
  lambdaoptim <-  res$minimum
  measoptim <- res$objective
  
  return(list(lambdaoptim = lambdaoptim, 
              measoptim = measoptim))
}  
  


#' Box Cox Estimation - lme
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
est_lme <- function(y, x, formula, data, rand_eff, method = method, 
                    lambdarange = lambdarange, transfor) {
  
  # Number of explanatory variables
  k <- ncol(x)

  # Get the optimal lambda via optimization on lambdarange
  res <- suppressWarnings(optimize(f = estim_lme, 
                                   y = y,
                                   formula = formula,
                                   data = data,
                                   rand_eff = rand_eff,
                                   method = method,
                                   transfor = transfor,
                                   interval = lambdarange, tol = 0.0001))
  
  if (is.infinite(res$objective)) {
    stop("For some lambda in the interval, the likelihood does not converge.
         Choose another lambdarange.")
  }
  
  # Optimal lambda and corresponding measure: likelihood, skewness or divergence
  lambdaoptim <-  res$minimum
  measoptim <- res$objective
  
  return(list(lambdaoptim = lambdaoptim, 
              measoptim = measoptim))
}







