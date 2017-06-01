#' Box Cox Estimation - lm
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
est_lm <- function(y, x , method, lambdarange, tr = FALSE, ...) {
  k <- ncol(x)
  # get the result of the optimation
  res <- suppressWarnings(optimize(f = estim_lm, y = y, x = x, method = method, 
                                   interval = lambdarange, tol = 0.0001))
  lambdaoptim <-  res$minimum
  logoptim <- res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  
  
  # !!! add other methods:wrapper for method
  logvector <- sapply(lambdavector, ML, y = y, x = x)
  
  
  # Here Box cox function aufrufen bzw. wrapper für trafo
  if (abs(lambdaoptim) > 0.05)  
    yt <- (y^lambdaoptim - 1)/lambdaoptim
  else 
    yt <- log(y) 
  
  # ??? Ist dies spezifisch für ML?? Und wenn nicht, dann wie können wir dies 
  # übertragen auf lme?
  zt <- yt/exp((lambdaoptim - 1)*mean(log(y)))
  suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))
  
  ans <- list()
  
  # here only llike considered but we have different functions that are optimized
  # change name! 
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE) 
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Box-Cox"
  ans$yt <- yt
  ans$zt <- zt
  ans$modelt <- modelt
  ans$method <- method
  # Do we want to change to class trafo??!!
  class(ans) <- "transformation"
  ans
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
est_lme <- function( y, x, formula, data, rand_eff, method = method, lambdarange = lambdarange, tr = FALSE, ...) {
  #x <- model.matrix(formula, data = data)
  k <- ncol(x)
  # get the result of the optimation
  res <- suppressWarnings(optimize(f = estim_lme, 
                                   y = y,
                                   #x = x,
                                   formula = formula,
                                   data = data,
                                   rand_eff = rand_eff,
                                   method = method, 
                                   interval = lambdarange, tol = 0.0001))
  lambdaoptim <-  res$minimum
  logoptim <- res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  
  # Wrapper für methods
  #logvector <- sapply(lambdavector, ML, y = y, x = x)
  
  
  # Take Box Coox or rather wrapper für trafo
  if (abs(lambdaoptim) > 0.05)  
    yt <- (y^lambdaoptim - 1)/lambdaoptim
  else 
    yt <- log(y) 
  
  # ??? 
  zt <- yt/exp((lambdaoptim - 1)*mean(log(y)))
  #suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))
  
  ans <- list()
  # Check names
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE) 
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  #ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Box-Cox"
  ans$yt <- yt
  ans$zt <- zt
  #ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}






