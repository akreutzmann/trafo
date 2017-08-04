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
est_lm <- function(y, x , method, lambdarange, tr = FALSE, transfor, ...) {
  k <- ncol(x)
  
  # get the result of the optimization
  # when you have the wrappers for the transformation in ML this function needs 
  # a transformation argument
  res <- suppressWarnings(optimize(f = estim_lm, y = y, x = x, method = method,
                                   transfor = transfor, interval = lambdarange, tol = 0.0001))
  lambdaoptim <-  res$minimum
  logoptim <- res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  
  # not done yet!
  # wrapper for other estimation methods, here you must be careful of the log-
  # likelihood is negative or positive, we needed the negative log likelihood for
  # the estimation but I guess that we need the positive values now, so whenever
  # the estimation method is ML we probably need -logvector 
  
  logvector <- sapply(lambdavector, ML, y = y, x = x, transfor = transfor)
  
  yt <- if(transfor == "t_bx_cx") {
    box_cox(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mdls") {
    modul(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_bck_dk") {
    Bick_dok(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mnl") {
    Manly(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_dl") {
    Dual(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_y_jhnsn") {
    Yeo_john(y = y, lambda = lambdaoptim)
  }
  
  zt <- if(transfor == "t_bx_cx") {
    box_cox_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mdls") {
    modul_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_bck_dk") {
    Bick_dok_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mnl") {
    Manly_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_dl") {
    Dual_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_y_jhnsn") {
    Yeo_john_std(y = y, lambda = lambdaoptim)
  }
  # Here wrapper for transformations
  # zt <- box_cox(y = y, lambda = lambdaoptim)$y # warum nicht standardizierte Daten?
  # zt <- box_cox_std(y = y, lambda = lambdaoptim)
  suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))
  
  family <- if(transfor == "t_bx_cx") {
    "Box-Cox"
  } else if (transfor == "t_mdls") {
    "Modulus"
  } else if (transfor == "t_bck_dk") {
    "Bickel-Doksum"
  } else if (transfor == "t_mnl") {
    "Manly"
  } else if (transfor == "t_dl") {
    "Dual"
  } else if (transfor == "t_y_jhnsn") {
    "Yeo-Johnson"
  }
  
  ans <- list()
  
  # here only llike considered but we have different functions that are optimized
  # change name! 
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE) 
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- family
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
est_lme <- function( y, x, formula, data, rand_eff, method = method, lambdarange = lambdarange, tr = FALSE, transfor, ...) {
  #x <- model.matrix(formula, data = data)
  k <- ncol(x)
  # get the result of the optimization
  # when you have the wrappers for the transformation in restricted_ML this 
  # function needs a transformation argument (Done!)
  res <- suppressWarnings(optimize(f = estim_lme, 
                                   y = y,
                                   #x = x,
                                   formula = formula,
                                   data = data,
                                   rand_eff = rand_eff,
                                   method = method,
                                   transfor = transfor,
                                   interval = lambdarange, tol = 0.0001))
  lambdaoptim <-  res$minimum
  logoptim <- res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  
  # wrapper for other estimation methods, here you must be careful of the log-
  # likelihood is negative or positive, we needed the negative log likelihood for
  # the estimation but I guess that we need the positive values now, so whenever
  # the estimation method is restricted_ML we probably need -logvector 
  # logvector <- sapply(lambdavector, ML, y = y, x = x, transfor = transfor)
  logvector <- if(method == "reml") {
    sapply(lambdavector, restricted_ML, y = y, formula = formula, 
           data = data, rand_eff = rand_eff, transfor = transfor)
  } else if (method == "skew") {
    1 # sapply()
  } else if (method == "pskew") {
    1
  } else if (method == "div.ks") {
    1
  } else if (method == "div.cvm") {
    1
  } else if (method == "div.kl") {
    1
  }
  
  # Take Box Coox or rather wrapper für trafo
  # if (abs(lambdaoptim) > 0.05)  
  #   yt <- (y^lambdaoptim - 1)/lambdaoptim
  # else 
  #   yt <- log(y) 
  yt <- if(transfor == "t_bx_cx") {
    box_cox(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mdls") {
    modul(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_bck_dk") {
    Bick_dok(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mnl") {
    Manly(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_dl") {
    Dual(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_y_jhnsn") {
    Yeo_john(y = y, lambda = lambdaoptim)
  }
  
  # ??? 
  zt <- if(transfor == "t_bx_cx") {
    box_cox_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mdls") {
    modul_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_bck_dk") {
    Bick_dok_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_mnl") {
    Manly_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_dl") {
    Dual_std(y = y, lambda = lambdaoptim)
  } else if (transfor == "t_y_jhnsn") {
    Yeo_john_std(y = y, lambda = lambdaoptim)
  }
  
  # Here wrapper for transformations
  # zt <- box_cox(y = y, lambda = lambdaoptim)$y
  # suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))
  
  
  ans <- list()
  # Check names
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE) 
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  #ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  
  ans$family <- if(transfor == "t_bx_cx") {
    "Box-Cox"
  } else if (transfor == "t_mdls") {
    "Modulus"
  } else if (transfor == "t_bck_dk") {
    "Bickel-Doksum"
  } else if (transfor == "t_mnl") {
    "Manly"
  } else if (transfor == "t_dl") {
    "Dual"
  } else if (transfor == "t_y_jhnsn") {
    "Yeo-Johnson"
  }
  ans$yt <- yt
  ans$zt <- zt
  # ans$modelt <- modelt
  class(ans) <- "transformation"
  ans
}






