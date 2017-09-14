#' Fits transformed linear mixed models
#'
#' Function \code{trans_lme} fits linear mixed models with one random effect 
#' and transformed dependent variable. The return are two lme objects where the 
#' first is the transformed and the second the untransformed linear mixed model. 
#'
#' @param object an object of type lm or lme with the model to transform
#' @param trafo a character string. Different transformations can be used.
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter.
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Restricted maximum likelihood approach ("reml"), 
#' (ii) Skewness minimization ("skew") and pooled skewness minimization ("pskew"), 
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' @param std logical. If TRUE, the transformed model is returned based on the 
#' standardized transformation.
#' @param custom_trafo a list that determines a one parameter transformation and
#' the standardized one parameter transformation.
#' @return an object of class \code{trafo_mod}.
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @import nlme
#' @keywords internal

trafo_lme <- function(object, trafo = "boxcox", lambda = "estim", 
                      method = "reml", lambdarange = c(-2, 2), 
                      std = FALSE, custom_trafo = NULL){
  
  plotit <- FALSE
  
  check_trafomod_lme(object = object, std = std, custom_trafo = custom_trafo)

  
  if (trafo %in% c("bickeldoksum", "boxcox", "dual", "gpower", "manly", 
                   "modulus", "logshiftopt", "sqrtshift", "yeojohnonson")) {
    trans_mod <- oneparam(object = object, trafo = trafo, lambda = lambda, 
                          method = method, lambdarange = lambdarange, 
                          plotit = plotit)
  } else if (trafo %in% c("reciprocal", "neglog", "glog")) {
    trans_mod <- woparam(object = object, trafo = trafo,
                         custom_trafo = custom_trafo)
  } else if (trafo == "custom" && length(custom_trafo) == 2) {
    trans_mod <- oneparam(object = object, trafo = trafo, lambda = lambda, 
                          method = method, lambdarange = lambdarange, 
                          plotit = plotit, custom_trafo = custom_trafo)
  } else if (trafo == "custom" && length(custom_trafo) == 1) {
    trans_mod <- woparam(object = object, trafo = trafo,
                         custom_trafo = custom_trafo)
  }
  
  # Get original lm object
  orig_mod <- object 
  
  # Get transformed lm object
  trafo_mod <- get_modelt(object = object, trans_mod = trans_mod, std = std)
  
  # Return new class
  trafo_out <- list(orig_mod = orig_mod,
                    trafo_mod = trafo_mod, 
                    trafo = trafo, 
                    method = method, 
                    lambdahat = trans_mod$lambdahat, 
                    std = std)
  
  if (inherits(trans_mod, "woparam")) {
    class(trafo_out) <- c("trafo_mod", "woparam")
  } else if (inherits(trans_mod, "oneparam")) {
    class(trafo_out) <- c("trafo_mod", "oneparam")
  }
  
  return(trafo_out)
}


