#' Fits transformed linear models
#'
#' Function \code{trafo_lm} fits linear models with transformed dependent 
#' variable. The main return are two lm objects where one is the untransformed
#' linear model and the other one the transformed linear model. 
#'
#' @param object an object of type \code{lm}. 
#' @param trafo a character string. Different transformations can be used 
#' for transforming the dependent variable in a linear model: 
#' (i)  "bickeldoksum", (ii) "boxcox", (iii) "dual", (iv) "glog", (v) "gpower", 
#' (vi) "log", (vii) "logshiftopt", (viii) "manly", (ix) "modulus", (x) "neglog",
#' (xi) "reciprocal", (xii) "yeojohnson".
#' Defaults to "boxcox".
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter. Defaults to "estim".
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),  
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' Defaults to "ml".
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to c(-2, 2).
#' @param std logical. If TRUE, the transformed model is returned based on the 
#' standardized transformation. 
#' @param custom_trafo a list. If the customized transformation does 
#' not contain a transformation parameter the list has one element that is a 
#' function specifying the desired transformation. If the customized 
#' transformation contains a transformation parameter the list has two elements
#' where the first element is a function specifying the desired transformation
#' and the second element is a function specifying the corresponding standardized
#' transformation. Defaults to \code{NULL}.
#' @return an object of class \code{trafo_mod}.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Compare transformed models
#' trafo_lm(object = lm_cars, trafo = "bickeldoksum", method = "skew", 
#' lambdarange = c(1e-11, 2))
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export

trafo_lm <- function(object, trafo = "boxcox", lambda = "estim", method = "ml", 
                     lambdarange = c(-2, 2), std = FALSE, 
                     custom_trafo = NULL) {
 
  check_trafomod_lm(object = object, std = std, custom_trafo = custom_trafo)
  
  plotit <- FALSE
  
  if (trafo %in% c("bickeldoksum", "boxcox", "dual", "gpower", "manly", 
                   "modulus", "logshiftopt", "sqrtshift", "yeojohnonson")) {
    trans_mod <- oneparam(object = object, trafo = trafo, lambda = lambda, 
                          method = method, lambdarange = lambdarange, 
                          plotit = plotit)
  } else if (trafo %in% c("log", "reciprocal", "neglog", "glog")) {
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


