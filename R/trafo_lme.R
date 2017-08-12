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
#' @param plotit logical. If TRUE, a plot that illustrates the optimal 
#' transformation parameter or the given transformation parameter is returned.
#' @param std logical. If TRUE, the transformed model is returned based on the 
#' standardized transformation.
#' @return an object of class \code{trafo_mod}.
#' @examples
#' # Load data
#' data("eusilcA_Vienna")
#' 
#' # Fit linear mixed model
#' require(nlme)
#' lme_Vienna <- lme(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
#' rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + fam_allow + 
#' house_allow, random = ~ 1 | county, data = eusilcA_Vienna, 
#' na.action = na.omit)
#' 
#' # Get linear model with untransformed and transformed model
#' trafo_lme(object = lme_Vienna, trafo = "box.cox", method = "reml", 
#' lambdarange = c(0,2), plotit = TRUE, std = TRUE)
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @import nlme
#' @export

trafo_lme <- function(object, trafo, lambda = "estim", method, 
                     lambdarange, plotit = TRUE, std = FALSE){
  
  
  
  if (trafo == "log") {
    trans_mod <- boxcox(object = object, lambda = 0, method = method, 
                       lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "box.cox") {
    trans_mod <- boxcox(object = object, lambda = lambda, method = method, 
                       lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "bickel.doksum") {
    trans_mod <- bickeldoksum(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "manly") {
    trans_mod <- manly(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "modulus") {
    trans_mod <- modulus(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "dual") {
    trans_mod <- dual(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "yeo.johnson") {
    trans_mod <- yeojohnson(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
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
                    lambdahat = trans_mod$lambdahat)
  
  class(trafo_out) <- "trafo_mod"
  
  return(trafo_out)
}


