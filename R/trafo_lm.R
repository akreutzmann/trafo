#' Fits transformed linear models
#'
#' Function \code{trafo_lm} fits linear models with transformed dependent 
#' variable. The return are two lm objects where the first is the transformed
#' and the second the untransformed linear model. 
#'
#' @param object an object of type lm or lme with the model to transform
#' @param trafo a character string. Different transformations can be used.
#' @param lambda either a character named "estim" if the optimal transformation
#' parameter should be estimated or a numeric value determining a given 
#' transformation parameter.
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),  
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl").
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
# #' @param plotit logical. If TRUE, a plot that illustrates the optimal 
# #' transformation parameter or the given transformation parameter is returned.
#' @param std logical. If TRUE, the transformed model is returned based on the 
#' standardized transformation.
#' @param custom_trafo a function that specifies a transformation without 
#' transformation parameter that needs to be estimated or given.
#' @return an object of class \code{trafo_mod}.
#' @examples
#' # Load data
#' data("eusilcA_Vienna")
#' 
#' # Fit linear model
#' lm_Vienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben + 
#' rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
#' fam_allow + house_allow, data = eusilcA_Vienna)
#' 
#' # Get linear model with untransformed and transformed model
#' trafo_lm(object = lm_Vienna, trafo = "boxcox", method = "ml", 
#' lambdarange = c(0,2), std = FALSE)
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export

trafo_lm <- function(object, trafo, lambda = "estim", method, 
                     lambdarange, std = FALSE, 
                     custom_trafo){
  
  
  plotit <- FALSE
  
  if (trafo == "log") {
    trans_mod <- boxcox(object = object, lambda = 0, method = method, 
                       lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "boxcox") {
    trans_mod <- boxcox(object = object, lambda = lambda, method = method, 
                       lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "bickeldoksum") {
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
  } else if (trafo == "yeojohnson") {
    trans_mod <- yeojohnson(object = object, lambda = lambda, method = method, 
                              lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "logshiftopt") {
    trans_mod <- logshiftopt(object = object, lambda = lambda, method = method, 
                            lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "sqrtshift") {
    trans_mod <- sqrtshift(object = object, lambda = lambda, method = method, 
                             lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "gpower") {
    trans_mod <- gpower(object = object, lambda = lambda, method = method, 
                           lambdarange = lambdarange, plotit = plotit)
  } else if (trafo == "reciprocal") {
    trans_mod <- simple_trafo(object = object, trafo = trafo)
  } else if (trafo == "neglog") {
    trans_mod <- simple_trafo(object = object, trafo = trafo)
  } else if (trafo == "custom") {
    trans_mod <- simple_trafo(object = object, trafo = trafo, 
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
                    lambdahat = trans_mod$lambdahat)
  
  class(trafo_out) <- "trafo_mod"
  
  return(trafo_out)
}


