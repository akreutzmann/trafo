#' Fits transformed linear models
#'
#' Function \code{trans_lm} fits linear models with transformed dependent 
#' variables. The return are two lm objects where the first is the transformed
#' and the second the untransformed linear model. 
#'
#' @param object an object of type lm or lme with the model to transform
#' @param trafo a character string. Different transformations can be used.
#' @param method a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter. 
#' (i) Maximum likelihood approaches: for linear models maximum likelihood ("ML")
#' and for linear mixed models restricted maximum likelihood ("reml"); 
#' (ii) Skewness minimizations: for linear models only skewness minimization 
#' ("skew") and for linear mixed models also pooled skewness minimization; 
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cm") or by Kullback-Leibler ("div.kl") for both 
#' model types. 
#' @param lambdarange a numeric vector with two elements defining an interval 
#' that is used for the estimation of the optimal transformation parameter. 
#' Defaults to \code{c(-2, 2)} for the Box-Cox transformation.
#' @return an object of class \code{transformation}
#' @keywords internal
#' @importFrom stats aggregate as.formula dnorm ecdf family lm logLik median 
#' model.frame model.matrix model.response na.omit optimize qchisq qnorm 
#' quantile residuals rstandard sd shapiro.test
#' @export

trafo_lm <- function(object, trafo, method, lambdarange, plotit = TRUE, 
                     std = FALSE){
  
  #if (trafo == log) {
  #  trans_mod <- bx_cx_new(object = object, method = method, 
  #                     lambdarange = lambdarange, plotit = plotit,...)
  #} else 
  if (trafo == "box.cox") {
    trans_mod <- bx_cx_new_lm(object = object, method = method, 
                       lambdarange = lambdarange)
  } else if (trafo == "bickel.doksum") {
    trans_mod <- bickeldoksum(object, method, lambdarange = lambdarange, ...)
  } else if (trafo == "manly") {
    trans_mod <- bickeldoksum(object, method, lambdarange = lambdarange, ...)
  } else if (trafo == "modulus") {
    trans_mod <- bickeldoksum(object, method, lambdarange = lambdarange, ...)
  } else if (trafo == "dual") {
    trans_mod <- bickeldoksum(object, method, lambdarange = lambdarange, ...)
  } else if (trafo == "yeo.johnson") {
    trans_mod <- bickeldoksum(object, method, lambdarange = lambdarange, ...)
  } 
  
  # Get original lm object
  orig_lm <- object 
  
  # Get transformed lm object
  if(std == FALSE) {
    model_frame <- object$model 
    x <- model.matrix(attr(model_frame, "terms"), data = model_frame)
    k <- ncol(x)
    suppressWarnings(modelt <- lm(yt ~ ., data.frame(yt = trans_mod$yt, x[, 2:k])))
  } else if (std == TRUE) {
    model_frame <- object$model 
    x <- model.matrix(attr(model_frame, "terms"), data = model_frame)
    k <- ncol(x)
    suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt = trans_mod$zt, x[, 2:k])))
  }
  
  trafo_lm <- modelt
  
  return(list(orig_lm = orig_lm,
              trafo_lm = trafo_lm, 
              trafo = trafo, 
              method = method, 
              lambdahat = trans_mod$lambdahat))
}


