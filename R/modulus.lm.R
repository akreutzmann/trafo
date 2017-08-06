#' Modulus transformation for linear and linear mixed models
#'
#' Depending on the class of the first object, this function estimates the 
#' optimal transformation parameter for the Modulus transformation for 
#' the model given to the function.
#'
#' @param object an object of type lm or lme with the model to transform
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
#' Defaults to \code{c(-2, 2)} for the Modulus transformation.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{transformation}
#' @keywords internal
#' @export
modulus.lm <- function(object, method, lambdarange = c(-2, 2), ...) {
  model_frame <- object$model 
  if (is.null(y <- model.response(model_frame))) 
    stop("Dependent variable y must not be empty")
  if (is.null(x <- model.matrix(attr(model_frame, "terms"), data = model_frame))) 
    stop("Matrix of covariates X must not be empty")
  # modulusEst(y, x, ...)
  est_modulus <- est_lm(y = y, x = x, transfor = "t_mdls", method = method, 
         lambdarange = lambdarange, tol = 0.0001)  
  est_modulus$model <- object
  est_modulus
}