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


trafo_lm <- trafo_lm(object = modelVienna, trafo = "box.cox", 
         method = "ml", lambdarange = c(-2,2), plotit = TRUE, 
                     std = FALSE)


summary_trafolm <- function(object, ...) {
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  
  # Summary of original model
  orig_sum <- summary(object$orig_lm)
  
  
  # Summary of transformed model
  trafo_sum <- summary(object$trafo_lm) 
  trafo_sum$coefficients <- as.matrix(trafo_sum$coefficients[, 1])
  colnames(trafo_sum$coefficients) <- c("Estimate")
  
  return(list(trafo = trafo, 
              method = method, 
              lambdahat = lambdahat, 
              orig_sum = orig_sum, 
              trafo_sum = trafo_sum))
}



print_summary_trafolm <- function(x, ...) {
  
  cat("Information about applied transformation \n")
  cat("Transformation: ",x$trafo," \n")
  cat("Estimation method: ", x$method, " \n")
  cat("Optimal Parameter: ", x$lambdahat, " \n")
  
  cat("Summary of transformed model \n")
  print(x$trafo_sum)
  cat("Note that the standard errors are missing due to the lack of methods 
      for correct standard errors in transformed models. \n")
  
  cat("Summary of original model \n")
  print(x$orig_sum)
  
  invisible(x)
  
}

sum_trafo <- summary_trafolm(trafo_lm)

print_summary_trafolm(sum_trafo)


plot_trafolm <- function(x, ...) {
  
  cat("Plots of original model \n")
  plot(x$orig_lm)
  
  cat("Plots of transformed model \n")
  plot(x$trafo_lm)
  
  
}

plot_trafolm(trafo_lm)

# + argument estim or optim_l = "estim" or value of lambda
trans_mod <- bx_cx_new_lm(object = modelVienna, method = "ml", 
                          lambdarange = c(-2,2), plotit = TRUE)



# Trafo summary close to the one that exists at the moment but we need more
# diagnostics






