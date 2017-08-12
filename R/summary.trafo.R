#' Summary transformations
#'
#' Information about the transformed data and model and components of an 
#' transformation object are extracted. The returned object is suitable for 
#' printing with the print.summary.transformation method.
#' 
#' @param x an object of type \code{transformation}
#' @param ... additional arguments that are not used in this method
#' @return an object of class \code{summary.transformation}
#' @keywords internal
#' @importFrom moments skewness kurtosis
#' @export

summary.trafo <- function(object, ...) {
  #yt <- summary(x$modelt)$residuals
  
  out <- NULL
  
  # Residuals for the transformed and non transformed model
  residt <- residuals(object$modelt, level = 0, type = "pearson")
  
  if (length(residt) > 3 & length(residt) < 5000) {
    shapiroEst_residt <- shapiro.test(residt)$statistic[[1]]
    shapiroP_residt <- shapiro.test(residt)$p.value[[1]]
  }
  else{
    warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
    
    shapiroEst_residt <- NA
    shapiroP_residt <- NA
  }
  
  skewness_residt <- skewness(residt)
  kurtosis_residt <- kurtosis(residt)
  
  norm <- data.frame(Skewness  = c(skewness_residt),
                     Kurtosis  = c(kurtosis_residt),
                     Shapiro_W = c(shapiroEst_residt),
                     Shapiro_p = c(shapiroP_residt),
                     row.names = c("Residuals"))
  
  #out$R2t <- summary(object$modelt)$r.squared
  
  
  if (inherits(object$modelt, "lme")) {
    
    raneft <- ranef(object$modelt)$'(Intercept)'
    
    if (length(raneft) > 3 & length(raneft) < 5000) {
      shapiroEst_raneft <- shapiro.test(raneft)$statistic[[1]]
      shapiroP_raneft <- shapiro.test(raneft)$p.value[[1]]
    }
    else{
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for random effects.")
      shapiroEst_raneft <- NA
      shapiroP_raneft <- NA
    }
    
    skewness_raneft <- skewness(raneft)
    kurtosis_raneft <- kurtosis(raneft)
    
    
    norm <- data.frame(Skewness  = c(skewness_residt, skewness_raneft),
                       Kurtosis  = c(kurtosis_residt, kurtosis_raneft),
                       Shapiro_W = c(shapiroEst_residt, shapiroEst_raneft),
                       Shapiro_p = c(shapiroP_residt, shapiroP_raneft),
                       row.names = c("Residuals", "Random effect"))
    
  }
  
  out <- list(family = object$family,
              method = object$method,
              lambdahat = object$lambdahat, 
              measoptim = object$measoptim, 
              normality = norm)

  class(out) <- "summary.trafo"
  out
}


#' Print summary transformations
#'
#' prints objects to be shown in the summary function for objects of type \code{transformation}
#' @param x an object of type \code{transformation}
#' @keywords internal
#' @export
print.summary.trafo <- function(x, ...) {
  cat(x$family, "Transformation \n")
  cat("Estimation method: ", x$method, " \n")
  cat("Optimal parameter: ", x$lambdahat, " \n")
  if (x$method == "ml" | x$method == "reml") {
    cat("Loglike: ",x$measoptim,"\n") 
  } else if (x$method == "skew" | x$method == "pskew" ) {
    cat("Skewness: ",x$measoptim,"\n")
  } else if (x$method == "div.ks" | x$method == "div.cvm" | 
             x$method == "div.kl") {
    cat("Divergence: ", x$measoptim,"\n")
  }  
  cat("\n")
  cat("Residual diagnostics:\n")
  
  cat("\n")
  cat("Normality:\n")
  print(x$normality)
}
