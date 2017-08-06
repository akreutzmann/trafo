#' Summary transformations
#'
#' Computes objects to be shown in the summary function for objects of type \code{transformation}
#' @param x an object of type \code{transformation}
#' @return out an object of class \code{summary.transformation}
#' #@keywords internal
#' @export
summary.transformation <- function(object, ...) {
  #yt <- summary(x$modelt)$residuals
  
  out <- NULL
  
  # Residuals for the transformed and non transformed model
  residt <- residuals(object$modelt, level=0, type="pearson")
  resid <- residuals(object$model, level=0, type="pearson")
  
  # Sample size/length of residual vector
  nt <- length(residt)
  n <- length(resid)
  
  # Get vectors for the plots
  logvector  <- as.vector(object$logvector)
  lambdavector <- object$lambdavector
  out$lambdaoptim <- object$lambdahat
  
  out$logoptim <- object$optmeas
  lim <- out$logoptim + qchisq(0.95, 1)/2

  m <- length(logvector)
  index <- range((1L:m)[logvector < lim])
  out$cinf <- lambdavector[index[1]]
  out$csup <- lambdavector[index[2]]
  if (n > 3 & n < 5000) {
    out$shapiro_est_resid <- shapiro.test(resid)$statistic[[1]]
    out$shapiro_pvalue_resid <- shapiro.test(resid)$p.value[[1]]
    
    out$shapiro_estt_resid <- shapiro.test(residt)$statistic[[1]]
    out$shapiro_pvaluet_resid <- shapiro.test(residt)$p.value[[1]]
  }
  else{
    warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
    out$shapiro_est_resid <- NA
    out$shapiro_pvalue_resid <- NA
    
    out$shapiro_estt_resid <- NA
    out$shapiro_pvaluet_resid <- NA
  }
  
  out$skewness_resid <- skewness(resid)
  out$kurtosis_resid <- kurtosis(resid)
  
  out$skewnesst_resid <- skewness(residt)
  out$kurtosist_resid <- kurtosis(residt)
  
  # norm <- data.frame(Skewness  = c(skewness,skewnesst),
  #                    Kurtosis  = c(kurtosis, kurtosist),
  #                    Shapiro_W = c(shapiro_est,  shapiro_estt),
  #                    Shapiro_p = c(shapiro_pvalue, shapiro_pvaluet),
  #                    row.names = c("Untransformed", "Transformed")
  
  out$R2 <- summary(x$model)$r.squared
  out$R2t <- summary(x$modelt)$r.squared
  
  
  if(class(object$model) == "lme") {
    
    out$rand <- TRUE
    
    randt <- ranef(object$modelt)$'(Intercept)'
    rand <- ranef(object$model)$'(Intercept)'
    
    if (length(randt) > 3 & length(randt) < 5000) {
      out$shapiro_estt_rand <- shapiro.test(randt)$statistic[[1]]
      out$shapiro_pvaluet_rand <- shapiro.test(randt)$p.value[[1]]
      
      out$shapiro_est_rand <- shapiro.test(rand)$statistic[[1]]
      out$shapiro_pvalue_rand <- shapiro.test(rand)$p.value[[1]]
    }
    else{
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for random effects.")
      out$shapiro_estt_rand <- NA
      out$shapiro_pvaluet_rand <- NA
      
      out$shapiro_est_rand <- NA
      out$shapiro_pvalue_rand <- NA
    }
    
    out$skewnesst_rand <- skewness(randt)
    out$kurtosist_rand <- kurtosis(randt)
    
    
    
    out$skewness_rand <- skewness(rand)
    out$kurtosis_rand <- kurtosis(rand)
  } else {
    
    out$rand <- FALSE
    
    out$shapiro_estt_rand <- NULL
    out$shapiro_pvaluet_rand <- NULL
    
    out$skewnesst_rand <- NULL
    out$kurtosist_rand <- NULL
    
    out$shapiro_est_rand <- NULL
    out$shapiro_pvalue_rand <- NULL
    
    out$skewness_rand <- NULL
    out$kurtosis_rand <- NULL
    }
  
  out$method <- object$method
  out$family <- object$family
  
  class(out) <- "summary.transformation"
  out
}
# summary.list.transformation <- function(x, ...) {
#     yt <- summary(x$modelt)$residuals
#     n <- length(yt)
#     out <- x
#     logvector  <- as.vector(x$logvector)
#     lambdavector <- x$lambdavector
#     lambdaoptim <- x$lambdaoptim
#     logoptim <- x$llike
#     lim <- logoptim - qchisq(0.95, 1)/2
#     m <- length(logvector)
#     index <- range((1L:m)[logvector > lim])
#     out$cinf <- lambdavector[index[1]]
#     out$csup <- lambdavector[index[2]]
#     if (n < 5000) {
#       out$shapiro_est <- shapiro.test(yt)$statistic[[1]]
#       out$shapiro_pvalue <- shapiro.test(yt)$p.value[[1]]
#     }
#     else{
#       out$shapiro_est <-"n > 5000" 
#       out$shapiro_pvalue <- "NA"
#     }
#     out$skewness <- skewness(yt)
#     out$kurtosis <- kurtosis(yt)
#     class(out) <- "summary.transformation"
#     out
# }


#' Print summary transformations
#'
#' prints objects to be shown in the summary function for objects of type \code{transformation}
#' @param x an object of type \code{transformation}
#' @keywords internal
#' @export
print.summary.transformation <- function(x, ...) {
  cat(x$family, "Transformation \n")
  #cat("\n Tranformation family:", x$family, "\n")
  #cat("Lambda hat:", x$lambdahat, "\n")
  #cat("Log-Likelihood:", x$llike, "\n")
  cat("\nlambdahat: ", x$lambdaoptim)
  if (x$method == "ml" | x$method == "reml") {
    cat("\nloglike: ",-x$logoptim,"\n") 
  } else if (x$method == "skew" | x$method == "pskew" ) {
    cat("\nskewness: ",x$logoptim,"\n")
  } else if (x$method == "div.ks" | x$method == "div.cvm" | 
             x$method == "div.kl") {
    cat("\ndivergence: ", x$logoptim,"\n")
  }  
  cat("95% LR Confidence Region for lambda hat: [" ,x$cinf, ",", x$csup ,"] (df=1)\n")
  cat("R.Squared:", x$R2t, "\n")
  cat("\n \t Statistics on transformed variable \n")
  cat("\n")
  cat("Shapiro-Wilk normality test: \n")
  cat("Residuals \n")
  cat("W = ",   x$shapiro_estt_resid, "\n")
  cat("p-value = ",   x$shapiro_pvaluet_resid, "\n")
  if(x$rand){
  cat("Random effect \n")
  cat("W = ",   x$shapiro_estt_rand, "\n")
  cat("p-value = ",   x$shapiro_pvaluet_rand, "\n")  
  }
  cat("\n")
  cat("Third and fourth moment: \n")
  cat("Residuals \n")
  cat("Skewness: ", x$skewnesst_resid, "\n")
  cat("Kurtosis: ", x$kurtosist_resid, "\n")
  if(x$rand){
    cat("Random effect \n")
    cat("Skewness: ", x$skewnesst_rand, "\n")
    cat("Kurtosis: ", x$kurtosist_rand, "\n") 
  }
}
