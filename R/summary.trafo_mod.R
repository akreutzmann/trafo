#' Summary for models with untransformed and transformed dependent variable
#'
#' Information about the transformed data and model and components of an 
#' transformation object are extracted. The returned object is suitable for 
#' printing with the print.summary.transformation method.
#' 
#' @param object an object of type \code{trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @return an object of class \code{summary.transformation}
#' @export

summary.trafo_mod <- function(object, ...) {
  
  formula <- NULL
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  
  
  if (inherits(object$orig_mod, "lm")) {
    # Summary of original model
    orig_sum <- summary(object$orig_mod)
    
    # Summary of transformed model
    trafo_sum <- summary(object$trafo_mod)
    trafo_sum$call <- as.symbol((paste(paste(deparse(object$trafo_mod$call), sep = "\n", 
                                      collapse = "\n"), 
                                paste0("formula = ",object$trafo_mod$formula), 
                                sep = "\n")))
    trafo_sum$coefficients <- matrix(trafo_sum$coefficients[, 1])
    colnames(trafo_sum$coefficients) <- c("Estimate")
    rownames(trafo_sum$coefficients) <- rownames(orig_sum$coefficients)
    
    
    resid <- residuals(object$orig_mod, level = 0, type = "pearson")
    residt <- residuals(object$trafo_mod, level = 0, type = "pearson")
    
    if (length(residt) > 3 & length(residt) < 5000) {
      shapiroEst_residt <- shapiro.test(residt)$statistic[[1]]
      shapiroP_residt <- shapiro.test(residt)$p.value[[1]]
      
      shapiroEst_resid <- shapiro.test(resid)$statistic[[1]]
      shapiroP_resid <- shapiro.test(resid)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_residt <- NA
      shapiroP_residt <- NA
      
      shapiroEst_resid <- NA
      shapiroP_resid <- NA
    }
    
    skewness_residt <- skewness(residt)
    kurtosis_residt <- kurtosis(residt)
    
    skewness_resid <- skewness(resid)
    kurtosis_resid <- kurtosis(resid)
    
    norm_resid <- data.frame(Skewness  = c(skewness_resid, skewness_residt),
                             Kurtosis  = c(kurtosis_resid, kurtosis_residt),
                             Shapiro_W = c(shapiroEst_resid, shapiroEst_residt),
                             Shapiro_p = c(shapiroP_resid, shapiroP_residt),
                             row.names = c("Untransformed model", 
                                           "Transformed model"))
    
    norm_ranef <- NULL
    
    
    breusch_pagan_orig <- bptest(formula(object$orig_mod$terms), 
                            data = object$orig_mod$model)
    breusch_pagan_trafo <- bptest(formula(object$trafo_mod$terms), 
                                 data = object$trafo_mod$model)
    
    hetero <- data.frame(BreuschPagan_V = c(breusch_pagan_orig$statistic,
                                            breusch_pagan_trafo$statistic),
                         BreuschPagan_p = c(breusch_pagan_orig$p.value,
                                            breusch_pagan_trafo$p.value),
                         row.names = c("Untransformed model", 
                                       "Transformed model"))
    
    
    
    
  } else if (inherits(object$orig_mod, "lme")) {
    # Summary of original model
    orig_sum <- summary(object$orig_mod)
    
    
    # Summary of transformed model
    trafo_sum <- summary(object$trafo_mod) 
    trafo_sum$tTable <- as.matrix(trafo_sum$tTable[, c(1, 5)])
    colnames(trafo_sum$tTable) <- c("Value", "p-value")
    
    resid <- residuals(object$orig_mod, level = 0, type = "pearson")
    residt <- residuals(object$trafo_mod, level = 0, type = "pearson")
    
    if (length(residt) > 3 & length(residt) < 5000) {
      shapiroEst_residt <- shapiro.test(residt)$statistic[[1]]
      shapiroP_residt <- shapiro.test(residt)$p.value[[1]]
      
      shapiroEst_resid <- shapiro.test(resid)$statistic[[1]]
      shapiroP_resid <- shapiro.test(resid)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_residt <- NA
      shapiroP_residt <- NA
      
      shapiroEst_resid <- NA
      shapiroP_resid <- NA
    }
    
    skewness_residt <- skewness(residt)
    kurtosis_residt <- kurtosis(residt)
    
    skewness_resid <- skewness(resid)
    kurtosis_resid <- kurtosis(resid)
    
    
    raneft <- ranef(object$trafo_mod)$'(Intercept)'
    ranefo <- ranef(object$orig_mod)$'(Intercept)'
    
    if (length(raneft) > 3 & length(raneft) < 5000) {
      shapiroEst_raneft <- shapiro.test(raneft)$statistic[[1]]
      shapiroP_raneft <- shapiro.test(raneft)$p.value[[1]]
      
      shapiroEst_ranef <- shapiro.test(ranefo)$statistic[[1]]
      shapiroP_ranef <- shapiro.test(ranefo)$p.value[[1]]
    }
    else{
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for random effects.")
      shapiroEst_raneft <- NA
      shapiroP_raneft <- NA
      
      shapiroEst_ranef <- NA
      shapiroP_ranef <- NA
    }
    
    skewness_raneft <- skewness(raneft)
    kurtosis_raneft <- kurtosis(raneft)
    
    skewness_ranef <- skewness(ranefo)
    kurtosis_ranef <- kurtosis(ranefo)
    
    norm_resid <- data.frame(Skewness  = c(skewness_resid, skewness_residt),
                             Kurtosis  = c(kurtosis_resid, kurtosis_residt),
                             Shapiro_W = c(shapiroEst_resid, shapiroEst_residt),
                             Shapiro_p = c(shapiroP_resid, shapiroP_residt),
                             row.names = c("Untransformed model", 
                                           "Transformed model"))
    
    norm_ranef <- data.frame(Skewness  = c(skewness_ranef, skewness_raneft),
                             Kurtosis  = c(kurtosis_ranef, kurtosis_raneft),
                             Shapiro_W = c(shapiroEst_ranef, shapiroEst_raneft),
                             Shapiro_p = c(shapiroP_ranef, shapiroP_raneft),
                             row.names = c("Untransformed model", 
                                           "Transformed model"))
    
    breusch_pagan_orig <- bptest(formula(object$orig_mod$terms), 
                                 data = object$orig_mod$data)
    
    breusch_pagan_trafo <- bptest(formula(object$trafo_mod$terms), 
                                 data = object$trafo_mod$data)
    
    hetero <- data.frame(BreuschPagan_V = c(breusch_pagan_orig$statistic,
                                            breusch_pagan_trafo$statistic),
                         BreuschPagan_p = c(breusch_pagan_orig$p.value,
                                            breusch_pagan_trafo$p.value),
                         row.names = c("Untransformed model", 
                                       "Transformed model"))
    
  }
  
  sum_out <- list(trafo = trafo, 
                  method = method, 
                  lambdahat = lambdahat, 
                  orig_sum = orig_sum, 
                  trafo_sum = trafo_sum,
                  norm_resid = norm_resid, 
                  norm_ranef = norm_ranef, 
                  hetero = hetero)
  
  class(sum_out) <- "summary.trafo_mod"
  
  return(sum_out)
}



#' Print summary trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{summary.trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @export

print.summary.trafo_mod <- function(x, ...) {
  
  
  cat("Applied transformation \n")
  cat("Transformation: ",x$trafo," \n")
  if (x$trafo != "log") {
    cat("Estimation method: ", x$method, " \n")
  }
  cat("Optimal Parameter: ", x$lambdahat, " \n")
  cat("\n")
  cat("Residual diagnostics:\n")
  
  cat("\n")
  cat("Normality:\n")
  cat("Pearson residuals:\n")
  print(x$norm_resid)
  if (!is.null(x$norm_ranef)) {
    cat("Standardized random effects:\n")
    print(x$norm_ranef) 
  }
  cat("\n")
  cat("Heteroscedasticity:\n")
  print(x$hetero)
  cat("\n")
  cat("Summary of transformed model \n")
  print(x$trafo_sum)
  cat("\n")
  cat("Note that the standard errors are missing due to the lack of methods 
      for correct standard errors in transformed models. \n")
  cat("\n")
  #cat("Press [enter] to compare with untransformed model or type in [q] to quit" )
  #line <- readline()
  #if (substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q") {
  #  cat("Summary of original model \n")
  #  print(x$orig_sum) 
  #}
  invisible(x)
}




