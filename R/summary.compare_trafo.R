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

summary.compare_trafo <- function(object, ...) {
  
  formula <- NULL
  
  trafos <- object$trafos
  method <- object$method
  lambdahat <- object$lambdahat
  
  
  if (inherits(object$trafoOne, "lm")) {
    # Summary of original model
    orig_sum <- summary(object$trafoOne)
    
    # Summary of first transformed model
    trafoOne_sum <- summary(object$trafoOne)
    trafoOne_sum$call <- as.symbol((paste(paste(deparse(object$trafoOne$call), sep = "\n", 
                                                collapse = "\n"), 
                                          paste0("formula = ",object$trafoOne$formula), 
                                          sep = "\n")))
    trafoOne_sum$coefficients <- matrix(trafoOne_sum$coefficients[, 1])
    colnames(trafoOne_sum$coefficients) <- c("Estimate")
    rownames(trafoOne_sum$coefficients) <- rownames(orig_sum$coefficients)
    
    # Summary of second transformed model
    trafoTwo_sum <- summary(object$trafoTwo)
    trafoTwo_sum$call <- as.symbol((paste(paste(deparse(object$trafoTwo$call), sep = "\n", 
                                      collapse = "\n"), 
                                paste0("formula = ",object$trafoTwo$formula), 
                                sep = "\n")))
    trafoTwo_sum$coefficients <- matrix(trafoTwo_sum$coefficients[, 1])
    colnames(trafoTwo_sum$coefficients) <- c("Estimate")
    rownames(trafoTwo_sum$coefficients) <- rownames(orig_sum$coefficients)
    
    
    residOne <- residuals(object$trafoOne, level = 0, type = "pearson")
    residTwo <- residuals(object$trafoTwo, level = 0, type = "pearson")
    
    if (length(residOne) > 3 & length(residOne) < 5000) {
      shapiroEst_residOne <- shapiro.test(residOne)$statistic[[1]]
      shapiroP_residOne <- shapiro.test(residOne)$p.value[[1]]
      
      shapiroEst_residTwo <- shapiro.test(residTwo)$statistic[[1]]
      shapiroP_residTwo <- shapiro.test(residTwo)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_residOne <- NA
      shapiroP_residOne <- NA
      
      shapiroEst_residTwo <- NA
      shapiroP_residTwo <- NA
    }
    
    skewness_residOne <- skewness(residOne)
    kurtosis_residOne <- kurtosis(residOne)
    
    skewness_residTwo <- skewness(residTwo)
    kurtosis_residTwo <- kurtosis(residTwo)
    
    norm_resid <- data.frame(Skewness  = c(skewness_residOne, skewness_residTwo),
                             Kurtosis  = c(kurtosis_residOne, kurtosis_residTwo),
                             Shapiro_W = c(shapiroEst_residOne, shapiroEst_residTwo),
                             Shapiro_p = c(shapiroP_residOne, shapiroP_residTwo),
                             row.names = object$trafos)
    
    norm_ranef <- NULL
    
    
    breusch_pagan_One <- bptest(formula(object$trafoOne$terms), 
                            data = object$trafoOne$model)
    breusch_pagan_Two <- bptest(formula(object$trafoTwo$terms), 
                                 data = object$trafoTwo$model)
    
    hetero <- data.frame(BreuschPagan_V = c(breusch_pagan_One$statistic,
                                            breusch_pagan_Two$statistic),
                         BreuschPagan_p = c(breusch_pagan_One$p.value,
                                            breusch_pagan_Two$p.value),
                         row.names = object$trafos)
    
    
    
    
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
  
  sum_out <- list(trafo = trafos, 
                  method = method, 
                  lambdahat = lambdahat, 
                  trafoOne_sum  = trafoOne_sum , 
                  trafoTwo_sum  = trafoTwo_sum,
                  norm_resid = norm_resid, 
                  norm_ranef = norm_ranef, 
                  hetero = hetero)
  
  class(sum_out) <- "summary.compare_trafo"
  
  return(sum_out)
}



#' Print summary trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{summary.trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @export

print.summary.compare_trafo <- function(x, ...) {
  
  
  cat("Applied transformations \n")
  cat("Transformations: ",x$trafo[[1]], "and",x$trafo[[2]],"\n")
  cat("Estimation methods: ", x$method[[1]], "and", x$method[[2]], " \n")
  cat("Optimal Parameters: ", x$lambdahat[[1]], "and", x$lambdahat[[2]]," \n")
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
  cat("Summary of model with ", x$trafo[[1]], "\n")
  print(x$trafoOne_sum)
  cat("\n")
  cat("Summary of model with ", x$trafo[[2]], "\n")
  print(x$trafoTwo_sum)
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




