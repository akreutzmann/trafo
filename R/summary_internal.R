summary_internal <- function(modOne, modTwo, compare, std) {
  
  formula <- NULL
  
  if (inherits(modOne, "lm") && std == TRUE) {
    
    # Summary of transformed model
    modTwo_sum <- summary(modTwo)
    modTwo_tmp <- summary(modTwo)
    modTwo$call <- as.symbol((paste(paste(deparse(modTwo$call), sep = "\n", 
                                             collapse = "\n"), 
                                       paste0("formula = ",modTwo$formula), 
                                       sep = "\n")))
    modTwo_sum$coefficients <- matrix(modTwo_sum$coefficients[, 1])
    colnames(modTwo_sum$coefficients) <- c("Estimate")
    rownames(modTwo_sum$coefficients) <- rownames(modTwo_tmp$coefficients)
    modTwo_sum$formula <- modTwo$formula
    modOne_sum <- summary(modOne)
    
    if (compare == TRUE) {
      modOne_sum <- summary(modOne)
      modOne_tmp <- summary(modOne)
      modOne$call <- as.symbol((paste(paste(deparse(modOne$call), sep = "\n", 
                                            collapse = "\n"), 
                                      paste0("formula = ",modOne$formula), 
                                      sep = "\n")))
      modOne_sum$coefficients <- matrix(modOne_sum$coefficients[, 1])
      colnames(modOne_sum$coefficients) <- c("Estimate")
      rownames(modOne_sum$coefficients) <- rownames(modOne_tmp$coefficients)
      modOne_sum$formula <- modOne$formula
      
    }
    
  } else if (inherits(modOne, "lm") && std == FALSE) {
  
    # Normal summary
    modOne_sum <- summary(modOne)
    modTwo_sum <- summary(modTwo)
    modTwo_sum$formula <- modTwo$formula
    
  }   
    
  if (inherits(modOne, "lme")) {
    
    object <- NULL
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
  
  return(list(modOne_sum = modOne_sum, 
              modTwo_sum = modTwo_sum))
  
  
  
  }