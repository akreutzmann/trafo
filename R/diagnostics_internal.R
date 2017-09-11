diagnostics_internal <- function(modOne, modTwo) {
  
  
  formula <- NULL
  
  if (inherits(modOne, "lm")) {
    
    resid_One <- residuals(modOne, level = 0, type = "pearson")
    resid_Two <- residuals(modTwo, level = 0, type = "pearson")
    
    if (length(resid_One) > 3 & length(resid_One) < 5000) {
      
      # Original model or first transformed
      shapiroEst_One <- shapiro.test(resid_One)$statistic[[1]]
      shapiroP_One <- shapiro.test(resid_One)$p.value[[1]]
      
      # Transformed model or second transformed
      shapiroEst_Two <- shapiro.test(resid_Two)$statistic[[1]]
      shapiroP_Two <- shapiro.test(resid_Two)$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_One <- NA
      shapiroP_One <- NA
      
      shapiroEst_Two <- NA
      shapiroP_Two <- NA
    }
    
    skewness_One <- skewness(resid_One)
    kurtosis_One <- kurtosis(resid_One)
    
    skewness_Two <- skewness(resid_Two)
    kurtosis_Two <- kurtosis(resid_Two)
    
    norm_resid <- data.frame(Skewness  = c(skewness_One, skewness_Two),
                             Kurtosis  = c(kurtosis_One, kurtosis_Two),
                             Shapiro_W = c(shapiroEst_One, shapiroEst_Two),
                             Shapiro_p = c(shapiroP_One, shapiroP_Two),
                             row.names = c(modOne$name, 
                                           modTwo$name))
    
    norm_ranef <- NULL
    
    
    breusch_pagan_One <- bptest(formula(modOne$terms), 
                                 data = modOne$model)
    breusch_pagan_Two <- bptest(formula(modTwo$terms), 
                                  data = modTwo$model)
    
    hetero <- data.frame(BreuschPagan_V = c(breusch_pagan_One$statistic,
                                            breusch_pagan_Two$statistic),
                         BreuschPagan_p = c(breusch_pagan_One$p.value,
                                            breusch_pagan_Two$p.value),
                         row.names = c(modOne$name, 
                                       modTwo$name))
    
    
    
    
  } else if (inherits(modOne, "lme")) {
    
    resid <- residuals(modOne, level = 0, type = "pearson")
    residt <- residuals(modTwo, level = 0, type = "pearson")
    
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
    
    
    raneft <- ranef(modOne)$'(Intercept)'
    ranefo <- ranef(modTwo)$'(Intercept)'
    
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
    
  }
  
  
  return(list(norm_resid = norm_resid, 
              norm_ranef = norm_ranef, 
              hetero = hetero))
  
  
}