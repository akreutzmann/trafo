#' Selects Transformation
#'
#' Chooses the 'best' transformation in the sense of AIC, R.Squared, Skewness or Kurtosis; For positive \code{y} compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum  
#' @param object an optional data frame, list or environment as in lm.
#' @param criterion a character string to select the desired criterion to be used to select a family of transformation whose parameter is estimated using the likelihood approach. The available options ara (i) Akaike's information criterion "AIC"; (ii) R.Squared "R2"; (iii) Skewnnes "SK"; (iv) Kurtosis "KT".
#' @param usefamily a character string. Six different transformation families for the dependent variable can be chosen. For positive response variable the available options are (i) "Box-Cox" or "BC" ; (ii) "Dual" or "D" ("log"); (iii) "Manly" or "Ma". For negative response variables (i) "Yeo-Johhson" or "YJ"; (ii) "Modulus" or "Mod"; (iii) "Bickel-Doksum" or "BD"; (iv) "Manly" or "Ma" 
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of profile log-likelihood at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
doselectTransformation <- function(data,  criterion = "AIC", usefamily = NULL, ...) {
  trans <- list()
  if(is.null(usefamily)) {
    if (any(y <= 0)) {
    t1 <- trans$bickeldoksum <-  bickeldoksum(data,...)
    t2 <- trans$manly <-  manly(data, tr = TRUE,...)
    t3 <- trans$modulus <- modulus(data,...)
    t4 <- trans$yeojohnson <-  yeojohnson(data,...)
   
    if (criterion == "AIC") {
      AICVector <- c(-2*t1$llike, -2*t2$llike, -2*t3$llike, -2*t4$llike)
      selectedTrans <-  trans[[which.min(AICVector)]]
    }
    if(criterion == "R2") {
      R2Vector <-  c(summary(t1$modelt)$r.squared, summary(t2$modelt)$r.squared, summary(t3$modelt)$r.squared, summary(t4$modelt)$r.squared)       
      selectedTrans <- trans[[which.max(R2Vector)]]
    }
    if(criterion == "SK") {
      SKVector <-  c(abs(skewness(t1$yt)), abs(skewness(t2$yt)),abs(skewness(t3$yt)), abs(skewness(t4$yt)))
      selectedTrans <- trans[[which.min(SKVector)]]
    }
    if(criterion == "KT") {
      KTVector <-  c(abs(kurtosis(t1$yt)), abs(kurtosis(t2$yt)), abs(kurtosis(t3$yt)), abs(kurtosis(t3$yt)))
      selectedTrans <- trans[[which.min(KTVector)]]
    }
  }
    else {
      t1 <-  trans$bx <-  bx_cx( data, tr = TRUE,...)
      t2  <-  trans$manly <- manly( data,  tr = TRUE,...)
      t3 <- trans$modulus <- modulus( data, ...)

      
      if (criterion == "AIC") {
        AICVector <- c(-2*t1$llike, -2*t2$llike, -2*t3$llike)
        selectedTrans <-  trans[[which.min(AICVector)]]
      }
      if(criterion == "R2") {
        R2Vector <-  c(summary(t1$modelt)$r.squared, summary(t2$modelt)$r.squared, summary(t3$modelt)$r.squared)       
        selectedTrans <- trans[[which.max(R2Vector)]]
      }
      if(criterion == "SK") {
        SKVector <-  c(abs(skewness(t1$yt)), abs(skewness(t2$yt)),abs(skewness(t3$yt)))
        selectedTrans <- trans[[which.min(SKVector)]]
      }
      if(criterion == "KT") {
        KTVector <-  c(abs(kurtosis(t1$yt)), abs(kurtosis(t2$yt)), abs(kurtosis(t3$yt)), abs(kurtosis(t3$yt)))
        selectedTrans <- trans[[which.min(KTVector)]]
      }
    } 
    }
  else {
    
    if (usefamily=="YJ" | usefamily=="Yeo-Johnson"){
      selectedTrans <-  yeojohnson(data,...)
    }
   
    if (usefamily=="BC" | usefamily=="Box-Cox"){
      selectedTrans <-  bx_cx(data,..)
    }
    if (usefamily=="BD" | usefamily=="Bickel-Docksum"){
      selectedTrans <-  bickeldocksum(data,...)
    }
    if (usefamily=="Ma" | usefamily=="Manly"){
      selectedTrans <-  manly(object, data)
    }
    if (usefamily=="Mod" | usefamily=="Modulus"){
      selectedTrans <-  modulus( data,...)
    }
    
  }  
  selectedTrans
}
