#' Selects Transformation
#'
#' Chooses the 'best' transformation in the sense of AIC, R.Squared, Skewness or Kurtosis; For positive \code{y} compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum  
#' @param data a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of profile log-likelihood at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector Employed family of transformations
#' @return A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords internal
docompareTransformation <- function (data, lambda.dual = 0.5, plotit = TRUE,...) {
  n <- nrow(data)
  k <- ncol(data)-1
  y <- data[,1]
  x <- data[,2:k]
  modely <- lm(y ~ ., data.frame(y, x ))
  logVector <- NULL
  residt <- NULL
  lambdaVector <- NULL
  family1 <- NULL
  family2 <- NULL
  trans <- list()
  if (any(y <= 0)) {
      t1 <- trans$yeojohnson <-  yeojohnson(data)
      t2 <- trans$modulus <- modulus(data)
      t3 <- trans$bickeldoksum <-  bickeldoksum(data)
      t4 <- trans$manly <-  manly(data, tr = TRUE)
      showplot <- c(!is.infinite(t1$llike), !is.infinite(t2$llike), !is.infinite(t3$llike), !is.infinite(t4$llike))     
      lambdahVector <-  c(ifelse(showplot[1], t1$lambdahat , NA), 
                          ifelse(showplot[2], t2$lambdahat , NA),
                          ifelse(showplot[3], t3$lambdahat, NA),
                          ifelse(showplot[4], t4$lambdahat, NA))    
      AICVector <- c(-2*t1$llike, -2*t2$llike, -2*t3$llike, -2*t4$llike)
      R2Vector <-  c(ifelse(showplot[1], summary(t1$modelt)$r.squared, NA), 
                     ifelse(showplot[2], summary(t2$modelt)$r.squared, NA),
                     ifelse(showplot[3], summary(t3$modelt)$r.squared, NA),
                     ifelse(showplot[4], summary(t4$modelt)$r.squared, NA) )     
      SKVector <-  c(ifelse(showplot[1], skewness(t1$modelt$residuals), NA),
                     ifelse(showplot[2], skewness(t2$modelt$residuals), NA),
                     ifelse(showplot[3], skewness(t3$modelt$residuals), NA), 
                     ifelse(showplot[4], skewness(t4$modelt$residuals), NA))
      KTVector <-  c(ifelse(showplot[1], kurtosis(t1$modelt$residuals), NA),
                     ifelse(showplot[2], kurtosis(t2$modelt$residuals), NA),
                     ifelse(showplot[3], kurtosis(t3$modelt$residuals), NA), 
                     ifelse(showplot[4], kurtosis(t4$modelt$residuals), NA))
      if(length(t1$yt) < 5000){
        p_value <- c(ifelse(showplot[1],  shapiro.test(t1$modelt$residuals)$p.value, NA),
                     ifelse(showplot[2], shapiro.test(t2$modelt$residuals)$p.value, NA), 
                     ifelse(showplot[3], shapiro.test(t3$modelt$residuals)$p.value, NA),  
                     ifelse(showplot[4], shapiro.test(t4$modelt$residuals)$p.value, NA)) 
        W <- c(ifelse(showplot[1], shapiro.test(t1$modelt$residuals)$statistic, NA),
               ifelse(showplot[2], shapiro.test(t2$modelt$residuals)$statistic, NA), 
               ifelse(showplot[3], shapiro.test(t3$modelt$residuals)$statistic, NA), 
                ifelse(showplot[4],shapiro.test(t4$modelt$residuals)$statistic, NA)) 
      }
      else {
      p_value <- rep("n > 5000", 4) 
      W <- rep("-", 4)  
      }
      output <- data.frame(lambdahat = lambdahVector, AIC= AICVector, R.Squared = R2Vector, Skewness = SKVector, Kurtosis = KTVector, p_value = p_value)
      rownames(output) <- c("Yeo-Johnson", "Modulus", "Bickel-Doksum", "Manly")
  }
  else {
    t1 <- trans$bcx <-  bx_cx(data)
    t2 <- trans$dual <- dual(data, lambda = lambda.dual)
    t3 <- trans$manly <-  manly(data, tr = TRUE)
    t4 <- trans$notrans <- modely
    t4$family <- "No Trans."
    showplot <- c(!is.infinite(t1$llike), TRUE , !is.infinite(t3$llike), TRUE)     
    lambdahVector <-  c(ifelse(showplot[1], t1$lambdahat , NA),
                        lambda.dual,
                        ifelse(showplot[3], t3$lambdahat, NA),
                        "-")    
    AICVector <- c(-2*t1$llike, NA, -2*t3$llike, -2*logLik(t4))
    R2Vector <-  c(ifelse(showplot[1], summary(t1$modelt)$r.squared, NA),
                   ifelse(showplot[2], summary(t2$modelt)$r.squared, NA), 
                   ifelse(showplot[3], summary(t3$modelt)$r.squared, NA), 
                   ifelse(showplot[4], summary(t4)$r.squared, NA) )     
    SKVector <-  c(ifelse(showplot[1], skewness(t1$modelt$residuals), NA),
                   ifelse(showplot[2], skewness(t2$modelt$residuals), NA),
                   ifelse(showplot[3], skewness(t3$modelt$residuals), NA),
                   ifelse(showplot[4], skewness(t4$residuals), NA))
    KTVector <-  c(ifelse(showplot[1], kurtosis(t1$modelt$residuals), NA),
                   ifelse(showplot[2], kurtosis(t2$modelt$residuals), NA),
                   ifelse(showplot[3], kurtosis(t3$modelt$residuals), NA), 
                   ifelse(showplot[4], kurtosis(t4$residuals), NA))
    if(length(t1$yt) < 5000) {
    p_value <- c(ifelse(showplot[1], shapiro.test(t1$modelt$residuals)$p.value, NA), 
                 ifelse(showplot[2], shapiro.test(t2$modelt$residuals)$p.value, NA),
                 ifelse(showplot[3], shapiro.test(t3$modelt$residuals)$p.value, NA), 
                 ifelse(showplot[4], shapiro.test(t4$residuals)$p.value, NA)) 
    W <- c(ifelse(showplot[1], shapiro.test(t1$modelt$residuals)$statistic, NA), 
           ifelse(showplot[2], shapiro.test(t2$modelt$residuals)$statistic, NA), 
           ifelse(showplot[3], shapiro.test(t3$modelt$residuals)$statistic, NA), 
           ifelse(showplot[4], shapiro.test(t4$residuals)$statistic, NA)) 
    }
    else{ 
      p_value <- rep("n > 5000", 4)  
      W <- rep("-", 4)  
      }
    output <- data.frame(lambdahat = lambdahVector, AIC= AICVector, R.Squared = R2Vector, Skewness = SKVector, Kurtosis = KTVector, W = W, p_value = p_value)
      rownames(output) <- c("Box-Cox", "Dual", "Manly", "No Transf.")
    }
      if(plotit == TRUE) {
        nplot <- length(which(showplot))
        if(showplot[1]) {
          residt <- as.vector(rstandard(t1$modelt))
          fittedt <- as.vector(t1$modelt$fitted.values)
          family2 <- rep(t1$family, n)
          }
          if(showplot[2]) {
            l  <- length(residt) 
            lt2 <- length(t2$modelt$residuals)
            ini <- l + 1
            fin <- l + lt2
            residt[ini:fin] <- rstandard(t2$modelt)
            family2[ini:fin] <- rep(t2$family, lt2)
            fittedt[ini:fin] <- t2$modelt$fitted.values
          }
           if(showplot[3]) {
             l  <- length(logVector) 
             lt3 <- length(t3$logvector)
             ini <- l + 1
             fin <- l + lt3
             logVector[ini:fin] <- as.vector(t3$logvector)
             lambdaVector[ini:fin] <-  as.vector(t3$lambdavector)
             family1[ini:fin] <- rep(t3$family, lt3)
             l  <- length(residt) 
             lt3 <- length(t3$modelt$residuals)
             ini <- l + 1
             fin <- l + lt3
             residt[ini:fin] <- rstandard(t3$modelt)
             family2[ini:fin] <- rep(t3$family, lt3)
             fittedt[ini:fin] <- t3$modelt$fitted.value
             lm <- length(mean)+ 1
           }
        if(showplot[4]) {
          l  <- length(residt) 
          lt4 <- length(t4$residuals)
          ini <- l + 1
          fin <- l + lt4
          residt[ini:fin] <- rstandard(t4)
          family2[ini:fin] <- rep(t4$family, lt4)
          fittedt[ini:fin] <- t4$fitted.value
          lm <- length(mean)+ 1
        }
        
     if(any(showplot)){
       dataplot <- data.frame(loglike = logVector, lambda = lambdaVector, Family = family1 )
       dataplot$family <- as.factor(family1)
       dataplot2 <- data.frame(residualst= residt, fittedt = fittedt, Family = family2)
       dataplot2$family <- as.factor(family2)
     }
     
          cat("Press [enter] to continue or type in [q] to quit" ) 
          line <- readline()
          if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q" ) {
          stats <- aggregate(residualst ~ family2, dataplot2, function(x) c(mean = mean(x), sd = sd(x)))
          stats <- data.frame(Family = stats[, 1], stats[, 2])
          gg1 <- with(stats[1,],stat_function(data = dataplot2[dataplot2$Family == stats[1, 1],],fun = dnorm, args = list(mean = mean, sd = sd))) 
          gg2 <- if(nplot ==2  | nplot ==3)  with(stats[2,],stat_function(data = dataplot2[dataplot2$Family == stats[2, 1],],fun = dnorm, args = list(mean=mean, sd = sd))) 
          gg3 <- if(nplot ==3)  with(stats[3,],stat_function(data = dataplot2[dataplot2$Family == stats[3, 1],],fun = dnorm, args = list(mean = mean, sd = sd)))       
          print(ggplot(dataplot2, aes(x = residualst)) +  geom_histogram(bins = 20, aes(y=..density..,fill = Family),  color = "grey30")
                +gg1 + gg2 + gg3 + facet_grid(.~Family   , scales ="free") + xlab("Standarized Residuals") + ylab("Density"))
          cat("Press [enter] to continue or type in [q] to quit" ) 
          line <- readline()
          if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q" ) {
             print(ggplot(data=dataplot2,aes(sample = residualst))
                   + geom_point(stat="qq",aes(colour=Family))
                   + facet_grid(.~Family , scales = "free")
                   +  ggtitle("QQ-Plot")
                   + geom_abline())
            cat("Press [enter] to continue or type in [q] to quit" ) 
            line <- readline()
            if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q" ) {
              print(ggplot(dataplot2, aes(x=fittedt, y=residualst, colour=Family)) 
               + geom_point(aes(group=Family))
               + geom_hline( yintercept=0, color='black', linetype=2)
               + facet_grid(.~Family    , scales = "free")
               + ylab("Standarized Residuals") 
               + xlab("Fitted Values"))
             
            cat("Press [enter] to continue or type in [q] to quit" ) 
            line <- readline()
            if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q" ) {
              print(ggplot(dataplot2, aes(x = family, y = residualst , fill = Family)) +
                      geom_boxplot() + xlab("Transformations") + ylab("Standarized Residuals"))
            } 
            
           }
          }
        }  
    }
      cat( " \n *p-value for Shapiro-Wilk normality test \n \n ")
      output
  
}