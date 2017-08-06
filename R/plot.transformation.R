#' Plot Transformation
#'
#' Plots object of type \code{transformation}. Four plots are shown: a plot of the profile loglikelihood vs lambda; a qq plot; a histogram of the residuals vs fitted values of the transformed model 
#' @param x an object of type \code{transformation}
#' @param \dots additional arguments to be passed to the estimation function; see bcxEst()
#' @return an object of class \code{transformation}; see bcxEst()
#' @import ggplot2
#' @export

plot.transformation <- function(x, ...) {
  
  browser()
  
  logvector  <- as.vector(x$logvector)
  lambdavector <- x$lambdavector
  lambdaoptim <- x$lambdahat
  logoptim <- x$llike
  lim <- logoptim + qchisq(0.95, 1)/2
  m <- length(logvector)
  index <- range((1L:m)[logvector > lim])
  cinf <- lambdavector[index[1]]
  csup <- lambdavector[index[2]]
  vline <- c(cinf, lambdaoptim, csup)
  rt <-  x$modelt$residuals
  fittedt <- x$model$fitted.values
  data1 <- data.frame(logvector = logvector,  lambdavector= lambdavector) 
  data2 <- data.frame(residuals = rt) 
  data3 <- data.frame(residualst = rt, fittedt = fittedt)
  cat("Press [enter] to continue or type in [q] to quit" )
  line <- readline()
  if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q") {
    print(ggplot(data1, aes(x=lambdavector,
             y=logvector))+ geom_line()+ geom_vline(xintercept = vline,linetype="dashed") + geom_abline(intercept = logoptim, color="red", linetype="dashed") + xlab(expression(lambda)) + ylab("Profile log-likelihood"))

 cat("Press [enter] to continue or type in [q] to quit" ) 
    line <- readline()
    if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q" ) {
      print(   ggplot(data2, aes(sample = residuals)) + stat_qq() + ggtitle("QQ-Plot") )
      
      cat("Press [enter] to continue or type in [q] to quit" )
      line <- readline()
      if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q") {
         print(ggplot(data2, aes(x=residuals)) + geom_histogram(bins=20, colour="black",  aes(y=..density.., fill=..count..))
          + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
          + stat_function(fun=dnorm, color="red", args=list(mean=mean(rt), sd=sd(rt)))
          + xlab("Transformed Residuals") + ylab("Density") )
        cat("Press [enter] to continue or type in [q] to quit" )
        line <- readline()
        if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q") {
           print(ggplot(data3, aes(x=fittedt, y=residualst)) 
                 + geom_point(size=0.1) + geom_hline(yintercept = 0, color="red", linetype="dashed")
                 + xlab("Fitted Values")
                 + ylab("Transformed Residuals"))
        }
      }
    }
  }
}