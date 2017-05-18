#' Summary transformations
#'
#' Computes objects to be shown in the summary function for objects of type \code{transformation}
#' @param x an object of type \code{transformation}
#' @return out an object of class \code{summary.transformation}
#' #@keywords internal
#' @export
summary.transformation <- function(x, ...) {
  yt <- summary(x$modelt)$residuals
  n <- length(yt)
  out <- x
  logvector  <- as.vector(x$logvector)
  lambdavector <- x$lambdavector
  lambdaoptim <- x$lambdaoptim
  logoptim <- x$llike
  lim <- logoptim - qchisq(0.95, 1)/2
  m <- length(logvector)
  index <- range((1L:m)[logvector > lim])
  out$cinf <- lambdavector[index[1]]
  out$csup <- lambdavector[index[2]]
  if (n < 5000) {
    out$shapiro_est <- shapiro.test(yt)$statistic[[1]]
    out$shapiro_pvalue <- shapiro.test(yt)$p.value[[1]]
  }
  else{
    out$shapiro_est <-"n > 5000" 
    out$shapiro_pvalue <- "NA"
  }
  out$skewness <- skewness(yt)
  out$kurtosis <- kurtosis(yt)
  class(out) <- "summary.transformation"
  out
}
summary.list.transformation <- function(x, ...) {
    yt <- summary(x$modelt)$residuals
    n <- length(yt)
    out <- x
    logvector  <- as.vector(x$logvector)
    lambdavector <- x$lambdavector
    lambdaoptim <- x$lambdaoptim
    logoptim <- x$llike
    lim <- logoptim - qchisq(0.95, 1)/2
    m <- length(logvector)
    index <- range((1L:m)[logvector > lim])
    out$cinf <- lambdavector[index[1]]
    out$csup <- lambdavector[index[2]]
    if (n < 5000) {
      out$shapiro_est <- shapiro.test(yt)$statistic[[1]]
      out$shapiro_pvalue <- shapiro.test(yt)$p.value[[1]]
    }
    else{
      out$shapiro_est <-"n > 5000" 
      out$shapiro_pvalue <- "NA"
    }
    out$skewness <- skewness(yt)
    out$kurtosis <- kurtosis(yt)
    class(out) <- "summary.transformation"
    out
}


#' Print summary transformations
#'
#' prints objects to be shown in the summary function for objects of type \code{transformation}
#' @param x an object of type \code{transformation}
#' @keywords internal
print.summary.transformation <- function(x, ...) {
  cat("\n Tranformation family:", x$family, "\n")
  cat("Lambda hat:", x$lambdahat, "\n")
  cat("Log-Likelihood:", x$llike, "\n")
  cat("95% LR Confidence Region for lambda hat: [" ,x$cinf, ",", x$csup ,"] (df=1)\n")
  cat("R.Squared:", summary(x$modelt)$r.squared, "\n")
  cat("\n \t Statistics on transformed variable \n")
  cat("\n Shapiro-Wilk normality test: \n W =", 
      x$shapiro_est )
  cat(", p-value = ",   x$shapiro_pvalue, "\n")
  cat("\n Skewness: ", x$skewnes)
  cat("\n Kurtosis: ", x$kurtosis, "\n")
}
