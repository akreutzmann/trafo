#' Wrapper function for estimation methods - linear models
#' 
#' @param lambda transformation parameter
#' @param y vector of response variables
#' @param x matrix of regressors
#' @param method a character string. In order to determine the optimal parameter
#' for the transformation five different estimation methods can be chosen
#' (i) Maximum-Likelihood ("ml"); (ii) skewness minimization ("skew");
#' (iii) minimization of Kolmogorov-Smirnoff divergence  ("div.ks");
#' (iv) minimization of Craemer von Mises divergence ("div.cvm"); (v)
#' minimization of Kullback Leibner divergence  ("div.kl"). In case of no and
#' log transformation "NA" can be selected since no optimization is neccessary
#' for these two transformation types.
#' @return Depending on the selected \code{method} the return is a log
#' likelihood, a skewness, a pooled skewness or a Kolmogorov-Smirnoff, Craemer
#' von Mises or Kullback Leibner divergence.
#' @keywords internal


estim_lm <- function(lambda, y, x, method){
  
  # Get residuals for all methods but ML
  yt <- box_cox(y = y, lambda = lambda, shift = 0)$y
  model_ML <- lm(formula = yt ~ x)
  res <- residuals(model_ML)
  
  # Find the optimal lambda depending on method
  optimization <- if (method == "ml") {
    ML(y, x, lambda)
  } else if (method == "skew") {
    skewness_min(res = res)
  } else if (method == "div.ks") {
    divergence_min_KS(res = res)
  } else if (method == "div.cvm") {
    divergence_min_CvM(res = res)
  } else if (method == "div.kl") {
    divergence_min_KL(res = res)
  }
  
  return(optimization)
}


#' Maximum Likelihood 
#' 
#' @param y vector of response variables
#' @param x matrix of regressors
#' @param lambda transformation parameter
#' @return log-likelihood
#' @keywords internal

ML <- function(y, x, lambda){
  qr <- qr(x)
  n <- length(y)
  k <- ncol(x)
  yt <- rep(NA, n)

  lglike(lambda = lambda)
}

# Log-likelihood function for ML method
lglike <- function(lambda, ...) {
  if (abs(lambda) != 0) {
    yt <- (y^lambda - 1)/lambda
    
  }
  else {
    yt <- log(y) 
  }
  zt <- yt/exp((lambda - 1)*mean(log(y)))
  llike <- -n/2 * log((sum(qr.resid(qr, zt)^2))/n)
  llike
}


#' Skewness minimization by Molina 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return absolute value of the skewness of the residuals
#' @keywords internal

skewness_min <- function(res = res) {
  skew_resid <- skewness(res)
  
  absolute_skew <- abs(skew_resid)
  
  return(absolute_skew)
}

#' Divergence minimization by Kolmogorov Smirnoff
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return differences of supremum
#' @keywords internal

divergence_min_KS <- function(res = res) {
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs
  
  supremum_diff <- max(abs(difs))
  
  return(supremum_diff)
}


#' Divergence minimization by Craemer von Mises 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return sum of squared differences
#' @keywords internal

divergence_min_CvM <- function(res = res) {
  
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs
  
  sum_sq_diff <- sum((difs)^2)
  
  return(sum_sq_diff)
}


#' Divergence minimization by Kullback-Leibler
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return median of Kullback-Leibler divergence
#' @keywords internal

divergence_min_KL <- function(res = res) {
  
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.probs <- eval.probs[-c(1,length(eval.probs))]
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <-  quantile(res, probs = eval.probs)
  divergence_KL <- KL.divergence(eval.probs, test.probs, k = 5)
  
  median_divergence_KL <- median(divergence_KL)
  
  return(median_divergence_KL)
}
