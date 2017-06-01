# Only for linear models -------------------------------------------------------

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
  yt <- rep(NA, n)
  
  lglike <- -lglike(lambda = lambda, y = y, qr = qr, n = n)
}

# Log-likelihood function for ML method
lglike <- function(lambda, y, qr, n, ...) {
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

# Only for linear mixed models -------------------------------------------------


# Restricted maximum likelihood 
restricted_ML <- function(y = y,
                 formula = formula, 
                 lambda,
                 data = data,
                 rand_eff = rand_eff
                 ) {
  
  # Wrapper for other transformations
  yt <- box_cox_std(y = y, lambda = lambda)
  data[paste(formula[2])] <- yt
  tdata <- data
  model_REML <- lme(fixed     = formula,
                    data      = tdata,
                    random    = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                    method    = "REML",
                    keep.data = FALSE, 
                    na.action = na.omit)
  
  log_likelihood <- -logLik(model_REML)
  
  return(log_likelihood)
}


# Pooled skewness by Rojas-Perilla
pooled_skewness_min <- function(model, res) {
  skew_resid <- skewness(res)
  random_effect <- as.matrix(random.effects(model))[,1]
  skew_random_effect <- skewness(random_effect)
  sigmae2est <- model_est$sigma^2
  sigmau2est <- as.numeric(VarCorr(model_est)[1,1])
  w <- sigmae2est / (sigmae2est + sigmau2est)
  
  pooled_skew <- w * abs(skew_resid) + (1 - w) * abs(skew_random_effect)
  
  return(pooled_skew)
}


# For linear and linear mixed models -------------------------------------------

#' Skewness minimization by Molina 
#' 
#' @param res residuals from a linear model with response variable yt and 
#' explanatory variables x
#' @return absolute value of the skewness of the residuals
#' @import moments
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
#' @import FNN
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
