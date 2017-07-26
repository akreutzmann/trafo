#' Wrapper function for estimation methods - linear mixed models
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


estim_lme <- function(lambda, y, formula, data, rand_eff, method){
  
  # Get residuals for all methods but ML
  # Wrapper for transformations, this means that we need a new argument
  # trafo in the function
  yt <- box_cox(y = y, lambda = lambda, shift = 0)$y
  data[paste(formula[2])] <- yt
  tdata <- data
  model_REML <- lme(fixed = formula, 
                    data = tdata,
                    random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                    method = "REML", 
                    keep.data = FALSE, 
                    na.action = na.omit)
  res <- residuals(model_REML, level=0, type = "pearson")
  
  # Find the optimal lambda depending on method
  optimization <- if (method == "reml") {
    restricted_ML(y = y,
         formula = formula, 
         lambda = lambda,
         data = data,
         rand_eff = rand_eff)
  } else if (method == "pskew") {
    pooled_skewness_min(model = model_REML, res = res)  
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

