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
#' @param trafo a character string that selects the transformation.
#' @param custum_func a function that determines a customized transformation.
#' @param custom_func_std a function that determines a customized standard
#' transformation.
#' @return Depending on the selected \code{method} the return is a log
#' likelihood, a skewness, a pooled skewness or a Kolmogorov-Smirnoff, Craemer
#' von Mises or Kullback Leibner divergence.
#' @keywords internal


estim_lm <- function(lambda, y, x, method, trafo, custom_func, custom_func_std){

    # Get residuals for all methods but ML
  # Wrapper for transformations, this means that we need a new argument
  # trafo in the function
  
  # Find the optimal lambda depending on method
  optimization <- if (method == "ml") {
    ML(y, x, lambda, trafo, custom_func_std = custom_func_std)
  } else if (method != "ml") {
    
    yt <- if (trafo == "boxcox") {
      as.matrix(box_cox(y = y, lambda = lambda, shift = 0)$y)
    } else if (trafo == "modulus") {
      as.matrix(modul(y = y, lambda = lambda))
    } else if (trafo == "bickeldoksum") {
      as.matrix(Bick_dok(y = y, lambda = lambda))
    } else if (trafo == "manly") {
      as.matrix(Manly(y = y, lambda = lambda))
    } else if (trafo == "dual") {
      as.matrix(Dual(y = y, lambda = lambda))
    } else if (trafo == "yeojohnson") {
      as.matrix(Yeo_john(y = y, lambda = lambda))
    } else if (trafo == "logshiftopt") {
      as.matrix(log_shift(y = y, lambda = lambda))
    } else if (trafo == "sqrtshift") {
      as.matrix(sqrt_shift(y = y, lambda = lambda))
    } else if (trafo == "gpower") {
      as.matrix(gPower(y = y, lambda = lambda))
    } else if (trafo == "custom") {
      as.matrix(custom_func(y = y, lambda = lambda))
    }
    
    # yt <- as.matrix(box_cox(y = y, lambda = lambda, shift = 0)$y)
    
    model_ML <- NULL
    try(model_ML <- lm(formula = yt ~ - 1 + x), silent = TRUE)
    
    if (is.null(model_ML)){
      stop("For some lambda in the interval, the likelihood does not converge.
         Choose another lambdarange.")
    } else {
      model_ML <- model_ML
    }
    res <- residuals(model_ML, level = 0, type = "pearson")
    
    
    optimization <- if (method == "skew") {
      skewness_min(res = res)
    } else if (method == "div.ks") {
      divergence_min_KS(res = res)
    } else if (method == "div.cvm") {
      divergence_min_CvM(res = res)
    } else if (method == "div.kl") {
      divergence_min_KL(res = res)
    }
    
  }
  
  
  return(optimization)
}