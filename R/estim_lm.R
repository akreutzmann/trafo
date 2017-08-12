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
#' @param transfor a character string that selects the transformation.
#' @return Depending on the selected \code{method} the return is a log
#' likelihood, a skewness, a pooled skewness or a Kolmogorov-Smirnoff, Craemer
#' von Mises or Kullback Leibner divergence.
#' @keywords internal


estim_lm <- function(lambda, y, x, method, transfor){

    # Get residuals for all methods but ML
  # Wrapper for transformations, this means that we need a new argument
  # trafo in the function
  
  # Find the optimal lambda depending on method
  optimization <- if (method == "ml") {
    ML(y, x, lambda, transfor)
  } else if (method != "ml") {
    
    yt <- if(transfor == "t_bx_cx") {
      as.matrix(box_cox(y = y, lambda = lambda, shift = 0)$y)
    } else if (transfor == "t_mdls") {
      as.matrix(modul(y = y, lambda = lambda))
    } else if (transfor == "t_bck_dk") {
      as.matrix(Bick_dok(y = y, lambda = lambda))
    } else if (transfor == "t_mnl") {
      as.matrix(Manly(y = y, lambda = lambda))
    } else if (transfor == "t_dl") {
      as.matrix(Dual(y = y, lambda = lambda))
    } else if (transfor == "t_y_jhnsn") {
      as.matrix(Yeo_john(y = y, lambda = lambda))
    }
    # yt <- as.matrix(box_cox(y = y, lambda = lambda, shift = 0)$y)
    
    model_ML <- NULL
    try(model_ML <- lm(formula = yt ~ - 1 + x), silent = TRUE)
    
    if(is.null(model_ML)){
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