#' Wrapper function for estimation methods - linear mixed models
#' 
#' @param lambda transformation parameter
#' @param y vector of response variables
#' @param formula a formula object that contains the dependent and the explanatory
#' measures
#' @param data the data.frame that is given to functio nlme and that contains the 
#' regression variables. 
#' @param rand_eff the random effect extracted from the lme object. 
#' @param method a character string. In order to determine the optimal parameter
#' for the transformation five different estimation methods can be chosen
#' (i) Maximum-Likelihood ("ml"); (ii) skewness minimization ("skew");
#' (iii) minimization of Kolmogorov-Smirnoff divergence  ("div.ks");
#' (iv) minimization of Craemer von Mises divergence ("div.cvm"); (v)
#' minimization of Kullback Leibner divergence  ("div.kl"). In case of no and
#' log transformation "NA" can be selected since no optimization is neccessary
#' for these two transformation types.
#' @param trafo a character string that selects the transformation.
#' @return Depending on the selected \code{method} the return is a log
#' likelihood, a skewness, a pooled skewness or a Kolmogorov-Smirnoff, Craemer
#' von Mises or Kullback Leibner divergence.
#' @keywords internal


estim_lme <- function(lambda, y, formula, data, rand_eff, method, trafo){
  
  # Find the optimal lambda depending on method
  optimization <- if (method == "reml") {
    restricted_ML(y = y,
         formula = formula, 
         lambda = lambda,
         data = data,
         rand_eff = rand_eff,
         trafo = trafo)
  } else if (method != "reml") {
    
    # Get residuals for all methods but ML
    # Wrapper for transformations, this means that we need a new argument
    # trafo in the function
    yt <- if (trafo == "boxcox") {
      box_cox(y = y, lambda = lambda, shift = 0)$y
    } else if (trafo == "modulus") {
      modul(y = y, lambda = lambda)
    } else if (trafo == "bickeldoksum") {
      Bick_dok(y = y, lambda = lambda)
    } else if (trafo == "manly") {
      Manly(y = y, lambda = lambda)
    } else if (trafo == "dual") {
      Dual(y = y, lambda = lambda)
    } else if (trafo == "yeojohnson") {
      Yeo_john(y = y, lambda = lambda)
    } else if (trafo == "logshiftopt") {
      as.matrix(log_shift(y = y, lambda = lambda))
    } else if (trafo == "sqrtshift") {
      as.matrix(sqrt_shift(y = y, lambda = lambda))
    } else if (trafo == "gpower") {
      as.matrix(gPower(y = y, lambda = lambda))
    }
    
    data[paste(formula[2])] <- yt
    tdata <- data
    
    model_REML <- NULL
    try(model_REML <- lme(fixed = formula, 
                          data = tdata,
                          random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                          method = "REML", 
                          keep.data = FALSE, 
                          na.action = na.omit), silent=TRUE)
    if (is.null(model_REML)) {
      stop("For some lambda in the lambdarange, the likelihood does not converge.
           Choose another lambdarange.")
    } else {
      model_REML <- model_REML
    }
    res <- residuals(model_REML, level=0, type = "pearson")
    
    optimization <- if (method == "pskew") {
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
    
  
    
  }
  
  return(optimization)
}

