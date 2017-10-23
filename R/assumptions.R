#' First check of assumptions
#'
#' Gives a first overview if a transformation is useful and which transformation
#' is promising to fulfill the model assumptions normality, homoscedasticity and 
#' linearity.
#' 
#' @param object an object of type \code{lm}.
#' @param method  a character string. Different estimation methods can be used 
#' for the estimation of the optimal transformation parameter: 
#' (i) Maximum likelihood approach ("ml"), (ii) Skewness minimization ("skew"),  
#' (iii) Divergence minimization by Kolmogorov-Smirnoff ("div.ks"), 
#' by Cramer-von-Mises ("div.cvm") or by Kullback-Leibler ("div.kl"). Defaults
#' to "ml".
#' @return an object of class \code{diagnostics.compare_trafo}
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform with Bickel-Doksum transformation
#' bd_trafo <- bickeldoksum(object = lm_cars, plotit = FALSE)
#' 
#' # Transform with Box-Cox transformation
#' bc_trafo <- boxcox(object = lm_cars, method = "skew", plotit = FALSE)
#' 
#' # Compare transformed models
#' compare <- compare_trafo(object = lm_cars, trafos = list(bd_trafo, bc_trafo), 
#' std = FALSE)
#' 
#' # Get diagnostics
#' diagnostics(compare)
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export

assumptions <- function(object, method = "ml"){
  
  
  trafos <- list()

  trafos[["bickeldoksum"]] <- bickeldoksum(object = object, method = method,
                                           plotit = FALSE)
  trafos[["boxcoxshift"]] <- boxcoxshift(object = object, method = method,
                                         plotit = FALSE)
  trafos[["boxcox"]] <- boxcox(object = object, method = method,
                               plotit = FALSE)
  trafos[["dual"]] <- dual(object = object, method = method,
                           plotit = FALSE)
  trafos[["gpower"]] <- gpower(object = object, method = method,
                               plotit = FALSE)
  trafos[["manly"]] <- manly(object = object, method = method,
                             plotit = FALSE)
  trafos[["modulus"]] <- modulus(object = object, method = method,
                                 plotit = FALSE)
  trafos[["logshiftopt"]] <- logshiftopt(object = object, method = method,
                                         plotit = FALSE)
  trafos[["sqrtshift"]] <- sqrtshift(object = object, method = method,
                                     plotit = FALSE)
  trafos[["yeojohnson"]] <- yeojohnson(object = object, method = method,
                                       plotit = FALSE)
  
  trafos[["log"]] <- logtrafo(object = object)
  trafos[["reciprocal"]] <- reciprocal(object = object)
  trafos[["neglog"]] <- neglog(object = object)
  trafos[["glog"]] <- glog(object = object)
  
  # Get residuals
  resid_orig <- residuals(object, level = 0, type = "pearson")
  
  trafo_mod <- list()
  resid <- list()
  shapiroEst <- list()
  shapiroP <- list()
  skewness_trafo <- list()
  kurtosis_trafo <- list()
  breusch_pagan <- list()
  scatter_trafos <- list()
  
  for (transform in names(trafos)) {
    trafo_mod[[transform]] <- get_modelt(object = object, 
                                         trans_mod = trafos[[transform]], 
                                         std = FALSE)
    resid[[transform]] <- residuals(trafo_mod[[transform]], level = 0, 
                                    type = "pearson")
    
    if (length(resid[[transform]]) > 3 & length(resid[[transform]]) < 5000) {
      
      # Original model or first transformed
      shapiroEst_orig <- shapiro.test(resid_orig)$statistic[[1]]
      shapiroP_orig <- shapiro.test(resid_orig)$p.value[[1]]
      
      
      shapiroEst[[transform]] <- shapiro.test(resid[[transform]])$statistic[[1]]
      shapiroP[[transform]] <- shapiro.test(resid[[transform]])$p.value[[1]]
    } else {
      warning("Number of domains exceeds 5000 or is lower then 3 and thus the
              Shapiro-Wilk test is not applicable for residuals.")
      
      shapiroEst_orig <- NA
      shapiroP_orig <- NA
      
      shapiroEst[[transform]] <- NA
      shapiroP[[transform]] <- NA
    }
    
    skewness_orig <- skewness(resid_orig)
    kurtosis_orig <- kurtosis(resid_orig)
    
    skewness_trafo[[transform]] <- skewness(resid[[transform]])
    kurtosis_trafo[[transform]] <- kurtosis(resid[[transform]])
    
    breusch_pagan_orig <- bptest(formula(object$terms), 
                                 data = object$model)
    breusch_pagan[[transform]] <- bptest(formula(trafo_mod[[transform]]$terms), 
                                         data = trafo_mod[[transform]]$model)
    
  }
  
  norm_resid <- data.frame(Skewness  = round(skewness_orig, 4),
                           Kurtosis  = round(kurtosis_orig, 4),
                           Shapiro_W = round(shapiroEst_orig,4),
                           Shapiro_p = round(shapiroP_orig,4))
  
  hetero <- data.frame(BreuschPagan_V = round(breusch_pagan_orig$statistic, 4),
                       BreuschPagan_p = round(breusch_pagan_orig$p.value, 4))
  
  for (transform in names(trafos)) {
    
    norm_resid <- rbind(norm_resid, 
                        data.frame(Skewness  = round(skewness_trafo[[transform]], 4),
                                   Kurtosis  = round(kurtosis_trafo[[transform]], 4),
                                   Shapiro_W = round(shapiroEst[[transform]],4),
                                   Shapiro_p = round(shapiroP[[transform]], 4)))
    
    hetero <- rbind(hetero, 
                    data.frame(BreuschPagan_V = round(breusch_pagan[[transform]]$statistic, 4),
                               BreuschPagan_p = round(breusch_pagan[[transform]]$p.value, 4)))
    
  }
  rownames(norm_resid) <- c("untransformed", names(trafos))
  rownames(hetero) <- c("untransformed", names(trafos))
  
  norm_resid <- norm_resid[order(norm_resid$Shapiro_p, decreasing = TRUE),]
  hetero <- hetero[order(hetero$BreuschPagan_p, decreasing = TRUE),]
  
  cat("Test normality assumption \n")
  print(norm_resid)
  cat("\n")
  cat("Test homoscedasticity assumption \n")
  print(hetero)
  cat("\n")
  cat("Test linearity assumption \n")
  cat("Press [enter] to continue")
  line <- readline()
  pairs(formula(object$terms), data = object$model, main = "Untransformed model")
  for (transform in names(trafos)) {
    cat("Press [enter] to continue")
    line <- readline()
    pairs(formula(trafo_mod[[transform]]$terms), 
          data = trafo_mod[[transform]]$model,
          main = transform) 
  }
 
}
