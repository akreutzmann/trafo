#' Plots for objects of type trafo
#'
#' Plots that shows some residual diagnostics for the transformed model.

#' @param x an object of type \code{trafo}.
#' @param \dots additional arguments to be passed to the estimation function; see bcxEst()
#' @return Depending on the class of the underlying regression model one or two
#' Q-Q plots are returned for the Pearson residuals and, if suitable, for the 
#' standardized random effect.
#' @examples
#' # Load data
#' data("eusilcA_Vienna")
#' 
#' # Fit linear model
#' lm_Vienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben + 
#' rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
#' fam_allow + house_allow, data = eusilcA_Vienna)
#' 
#' # Transform dependent variable using skewness minimization
#' boxcox_trafo <- boxcox(object = lm_Vienna, lambda = "estim", method = "skew",
#' plotit = FALSE)
#' 
#' # Get plots
#' plot(boxcox_trafo)
#' @importFrom graphics mtext
#' @importFrom stats qqline qqnorm 
#' @export

plot.trafo <- function(x, ...) {

    residt <- NULL
    QQ_residt <- NULL
    raneft <- NULL
    sraneft <- NULL
    QQ_sraneft <- NULL
    qqnorm <- NULL
    qqline <- NULL
    mtext <- NULL
  
  
    # Residuals of the transformed model
    residt <- residuals(x$modelt, level = 0, type = "pearson")
    
    QQ_residt %<a-% qqnorm(residt, main = "Pearson residuals")
      
    
    if (inherits(x$modelt, "lme")) { 
      
      # Random effects of the transformed model
      raneft <- ranef(x$modelt)$'(Intercept)'
      sraneft <- (raneft - mean(raneft)) / sd(raneft)
      
      QQ_sraneft %<a-% qqnorm(sraneft, main = "Std. random effects")
      
      
      dev.hold()
      old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
      QQ_residt
      qqline(residt)
      QQ_sraneft
      qqline(sraneft)
      mtext("Normal Q-Q Plots", outer = TRUE, cex = 1.5)
      par(old.par)
      dev.flush()
    
    } else {
      dev.hold()
      old.par <- par(oma = c(0, 0, 2, 0))
      QQ_residt
      qqline(residt)
      mtext("Normal Q-Q Plot", outer = TRUE, cex = 1.5)
      par(old.par)
      dev.flush()
    }
    
    invisible()
  }
  
  
  
  
