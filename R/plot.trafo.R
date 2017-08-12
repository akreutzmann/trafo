#' Plot Transformation
#'
#' Plots object of type \code{transformation}. Four plots are shown: a plot of the profile loglikelihood vs lambda; a qq plot; a histogram of the residuals vs fitted values of the transformed model 
#' @param x an object of type \code{transformation}
#' @param \dots additional arguments to be passed to the estimation function; see bcxEst()
#' @return an object of class \code{transformation}; see bcxEst()
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
  
  
  
  
