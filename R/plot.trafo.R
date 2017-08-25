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
    residFitt <- NULL
  
  
    # Residuals of the transformed model
    residt <- residuals(x$modelt, level = 0, type = "pearson")
    
    QQ_residt %<a-% qqnorm(residt,
                           ylab = "Sample-quantiles: Pearson residuals",
                           main = "")
    residFitt %<a-% plot_lm_adj(x$modelt, which = c(1L), sub = "",
                                labels.id = 1:length(residt)) 
    
    yFitt %<a-% plot(predict(x$modelt), x$yt,
                     ylab = "Transformed y", xlab = "Fitted values")
    
    cookst %<a-% plot_lm_adj(x$modelt, which = c(4L), sub = "",
                             labels.id = 1:length(residt))  
    
    hist_residt %<a-% hist(residt, nclass = 20, 
                           xlab = "Pearson residuals", 
                           main = "", prob = TRUE)
    
    if (inherits(x$modelt, "lme")) { 
      
      # Random effects of the transformed model
      raneft <- ranef(x$modelt)$'(Intercept)'
      sraneft <- (raneft - mean(raneft)) / sd(raneft)
      
      QQ_sraneft %<a-% qqnorm(sraneft,
                              ylab = "Sample-quantiles: Std. random effects",
                              main = "")
      
      hist_sraneft %<a-% hist(sraneft, nclass = 20, 
                             xlab = "Std. random effects", 
                             main = "", 
                             prob = TRUE)
      
      scattert %<a-% pairs(formula(x$modelt$terms), data = x$modelt$data,
                       main="")
      
      
      dev.hold()
      old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
      QQ_residt
      qqline(residt)
      QQ_sraneft
      qqline(sraneft)
      mtext("Normal Q-Q Plots", outer = TRUE, cex = 1)
      cat("Press [enter] to continue")
      line <- readline()
      hist_residt
      hist_sraneft
      mtext("Histogram", outer = TRUE, cex = 1)
      par(old.par)
      cat("Press [enter] to continue")
      line <- readline()
      residFitt
      cat("Press [enter] to continue")
      line <- readline()
      yFitt
      abline(lm(as.numeric(x$yt) ~ as.numeric(predict(x$modelt))),col="red",lwd=1.5)
      mtext("Transformed y vs Fitted", 3, 0.25, outer = FALSE, cex = 1)
      cat("Press [enter] to continue")
      line <- readline()
      scattert
      mtext("Scatter plots", 3, 0.25, outer = TRUE, cex = 1)
      cat("Press [enter] to continue")
      line <- readline()
      cookst
      dev.flush()
    
    } else {
      
      scattert %<a-% pairs(formula(x$modelt$terms), data = x$modelt$model,
                          main="")
      
      dev.hold()
      old.par <- par(oma = c(0, 0, 2, 0))
      QQ_residt
      qqline(residt)
      mtext("Normal Q-Q Plot", 3, 0.25, outer = FALSE, cex = 1)
      par(old.par)
      cat("Press [enter] to continue")
      line <- readline()
      old.par <- par(oma = c(0, 0, 2, 0))
      hist_residt
      mtext("Histogram", 3, 0.25, outer = TRUE, cex = 1)
      par(old.par)
      cat("Press [enter] to continue")
      line <- readline()
      residFitt
      cat("Press [enter] to continue")
      line <- readline()
      yFitt
      abline(lm(as.numeric(x$yt) ~ as.numeric(predict(x$modelt))),col="red",lwd=1.5)
      mtext("Transformed y vs Fitted", 3, 0.25, outer = FALSE, cex = 1)
      cat("Press [enter] to continue")
      line <- readline()
      scattert
      mtext("Scatter plots", 3, 0.25, outer = TRUE, cex = 1)
      cat("Press [enter] to continue")
      line <- readline()
      cookst
      dev.flush()
    }
    
    invisible()
  }
  
  
  
  
