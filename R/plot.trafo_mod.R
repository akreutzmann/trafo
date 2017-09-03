#' Plot for regression models with untransformed and transformed dependent 
#' variable
#'
#' Plots for comparing the residual diagnostics of transformed and untransformed 
#' models.
#' 
#' @param x an object of type \code{trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @importFrom pryr %<a-%
#' @importFrom grDevices dev.flush dev.hold
#' @importFrom graphics abline par plot
#' @export

plot.trafo_mod <- function(x, ...) {
  
  residFit_orig <- NULL
  residFit_trafo <- NULL
  QQ_orig <- NULL
  QQ_trafo <- NULL
  scaleLoc_orig <- NULL
  scaleLoc_trafo <- NULL
  residLev_orig <- NULL
  residLev_trafo <- NULL
  cooks_orig <- NULL
  cooks_trafo <- NULL
  QQ_resid_orig <- NULL
  QQ_resid_trafo <- NULL
  QQ_sranef_orig <- NULL
  QQ_sranef_trafo <- NULL
  
  
  
  if (inherits(x$orig_mod, "lm")) {
    
    n <- length(x$orig_mod$residuals)
    
    residFit_orig %<a-%  plot(x$orig_mod, which = c(1L), main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residFit_trafo %<a-%  plot(x$trafo_mod, which = c(1L), main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    QQ_orig %<a-%  plot(x$orig_mod, which = c(2L), main = "Untransformed model",
                        labels.id = 1:n, cex.oma.main = 1.15, 
                        sub.caption = "")
    QQ_trafo %<a-%  plot(x$trafo_mod, which = c(2L), main = "Transformed model",
                         labels.id = 1:n, cex.oma.main = 1.15, 
                         sub.caption = "")
    
    scaleLoc_orig %<a-%  plot(x$orig_mod, which = c(3L), main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    scaleLoc_trafo %<a-%  plot(x$trafo_mod, which = c(3L), main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    cooks_orig %<a-%  plot(x$orig_mod, which = c(4L), main = "Untransformed model",
                           labels.id = 1:n, cex.oma.main = 1.15, 
                           sub.caption = "")
    cooks_trafo %<a-%  plot(x$trafo_mod, which = c(4L), main = "Transformed model",
                            labels.id = 1:n, cex.oma.main = 1.15, 
                            sub.caption = "")
    
    residLev_orig %<a-%  plot(x$orig_mod, which = c(5L), main = "Untransformed model",
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residLev_trafo %<a-%  plot(x$trafo_mod, which = c(5L), main = "Transformed model",
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))  
    residFit_orig
    residFit_trafo
    #par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2))
    QQ_orig
    QQ_trafo
    #par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2))
    scaleLoc_orig
    scaleLoc_trafo
    #par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2))
    residLev_orig
    residLev_trafo
    #par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2))
    cooks_orig
    cooks_trafo
    par(old.par)
  } else if (inherits(x$orig_mod, "lme")) {
    resid <- residuals(x$orig_mod, level = 0, type = "pearson")
    residt <- residuals(x$trafo_mod, level = 0, type = "pearson")
    
    
    QQ_resid_orig %<a-% qqnorm(resid,
                          ylab = "Sample-quantiles",
                          main = "Untransformed model")
    QQ_resid_trafo %<a-% qqnorm(residt,
                           ylab = "Sample-quantiles",
                           main = "Transformed model")
    
    n <- length(residt)
    
    residFit_orig %<a-% plot_lm_adj(x$orig_mod, which = c(1L), sub.caption = "",
                                   labels.id = 1:n, 
                                   main = "Untransformed model")
    residFit_trafo %<a-% plot_lm_adj(x$trafo_mod, which = c(1L), sub.caption = "",
                                    labels.id = 1:n, 
                                    main = "Transformed model") 
    
  #  scaleLoc_orig %<a-%  plot_lm_adj(x$orig_mod, which = c(3L), 
  #                                   main = "Untransformed model",
  #                            labels.id = 1:n, cex.oma.main = 1.15, 
  #                            sub.caption = "")
  #  scaleLoc_trafo %<a-%  plot_lm_adj(x$trafo_mod, which = c(3L), 
  #                                    main = "Transformed model",
  #                             labels.id = 1:n, cex.oma.main = 1.15, 
  #                             sub.caption = "")
    
 #   residLev_orig %<a-%  plot(x$orig_mod, which = c(5L), 
#                              main = "Untransformed model",
#                              labels.id = 1:n, cex.oma.main = 1.15, 
#                              sub.caption = "")
 #   residLev_trafo %<a-%  plot(x$trafo_mod, which = c(5L), 
#                               main = "Transformed model",
 #                              labels.id = 1:n, cex.oma.main = 1.15, 
 #                              sub.caption = "")
    
    raneft <- ranef(x$trafo_mod)$'(Intercept)'
    sraneft <- (raneft - mean(raneft)) / sd(raneft)
    
    ranef <- ranef(x$orig_mod)$'(Intercept)'
    sranef <- (ranef - mean(ranef)) / sd(ranef)
    
    QQ_sranef_trafo %<a-% qqnorm(sraneft,
                            ylab = "Sample-quantiles",
                            main = "Transformed model")
    QQ_sranef_orig %<a-% qqnorm(sranef,
                            ylab = "Sample-quantiles",
                            main = "Untransformed model")
    
    old.par <- par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    dev.hold()
    #old.par <- par(mfrow = c(1, 1))
    #par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    residFit_orig
    residFit_trafo
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    QQ_resid_orig
    qqline(resid)
    QQ_resid_trafo
    qqline(residt)
    mtext("Normal Q-Q Plots - Pearson residuals", outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    #old.par <- par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    QQ_sranef_orig
    qqline(sranef)
    QQ_sranef_trafo
    qqline(sraneft)
    mtext("Normal Q-Q Plots - Std. random effects", outer = TRUE, cex = 1)
  #  cat("Press [enter] to continue")
  #  line <- readline()
  #  old.par <- par(mfrow = c(1, 2))
  #  scaleLoc_orig
  #  scaleLoc_trafo
  #  par(old.par)
   # cat("Press [enter] to continue")
  #  line <- readline()
  #  old.par <- par(mfrow = c(1, 2))
  #  residLev_orig
   # residLev_trafo
    #par(old.par)
    dev.flush()
    par(old.par)
  }
  invisible()
}