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

plot.compare_trafo <- function(x, ...) {
  
  residFit_One <- NULL
  residFit_Two <- NULL
  QQ_One <- NULL
  QQ_Two <- NULL
  hist_One <- NULL
  hist_Two <- NULL
  yFit_One <- NULL
  yFit_Two <- NULL
  scatter_One <- NULL
  scatter_Two <- NULL
  scaleLoc_One <- NULL
  scaleLoc_Two <- NULL
  residLev_One <- NULL
  residLev_Two <- NULL
  cooks_One <- NULL
  cooks_Two <- NULL
  QQ_resid_One <- NULL
  QQ_resid_Two <- NULL
  QQ_sranef_One <- NULL
  QQ_sranef_Two <- NULL
  
  
  
  if (inherits(x$trafoOne, "lm")) {
    
    n <- length(x$trafoOne$residuals)
    
    residFit_One %<a-%  plot(x$trafoOne, which = c(1L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residFit_Two %<a-%  plot(x$trafoTwo, which = c(1L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    QQ_One %<a-%  plot(x$trafoOne, which = c(2L), main = x$trafos[[1]],
                        labels.id = 1:n, cex.oma.main = 1.15, 
                        sub.caption = "")
    QQ_Two %<a-%  plot(x$trafoTwo, which = c(2L), main = x$trafos[[2]],
                         labels.id = 1:n, cex.oma.main = 1.15, 
                         sub.caption = "")
    
    # Histogram
    resid_One <- residuals(x$trafoOne, level = 0, type = "pearson")
    resid_Two <- residuals(x$trafoTwo, level = 0, type = "pearson")
    
    
    hist_One %<a-% hist(resid_One, nclass = 20, xlab = "Pearson residuals", 
                         main = x$trafos[[1]], prob = TRUE)
    hist_Two %<a-% hist(resid_Two, nclass = 20, xlab = "Pearson residuals", 
                          main = x$trafos[[2]], prob = TRUE)
    
    # Fitted vs. observed
    fitted_One <- predict(x$trafoOne)
    fitted_Two <- predict(x$trafoTwo)
    
    y_One <- model.response(x$trafoOne$model)
    y_Two <- model.response(x$trafoTwo$model)
    
    yFit_One %<a-% plot(fitted_One, y_One,
                         ylab = "Transformed y", xlab = "Fitted values",
                         main = x$trafos[[1]])
    yFit_Two %<a-% plot(fitted_Two, y_Two,
                          ylab = "Transformed y", xlab = "Fitted values",
                          main = x$trafos[[2]])
    
    # Scatterplots
    scatter_One %<a-% pairs(formula(x$trafoOne$terms), data = x$trafoOne$model,
                             main = x$trafos[[1]])
    scatter_Two %<a-% pairs(formula(x$trafoTwo$terms), data = x$trafoTwo$model,
                              main = x$trafos[[2]])
    
    
    scaleLoc_One %<a-%  plot(x$trafoOne, which = c(3L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    scaleLoc_Two %<a-%  plot(x$trafoTwo, which = c(3L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    cooks_One %<a-%  plot(x$trafoOne, which = c(4L), main = x$trafos[[1]],
                           labels.id = 1:n, cex.oma.main = 1.15, 
                           sub.caption = "")
    cooks_Two %<a-%  plot(x$trafoTwo, which = c(4L), main = x$trafos[[2]],
                            labels.id = 1:n, cex.oma.main = 1.15, 
                            sub.caption = "")
    
    residLev_One %<a-%  plot(x$trafoOne, which = c(5L), main = x$trafos[[1]],
                              labels.id = 1:n, cex.oma.main = 1.15, 
                              sub.caption = "")
    residLev_Two %<a-%  plot(x$trafoTwo, which = c(5L), main = x$trafos[[2]],
                               labels.id = 1:n, cex.oma.main = 1.15, 
                               sub.caption = "")
    
    dev.hold()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))  
    # Normality
    QQ_One
    QQ_Two
    cat("Press [enter] to continue")
    line <- readline()
    hist_One
    mtext("Histogram", 3, 0.25, cex = 1)
    hist_Two
    mtext("Histogram", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    # Homoscedasticity
    residFit_One
    residFit_Two
    cat("Press [enter] to continue")
    line <- readline()
    # Linearity
    yFit_One
    abline(lm(as.numeric(y_One) ~ as.numeric(fitted_One)),col = "red",lwd = 1.5)
    mtext("Transformed observed vs Fitted", 3, 0.25, cex = 1)
    yFit_Two
    abline(lm(as.numeric(y_Two) ~ as.numeric(fitted_Two)),col = "red",lwd = 1.5)
    mtext("Transformed observed vs Fitted", 3, 0.25, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    par(old.par)
    scatter_One
    mtext("Scatter plot", 3, 0.25, outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    scatter_Two   
    mtext("Scatter plot", 3, 0.25, outer = TRUE, cex = 1)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 1))
    par(mfrow = c(1, 2))
    cooks_One
    cooks_Two
    cat("Press [enter] to continue")
    line <- readline()
    scaleLoc_One
    scaleLoc_Two
    cat("Press [enter] to continue")
    line <- readline()
    residLev_One
    residLev_Two
    par(old.par)
    dev.flush()
    
  } else if (inherits(x$orig_mod, "lme")) {
    resid <- residuals(x$orig_mod, level = 0, type = "pearson")
    residt <- residuals(x$trafo_mod, level = 0, type = "pearson")
    
    QQ_resid_orig <- NULL
    QQ_resid_trafo <- NULL
    residFit_orig <- NULL
    residFit_trafo <- NULL
    QQ_sranef_trafo <- NULL
    QQ_sranef_orig <- NULL
    
    
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