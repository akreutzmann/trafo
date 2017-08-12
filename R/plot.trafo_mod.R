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
  
  
  if (inherits(x$orig_mod, "lm")) {
    residFit_orig %<a-%  plot(x$orig_mod, which = c(1L), main = "Orig. model")
    residFit_trafo %<a-%  plot(x$trafo_mod, which = c(1L), main = "Transf. model")
    
    QQ_orig %<a-%  plot(x$orig_mod, which = c(2L), main = "Orig. model")
    QQ_trafo %<a-%  plot(x$trafo_mod, which = c(2L), main = "Transf. model")
    
    scaleLoc_orig %<a-%  plot(x$orig_mod, which = c(3L), main = "Orig. model")
    scaleLoc_trafo %<a-%  plot(x$trafo_mod, which = c(3L), main = "Transf. model")
    
    residLev_orig %<a-%  plot(x$orig_mod, which = c(5L), main = "Orig. model")
    residLev_trafo %<a-%  plot(x$trafo_mod, which = c(5L), main = "Transf. model")
    
    
    old.par <- par(mfrow = c(1, 2))
    residFit_trafo
    residFit_orig
    par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 2))
    QQ_trafo
    QQ_orig
    par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 2))
    scaleLoc_trafo
    scaleLoc_orig
    par(old.par)
    cat("Press [enter] to continue")
    line <- readline()
    old.par <- par(mfrow = c(1, 2))
    residLev_trafo
    residLev_orig
    par(old.par)
  } else if (inherits(x$orig_mod, "lme")) {
    cat("No diagnostic plots are available at the moment for linear 
        mixed models.")
  }
}