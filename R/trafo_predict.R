#' Predict method for linear models with transformed dependent variable
#'
#' The function returns predicted values based on the linear model. The 
#' predicted values are back-transformed corresponding to the transformation 
#' used in the model. Note that the back-transformation can induce a bias.
#' 
#' @param object an object of type \code{trafo_mod}.
#' @param newdata an optional data frame in which to look for variables with 
#' which to predict. If omitted, the fitted values are used. 
#' @param se.fit a switch indicating if standard errors are required.
#' @param scale scale parameter for std. error calculation.
#' @param df degrees of freedom for scale. 
#' @param interval type of interval calculation.
#' @param level tolerance/confidence level. 
#' @param type type of prediction. In this version, only the response can be 
#' predicted. Thus, the default is set to "response". 
#' @param na.action function determining what should be done with missing value 
#' in newdata. The default is to predict NA. 
#' @param pred.var the variance for future observations to be assumed for 
#' prediction intervals. 
#' @param weights variance weights for prediction. This can be a numeric vector 
#' or a one-sided model formula. In the latter case, it is interpreted as an 
#' expression evaluated in newdata.
#' @param ... further arguments passed to or from other methods.
#' @return A vector of predictions or a matrix of predictions and bounds with 
#' column names fit, lwr and upr if interval is set. 
#' If se.fit is \code{TRUE}, a list with the following components is returned: fit, 
#' se.fit, residual.scale, df
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Compare untransformed and transformed model
#' trafo_lm(object = lm_cars, trafo = "bickeldoksum", method = "skew", 
#' lambdarange = c(1e-11, 2))
#' 
#' # Get predictions in the back-transformed scale
#' trafo_predict(trafo_lm)
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export


trafo_predict <- function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
                     interval = c("none", "confidence", "prediction"), level = 0.95, 
                     type = "response", 
                     #terms = NULL, 
                     na.action = na.pass, 
                     pred.var = res.var/weights, weights = 1, ...) 
{
  object <- object$trafo_mod
  
  tt <- terms(object)
  if (!inherits(object, "lm")) 
    warning("calling predict.lm(<fake-lm-object>) ...")
  
  # When the old data is used for prediction
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
    mmDone <- TRUE
    
    
    # A term that is added to the regression
    offset <- object$offset
  }
  # This is if new data is included
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action, 
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses"))) 
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset"))) 
      for (i in off.num) offset <- offset + eval(attr(tt, 
                                                      "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset)) 
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  
  
  # Number of observations
  n <- length(object$residuals)
  # Number of predictors
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p) {
    qr.lm(object)$pivot[p1]
  }
  # Warning if new data contains less explanatory variables as original data
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) {
    warning("prediction from a rank-deficient fit may be misleading")
  } 
  
  # Get regression parameters  
  beta <- object$coefficients
  # Get predictions
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  
  # If an offset is set it is added to the prediction
  if (!is.null(offset)) {
    predictor <- predictor + offset
   } 
  
  # For the interval three arguments are possible: no interval, confidence 
  # interval and prediction interval  
  interval <- match.arg(interval)
  
  # Prediction interval
  if (interval == "prediction") {
    if (missing(newdata)) 
      warning("predictions on current data refer to _future_ responses\n")
    
    # If weights are included in the lm object but not added as argument
    if (missing(newdata) && missing(weights)) {
      w <- weights.default(object)
      if (!is.null(w)) {
        weights <- w
        warning("assuming prediction variance inversely proportional to weights used for fitting\n")
      }
    }
    # If prediction variance is not given it is assumed
    if (!missing(newdata) && missing(weights) && !is.null(object$weights) && 
        missing(pred.var)) {
      warning("Assuming constant prediction variance even though model fit is weighted\n")
    } 
      
    
    # ???
    if (inherits(weights, "formula")) {
      if (length(weights) != 2L) 
        stop("'weights' as formula should be one-sided")
      d <- if (missing(newdata) || is.null(newdata)) 
        model.frame(object)
      else newdata
      weights <- eval(weights[[2L]], d, environment(weights))
    }
  }
  
  
  # Type can be response or terms
  # type <- match.arg(type)
  if (se.fit || interval != "none") {
    w <- object$weights
    
    # Calculation of residual variance
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
   } else { 
     scale^2
   }
    
    # For the one type choice
    if (type != "terms") {
      if (p > 0) {
        XRinv <- if (missing(newdata) && is.null(w)) {
          qr.Q(qr.lm(object))[, p1, drop = FALSE]
        } else {
          X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1,p1]) 
        }
        ip <- drop(XRinv^2 %*% rep(res.var, p))
      } else {
        ip <- rep(0, n)
      } 
    }
  }
  
  # For the second type choice - we only allow for the prediction of the response
  # if (type == "terms") {
  #   if (!mmDone) {
  #     mm <- model.matrix(object)
  #     mmDone <- TRUE
  #   }
  #   aa <- attr(mm, "assign")
  #   ll <- attr(tt, "term.labels")
  #   hasintercept <- attr(tt, "intercept") > 0L
  #   if (hasintercept) 
  #     ll <- c("(Intercept)", ll)
  #   aaa <- factor(aa, labels = ll)
  #   asgn <- split(order(aa), aaa)
  #   if (hasintercept) {
  #     asgn$"(Intercept)" <- NULL
  #     avx <- colMeans(mm)
  #     termsconst <- sum(avx[piv] * beta[piv])
  #   }
  #   nterms <- length(asgn)
  #   if (nterms > 0) {
  #     predictor <- matrix(ncol = nterms, nrow = NROW(X))
  #     dimnames(predictor) <- list(rownames(X), names(asgn))
  #     if (se.fit || interval != "none") {
  #       ip <- matrix(ncol = nterms, nrow = NROW(X))
  #       dimnames(ip) <- list(rownames(X), names(asgn))
  #       Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
  #     }
  #     if (hasintercept) {
  #       X <- sweep(X, 2L, avx, check.margin = FALSE)
  #     }
  #       
  #     unpiv <- rep.int(0L, NCOL(X))
  #     unpiv[piv] <- p1
  #     for (i in seq.int(1L, nterms, length.out = nterms)) {
  #       iipiv <- asgn[[i]]
  #       ii <- unpiv[iipiv]
  #       iipiv[ii == 0L] <- 0L
  #       predictor[, i] <- if (any(iipiv > 0L)) 
  #         X[, iipiv, drop = FALSE] %*% beta[iipiv]
  #       else 0
  #       if (se.fit || interval != "none") 
  #         ip[, i] <- if (any(iipiv > 0L)) 
  #           as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii, 
  #                                                       , drop = FALSE])^2 %*% rep.int(res.var, 
  #                                                                                      p)
  #       else 0
  #     }
  #     if (!is.null(terms)) {
  #       predictor <- predictor[, terms, drop = FALSE]
  #       if (se.fit) 
  #         ip <- ip[, terms, drop = FALSE]
  #     }
  #   }
  #   
  #   
  #   else {
  #     predictor <- ip <- matrix(0, n, 0L)
  #   }
  #   attr(predictor, "constant") <- if (hasintercept) 
  #     termsconst
  #   else 0
  # }
  
  
  if (interval != "none") {
    tfrac <- qt((1 - level)/2, df)
    hwid <- tfrac * switch(interval, confidence = sqrt(ip), 
                           prediction = sqrt(ip + pred.var))
    if (type != "terms") {
      predictor <- cbind(predictor, predictor + hwid %o% 
                           c(1, -1))
      colnames(predictor) <- c("fit", "lwr", "upr")
    }
    # Terms is always NULL
    # else {
    #   if (!is.null(terms)) 
    #     hwid <- hwid[, terms, drop = FALSE]
    #   lwr <- predictor + hwid
    #   upr <- predictor - hwid
    # }
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
    
    # Type is always response
    # if (type == "terms" && !is.null(terms) && !se.fit) {
    #   se <- se[, terms, drop = FALSE]
    # }
      
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
    if (se.fit) 
      se <- napredict(na.act, se)
  }
  
  # Type is always response
  # if (type == "terms" && interval != "none") {
  #   if (missing(newdata) && !is.null(na.act)) {
  #     lwr <- napredict(na.act, lwr)
  #     upr <- napredict(na.act, upr)
  #   }
  #   list(fit = predictor, se.fit = se, lwr = lwr, upr = upr, 
  #        df = df, residual.scale = sqrt(res.var))
  # } else 
  if (se.fit) 
    list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
  else predictor
}



weights.default <- function(object, ...) 
{
  wts <- object$weights
  if (is.null(wts)) 
    wts
  else napredict(object$na.action, wts)
}


qr.lm <- function(x, ...) 
{
  if (is.null(r <- x$qr)) 
    stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
  r
}