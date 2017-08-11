get_modelt <- function(object, trans_mod, std) {
  
  
  if (inherits(object, "lm")) {
    if (std == FALSE) {
      model_frame <- object$model 
      x <- model.matrix(attr(model_frame, "terms"), data = model_frame)
      k <- ncol(x)
      yt <- trans_mod$yt
      suppressWarnings(modelt <- lm(yt ~ ., data.frame(yt = yt, x[, 2:k])))

      
    } else if (std == TRUE) {
      model_frame <- object$model 
      x <- model.matrix(attr(model_frame, "terms"), data = model_frame)
      k <- ncol(x)
      zt <- trans_mod$zt
      suppressWarnings(modelt <- lm(zt ~ ., data.frame(zt = zt, x[, 2:k])))

    }
    
    
  } else if (inherits(object, "lme")) {
  
    if (std == FALSE) {
      formula <- formula(object)
      tdata <- object$data
      tdata[paste(formula[2])] <- trans_mod$yt
      rand_eff <- names(object$coefficients$random)
      suppressWarnings(modelt <- lme(fixed = formula, data = tdata,
                                     random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                                     method = "REML",
                                     keep.data = FALSE,
                                     na.action = na.omit))
      

    } else if (std == TRUE) {
      formula <- formula(object)
      tdata <- object$data
      tdata[paste(formula[2])] <- trans_mod$zt
      rand_eff <- names(object$coefficients$random)
      suppressWarnings(modelt <- lme(fixed = formula, data = tdata,
                                     random = as.formula(paste0("~ 1 | as.factor(", rand_eff, ")")),
                                     method = "REML",
                                     keep.data = FALSE,
                                     na.action = na.omit))
      
    }
    
    
  }
  
  return(modelt = modelt)
  
  }