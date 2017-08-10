#trafo_lm <- trafo_lm(object = modelVienna, trafo = "box.cox", 
#         method = "ml", lambdarange = c(-2,2), plotit = TRUE, 
#                     std = FALSE)


summary_trafolm <- function(object, ...) {
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  
  # Summary of original model
  orig_sum <- summary(object$orig_lm)
  
  
  # Summary of transformed model
  trafo_sum <- summary(object$trafo_lm) 
  trafo_sum$coefficients <- as.matrix(trafo_sum$coefficients[, 1])
  colnames(trafo_sum$coefficients) <- c("Estimate")
  
  return(list(trafo = trafo, 
              method = method, 
              lambdahat = lambdahat, 
              orig_sum = orig_sum, 
              trafo_sum = trafo_sum))
}



print_summary_trafolm <- function(x, ...) {
  
  cat("Information about applied transformation \n")
  cat("Transformation: ",x$trafo," \n")
  cat("Estimation method: ", x$method, " \n")
  cat("Optimal Parameter: ", x$lambdahat, " \n")
  
  cat("Summary of transformed model \n")
  print(x$trafo_sum)
  cat("Note that the standard errors are missing due to the lack of methods 
      for correct standard errors in transformed models. \n")
  
  cat("Summary of original model \n")
  print(x$orig_sum)
  
  invisible(x)
  
}

#sum_trafo <- summary_trafolm(trafo_lm)

#print_summary_trafolm(sum_trafo)


plot_trafolm <- function(x, ...) {
  
  cat("Plots of original model \n")
  plot(x$orig_lm)
  
  cat("Plots of transformed model \n")
  plot(x$trafo_lm)
  
  
}

#plot_trafolm(trafo_lm)

# + argument estim or optim_l = "estim" or value of lambda
#trans_mod <- bx_cx_new_lm(object = modelVienna, method = "ml", 
#                          lambdarange = c(-2,2), plotit = TRUE)



# Trafo summary close to the one that exists at the moment but we need more
# diagnostics






