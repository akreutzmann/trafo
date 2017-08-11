trafo_lm <- trafo_lm(object = modelVienna, trafo = "log", 
                     method = "ml", lambdarange = c(-2,2), plotit = TRUE, 
                     std = FALSE)
summary(trafo_lm)
plot(trafo_lm)


trafo_lme <- trafo_lme(object = modelAustria, trafo = "box.cox", 
                     method = "reml", lambdarange = c(-2,2), plotit = FALSE, 
                     std = FALSE)

summary(trafo_lme)







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






