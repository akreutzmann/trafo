trafo_lm <- trafo_lm(object = modelVienna, trafo = "boxcox", 
                     method = "ml", lambdarange = c(0,2), plotit = TRUE, 
                     std = FALSE)

print(trafo_lm)
summary(trafo_lm)
plot(trafo_lm)


trafo_lm_log <- trafo_lm(object = modelVienna, trafo = "log", 
                     method = "ml", lambdarange = c(0,2), plotit = TRUE, 
                     std = FALSE)

print(trafo_lm_log)
summary(trafo_lm_log)
plot(trafo_lm_log)





trafo_lme <- trafo_lme(object = modelVienna2, trafo = "box.cox", 
                     method = "reml", lambdarange = c(-2,2), plotit = FALSE, 
                     std = FALSE)

print(trafo_lme)
summary(trafo_lme)
plot(trafo_lme)




trafo_lme_log <- trafo_lme(object = modelVienna2, trafo = "log", 
                       method = "reml", lambdarange = c(-2,2), plotit = FALSE, 
                       std = FALSE)

print(trafo_lme_log)
summary(trafo_lme_log)
plot(trafo_lme_log)








