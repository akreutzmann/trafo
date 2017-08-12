trafo_lm <- trafo_lm(object = modelVienna, trafo = "box.cox", 
                     method = "ml", lambdarange = c(0,2), plotit = TRUE, 
                     std = TRUE)

print(trafo_lm)
summary(trafo_lm)
plot(trafo_lm)





trafo_lme <- trafo_lme(object = modelAustria, trafo = "box.cox", 
                     method = "reml", lambdarange = c(-2,2), plotit = FALSE, 
                     std = FALSE)

print(trafo_lme)
summary(trafo_lme)
plot(trafo_lme)









