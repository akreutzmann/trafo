# lm ---------------------------------------------------------------------------
data("eusilcA_Vienna")
modelVienna <- lm(eusilcA_Vienna$eqIncome ~ eusilcA_Vienna$eqsize +
                    eusilcA_Vienna$cash)

library(simFrame)
data("eusilcP")



bx_cx(modelVienna, method = "ml")
bx_cx(modelVienna, method = "skew", lambdarange = c(0.4,0.5))
bx_cx(modelVienna, method = "div.ks")
bx_cx(modelVienna, method = "div.cvm")
bx_cx(modelVienna, method = "div.kl")


# lme --------------------------------------------------------------------------

library(laeken)
data(eusilc)


library(nlme)
modelAustria <- lme(eqIncome ~ pb220a + py050n, random = ~ 1 | db040, data = eusilc, 
                    na.action = na.omit)

modelAustria <- lme(eusilc$eqIncome ~ eusilc$pb220a + eusilc$py050n, 
                    random = ~ 1 | eusilc$db040, 
                    na.action = na.omit)


modelAustria <- lme(eqIncome ~ pb220a + py050n, 
                    random = ~ 1 | db040, data = eusilc, 
                    na.action = na.omit)

bx_cx(modelAustria, method = "skew")
bx_cx(modelAustria, method = "div.kl")
bx_cx(modelAustria, method = "div.cvm")
bx_cx(modelAustria, method = "div.ks")
