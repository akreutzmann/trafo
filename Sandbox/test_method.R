# lm ---------------------------------------------------------------------------
data("eusilcA_Vienna")
modelVienna <- lm(eusilcA_Vienna$eqIncome ~ eusilcA_Vienna$eqsize +
                    eusilcA_Vienna$cash)

library(simFrame)
data("eusilcP")



lily_bc <- bx_cx(modelVienna, method = "ml")

box_cox <- box_cox(eusilcA_Vienna$eqIncome, lambda = 0.4690185)

all.equal(box_cox$y, as.numeric(lily_bc$yt))

(box_cox$y - as.numeric(lily_bc$yt))/box_cox$y

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
