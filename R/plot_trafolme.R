plot_trafolme <- function(lambdarange, lambdaoptim, measoptim, 
                         y, formula, data, rand_eff, transfor, method) {
  
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.025)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  measvector <- sapply(lambdavector, estim_lme, y = y, formula = formula, 
                       data = data, rand_eff = rand_eff, method = method, 
                       transfor =  transfor)
  vline <- lambdaoptim
  
  if (method == "ml" | method == "reml") {
    measvector <- -measvector
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)  
    measoptim <- -measoptim
    y_lab <- "Profile log-likelihood"
    
    
  } else if (method == "skew" | method == "pskew") {
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
    y_lab <- "Skewness"
  } else if (method == "div.ks" | method == "div.cvm" | method == "div.kl") {
    data1 <- data.frame(measvector = measvector,  lambdavector = lambdavector)
    y_lab <- "Divergence"
  }
  
  #plot <- ggplot(data1, aes(x = lambdavector,
  #                          y = measvector)) + geom_line() + 
  #  geom_vline(xintercept = vline, linetype = "dashed") + 
  #  geom_hline(yintercept = measoptim, color = "red", linetype = "dashed") + 
  #  xlab(expression(lambda)) + ylab(y_lab)
  
  plot <- NULL
  abline <- NULL
  dev.flush <- NULL
  
  dev.hold()
  plot(data1$lambdavector, data1$measvector, type = "l", lwd = 1.5,
      xlab = expression(lambda), ylab = y_lab) 
  abline(h = measoptim, lty = 2, col = "red")
  abline(v = lambdaoptim, lty = 2)
  dev.flush()


  out <- list(lambdavector = lambdavector, 
              measvector = measvector)
  return(out)
  }