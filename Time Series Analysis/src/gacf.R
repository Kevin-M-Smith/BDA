
##############################
# ACF AND PACF GGPLOT STYLE
##############################

# ggacf <- function(x){
#   gacf = acf(x, plot=FALSE, lag.max=120)
#   gacf.df = with(gacf, data.frame(lag, acf))
#   gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
#   
#   q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
#   q = q + xlim(c(0,120))
#   q = q + geom_hline(aes(yintercept = 0))
#   q = q + geom_segment(mapping = aes(xend = lag), yend = 0)
#   q = q + theme_bw()
#   q = q + geom_hline(aes(yintercept = c(sig,-1*sig)), linetype=2)
#   q = q + ylab("$\\rho_k$")
#   q = q + xlab("lag, $k$")
# }
# 
# ggpacf <- function(x){
#   gacf = pacf(x, plot=FALSE, lag.max=120)
#   gacf.df = with(gacf, data.frame(lag, acf))
#   gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
#   
#   q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
#   q = q + xlim(c(0,120))
#   q = q + geom_hline(aes(yintercept = 0))
#   q = q + geom_segment(mapping = aes(xend = lag), yend = 0)
#   q = q + theme_bw()
#   q = q + geom_hline(aes(yintercept=c(sig,-1*sig)), linetype=2)
#   q = q + ylab("$\\alpha_k$")
#   q = q + xlab("lag, $k$")
# }

ggacf <- function(x){
  gacf = acf(x, plot=FALSE, lag.max=120)
  gacf.df = with(gacf, data.frame(lag, acf))
  gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
  
  q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
  q = q + xlim(c(0,120))
  q = q + geom_hline(aes(yintercept = 0))
  q = q + geom_segment(mapping = aes(xend = lag), yend = 0, lwd = 1)
  q = q + theme_bw()
  q = q + geom_hline(aes(yintercept = c(sig,-1*sig)), linetype=2)
  q = q + ylab(expression(rho[k]))
  q = q + xlab("lag k")
}

ggpacf <- function(x){
  gacf = pacf(x, plot=FALSE, lag.max=120)
  gacf.df = with(gacf, data.frame(lag, acf))
  gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
  
  q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
  q = q + xlim(c(0,120))
  q = q + geom_hline(aes(yintercept = 0))
  q = q + geom_segment(mapping = aes(xend = lag), yend = 0, lwd = 1)
  q = q + theme_bw()
  q = q + geom_hline(aes(yintercept=c(sig,-1*sig)), linetype=2)
  q = q + ylab(expression(alpha[k]))
  q = q + xlab("lag k")
  q
}

###########################
#  PLOT EMP. and TH. ACF
##########################
ggacf.th <- function(x, fit){
  
  coefs = coef(fit)
  print(coefs)
  ar = grep("ar", names(coefs))
  ma = grep("ma", names(coefs))
  theory <- ARMAacf(ar = coefs[ar], ma = coefs[ma], lag.max=120)
  
  gacf = acf(x, plot=FALSE, lag.max=120)
  gacf.df = with(gacf, data.frame(lag, acf))
  gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
  
  gacf.th = with(gacf, data.frame(lag, acf = theory))
  rownames(gacf.th) <- seq(0, 120)
  
  q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
  q = q + xlim(c(0,120))
  q = q + geom_hline(aes(yintercept = 0))
  q = q + geom_segment(data = gacf.df, mapping = aes(xend = lag), yend = 0, lwd=1)
  q = q + theme_bw()
  q = q + geom_hline(aes(yintercept = c(sig,-1*sig)), linetype=2)
  q = q + ylab(expression(rho[k]))
  
  q = q + geom_segment(data = gacf.th, mapping = aes(y = 0, yend = acf, 
                                                     x = lag + 0.2, 
                                                     xend = lag + 0.2), lwd = 1, color="red")

  q = q + xlab("lag k")
  q
}

###########################
#  PLOT EMP. and TH. PACF
##########################
ggpacf.th <- function(x, fit){
  
  coefs = coef(fit)
  ar = grep("ar", names(coefs))
  ma = grep("ma", names(coefs))
  theory <- ARMAacf(ar = coefs[ar], ma = coefs[ma], lag.max=120, pacf = TRUE)
  
  gacf = pacf(x, plot=FALSE, lag.max=120)
  gacf.df = with(gacf, data.frame(lag, acf))
  gacf.df$sig = qnorm((1 + 0.95)/2)/sqrt(length(x))
  
  gacf.th = with(gacf, data.frame(lag, acf = theory))
  
  q <- ggplot(data = gacf.df, mapping = aes(x = lag, y = acf))
  q = q + xlim(c(0,120))
  q = q + geom_hline(aes(yintercept = 0))
  q = q + geom_segment(data = gacf.df, mapping = aes(xend = lag), yend = 0, lwd=1)
  q = q + theme_bw()
  q = q + geom_hline(aes(yintercept = c(sig,-1*sig)), linetype=2)
  q = q + ylab(expression(rho[k]))
   
  q = q + geom_segment(data = gacf.th, mapping = aes(y = 0, yend = acf, 
                                      x = lag + 0.2, 
                                      xend = lag + 0.2), lwd = 1, color="red")
  
 q = q + xlab("lag k")
 q
}

