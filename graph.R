rm(list=ls())
p = seq(0,1,0.00025)[c(-1,-length(p))]
p
y1 = log(p/(1-p));y1
y2 = qnorm(p);y2
matplot(p, matrix(c(y1,y2),ncol = 2), type='l',col = c(1,2), lty=1)
abline(h=0, v=0.5)





rm(list=ls())
#MA(1) with parameters (1,2),(1,5),(1,10)
ma1=