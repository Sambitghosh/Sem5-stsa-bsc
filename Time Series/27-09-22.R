rm(list=ls())
setwd("C:/Users/LAB4_43/Desktop");
D = read.csv("seaice.csv");D
attach(D)
x = D$Arctic;x
D$year  = c(rep(1990:2010,each = 12));D
y = c(1:21);y
#Eliminating Trend:
plot(D$Arctic, type = 'l', xaxt = "n")
axis(1,at = seq(0,252,12))
grid(nx = 21, ny = 0, lty =1)
yavg = aggregate(D$Arctic~D$year, D, mean)[2]


?grid
#axis(<which axis: 1 or 0(axis)>, <first value>,<last value>, <interval>)




D1 = decompose(ts(x, freq=12),type = "additive");D1
ds = x - D1$seasonal;ds
y = 1:252
L1 = lm(formula = ds~I(y))
MAPE1 = mean(abs((ds-predict(L1))/ds));MAPE1
L2 = lm(formula = ds~I(y)+I(y^2))
MAPE2 = mean(abs((ds-predict(L2))/ds));MAPE2
td = predict(L1)
res = ds - td;res
par(mfrow = c(2,2))
plot(x, type = 'l', main = "Sea Ice Data with the fitted trend") 
lines(td) 
plot(D1$figure, type = 'b', ylab = "Seasonal Indices", main = "Seasonal Indices")
plot(res,type='l', ylab = "Residuals", main = "Correllogram")
abline(h=0)
acf(as.vector(res), main = "Correllogram")


ar(res)
acf(res)[1]
acf(as.vector(res),plot = F, main = "Correllogram")[1]
#as.vector(res) is done so that its usable
