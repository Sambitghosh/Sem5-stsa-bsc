rm(list=ls())
setwd("C:/Users/LAB4_39/Desktop");
D=read.csv("Book.csv",header=F);D
plot(D$V2,type='l')
D$V2[1:468]
D1=ts(D$V2[1:468],freq=12);D1
ds=D1/decompose(D1,type="multiplicative")$seasonal;ds
plot(ds)
t=1:(12*39);t
#summary(lm(ds~t))
MAPE1=mean(abs(resid(lm(ds~t+I(t^2)))/ds));MAPE1			#Quadratic
MAPE2=mean(abs(resid(lm(ds~t))/ds));MAPE2					#Linear

res1 = ds/predict(lm(ds~t+I(t^2)));res1
res2 = ds/predict(lm(ds~t));res2
#matplot(t,(matrix(c(res1,res2),ncol=2)),type='l', ylab="Residuals", main = "Residual Plot")
plot(res1)












sunspots
plot(sunspots)













