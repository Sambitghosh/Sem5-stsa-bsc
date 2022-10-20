rm(list=ls())
setwd("C:/Users/LAB4_39/Desktop")
data = read.csv("Cyclical.csv");data
attach(data)
str(data)				#Gives us the summary of the dataframe
names(data)				#Gives us names of the columns
x = data[,3];x
n = length(x);n
year = unique(data[,1]);year
length(year)

#Seems to be a non-linear trend present, cycles aand seasonality are present
plot(x,type='l')

#IMP Fn ALERT!!! : names
#DESEASONALIZATION:
t1 = year - 2000
y_avg = aggregate(Plantation.Forest ~ Year, data, mean)$Plantation.Forest;y_avg
L = lm(y_avg~t1+I(t1^2))
#Quadratic fit for yearly trend equation; Origin : 1st July, 2000
b=0
a = L$coefficients
for( i in 1:length(a))
	b[i] = a[i]*(1/4)^(i-1)
#Quarterly trend equation; Origin: 3rd quarter midpoint of 2000
t = 2:57
fit = b[1];
for(i in 2:length(a))
	fit = fit + b[i]*(t+1/2)^(i-1)
fit
#matplot(matrix(c(fit,x), ncol =2),type ='l')
dt = x/fit;dt
M = matrix(dt, ncol = 4,byrow=T);M
USI = apply(M,2,mean);USI
ASI  = USI/sum(USI)*4
ASI
#sum(ASI)
#rep(ASI,length(year))
dts = dt/rep(ASI,length(year));dts				#deseasonalization on the already detrended values only not the original values
matplot(matrix(c(fit,x), ncol =2),type ='l')



#dst = residuals
mu = 30:50
A=B=0;
for(i in 1:length(mu))
{
	A[i] = 2*(sum(dts*cos(2*pi*(t-1)/mu[i])))/n;A
	B[i] = 2*(sum(dts*sin(2*pi*(t-1)/mu[i])))/n;A
}
par(mfrow = c(1,1))
S.mu = A^2+B^2
plot(mu,S.mu,type='b')
abline(v=39)
lambda = mu[which(S.mu==max(S.mu))];lambda

#RESIDUAL ANAL:
t = 1:39
u = dts[t];u
A0 = mean(u);A0
A1 = 2*(sum(u*cos(2*pi*t/lambda)))/39;A1
A2 = 2*(sum(u*sin(2*pi*t/lambda)))/39;A2
ut = A0 + A1*cos(2*pi*t/lambda) + A2*sin(2*pi*t/lambda)
plot(t,ut, type = 'l')

#PLOTS:
par(mfrow = c(2,2))
plot(x,type='l')
plot(ASI,type = 'l')
matplot(matrix(c(fit,x), ncol =2),type ='l')
plot(dts[t], type='l')
lines(ut)
 