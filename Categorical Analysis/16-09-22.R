rm(list=ls())
y=rpois(100,25)
x=1:100
plot(x,y, type='l')
n = length(y)
lm(y~x)$coefficients[1]
B0 = lm(y~x)$coefficients[1];B0
B = lm(y~x)$coefficients[2];B
eta = B0 + B*x;eta
mu = exp(eta);mu
g_dash = 1/mu;g_dash
z = eta + (y-mu)*g_dash;z
w = mu;w
(z~x,weights = w)$coefficients[1]
(z~x,weights = w)$coefficients[2]
??fsolve
glm(z~x,weights = w);
glm(z~x,weights = w,family=poisson())
glm(y~x,family=poisson("log"))
?glm
fit = glm.fit(x,z, weights = w)$fitted.values;fit
matplot(x, matrix(c(fit, y), ncol =2), type ='l',col = c(1,2))


library(nleqslv)









rm(list=ls())
set.seed(seed=987654321)
n=100
x = runif(n);x
Beta0 = -2 			#unknown parameter
Beta = 1.2			#unknown parameter
eta = Beta0 + Beta*x
y = exp(eta)/(exp(eta)+1);y
plot(x,y, type='l')


















#Problem Set - 4
#Q2):
rm(list=ls())
setwd("C:/Users/LAB 4/Desktop")
D1 = read.csv("data.csv", header=T)
D=na.omit(D1);D
length(D$age)


D[,1] = replace(D[,1], D[,1] == 2, 0);D
D[,2] = replace(D[,2], D[,2] == 2, 0);D
attach(D)
#Y <- Literacy (0 = not literate)
#Z <- Gender(0 = Female)




T = table(literacy,gender, dnn=c("Y","Z"));T
?table
View(as.data.frame(T))