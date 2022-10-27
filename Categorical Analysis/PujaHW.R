#Hpertension ~ BMI
rm(list=ls())
setwd("C:/Users/user/Documents/Xaviers/Prac Sem 5/Categorical Analysis")
Da = read.csv("Da.csv")
y = Da$A;y
n = length(y)
x = Da$BMI
L = glm(y~x, family = binomial(link = "logit"))
B0 = L$coefficients[1]
B = L$coefficients[2]
pi = exp(B0+B*x)/(1+exp(B0+B*x))
Data = data.frame(y,x,pi)
Data = Data[order(Data$pi,decreasing=T),]
plot(Data$x,Data$pi,lwd = 2, type = 'l')
D = Data;D
TPR = rep(0,n)
FPR = rep(0,n)
Py1 = sum(D$y);Py1
Py0 = n - Py1;Py0
for(i in 1:n)
{
	Y_hat = c(rep(1, i),rep(0,n-i))
	TPR[i] = sum(ifelse(Y_hat == 1 & D$y == 1, 1, 0))
	FPR[i] = sum(ifelse(Y_hat == 1 & D$y == 0, 1, 0))
}
TPR = TPR/Py1;
FPR = FPR/Py0;
Cutoff = TPR*(1-FPR);Cutoff
a = which(Cutoff == max(Cutoff))
plot(D$x, Cutoff, type = 'l', main = "OC Curve")
D$Y_hat = c(rep(1,a),rep(0,n-a));
D$Sl = as.numeric(row.names(D))
Final = D[order(D$Sl, decreasing = F),];Final
View(D)
View(Final[,-5])






