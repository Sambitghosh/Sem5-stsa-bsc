rm(list = ls())
setwd("C:/Users/user/Documents/Xaviers/Prac Sem 5/Categorical Analysis")
D = read.csv("lbw.csv")
attach(D)
D$names
L = glm(low~age+race+smoke,family = binomial(link = "logit"))
#(i)
a = L$coefficients
Beta = a[2] + a[2]*age + a[3]*race + a[4]*smoke;Beta
pi = exp(Beta)/(1+exp(Beta));pi
Data = data.frame(D,pi)
View(Data)
Data = Data[order(Data$age,decreasing=F),]
plot(Data$age,Data$pi,type = "b")
#(ii)
smoke0 = Data$pi[Data$smoke == 0]
smoke1 = Data$pi[Data$smoke ==1]
summary(smoke0)
summary(smoke1)
#(iii)
Confuse = function(Data,p)
{
  Data$Y_hat = ifelse(Data$pi>p,1,0)
  T = table(Data$Y_hat,D$low, dnn=c("Predicted","Response"))
  print(T)
  return((T[2]+T[3])/sum(T))
}
Confuse(Data,mean(Data$pi))
Confuse(Data,median(Data$pi))

