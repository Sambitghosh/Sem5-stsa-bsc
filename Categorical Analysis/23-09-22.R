rm(list=ls())
setwd("C:/Users/LAB4_39/Desktop")
D = na.omit(read.csv("Data.csv"));D
length(D$age)
attach(D)
D$literacy = replace(D$literacy,which(D$literacy==2),0)
D$gender = replace(D$gender,which(D$gender==2),0)
D
X = D$age;X
Y = D$literacy;Y
Z = D$gender;Z
table(Y,Z)
M1 = glm(Y~X,family=binomial(link="logit"));M1
deviance(M1)
piL = exp(M1$coefficients[1]+M1$coefficients[2]*X)/(1+exp(M1$coefficients[1]+M1$coefficients[2]*X))
M2 = glm(Y~X,family=binomial(link="probit"));M2
deviance(M2)
piP = pnorm(M2$coefficients[1]+M2$coefficients[2]*X)
Data = data.frame(X,piL,piP);Data
D1 = Data[order(Data$X, decreasing = F),]
par(mfrow = c(1,2))
plot(D1$X,D1$piL,type='l',lwd = 2,main = "Logistic Regression", xlab = "Age", ylab = "pi")
plot(D1$X,D1$piP,type='l',lwd = 2, main = "Probit Regression", xlab = "Age", ylab = "pi")
?plot
?glm
?deviance