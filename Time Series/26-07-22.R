rm(list=ls())
setwd("F:/Prac Sem 5")
dat=read.csv("dat.csv");dat
attach(dat)

D=dat[3:38,]				#Ignoring the first two observations i.e. the oldest 2 values
m=12
S1=sum(D[1:12,]$Population)
S2=sum(D[13:24,]$Population)
S3=sum(D[25:36,]$Population)
D1=S2-S1
D2=S3-S2
D=D2/D1
c=D^(1/m)
b=(c-1)*D1/(c*(D-1)^2)
a=(S1-D1/(D-1))/m

Tt=a+b*c^(1:36)

MAPE=mean(abs((Tt-dat[3:38,]$Population)/dat[3:38,]$Population))*100;MAPE			#The percent error


#GOMPERTZ:
xt=log(dat[3:38,]$Population)
S1=sum(xt[1:12])
S2=sum(xt[13:24])
S3=sum(xt[25:36])
D1=S2-S1
D2=S3-S2
D=D2/D1
c1=D^(1/m)
b1=(c1-1)*D1/(c1*(D-1)^2)
a1=(S1-D1/(D-1))/m
K=exp(a1)
a=exp(b1)
b=c1
Tt1=K*a^(b^(1:36))
MAPE=mean(abs((Tt1-dat[3:38,]$Population)/dat[3:38,]$Population))*100;MAPE


#LOGISTIC:
xt=(dat[3:38,]$Population)^(-1)
S1=sum(xt[1:12])
S2=sum(xt[13:24])
S3=sum(xt[25:36])
D1=S2-S1
D2=S3-S2
D=D2/D1
c1=D^(1/m)
b1=(c1-1)*D1/(c1*(D-1)^2)
a1=(S1-D1/(D-1))/m
k=1/a1
a=log(b1/a1)
b=log(c1)
Tt2=k/(1+exp(a+b*1:36))
MAPE=mean(abs((Tt2-dat[3:38,]$Population)/dat[3:38,]$Population))*100;MAPE
C=matrix(c(Tt,Tt1,Tt2,dat[3:38,]$Population),ncol=4)
colnames(C)=c("Modified","Gompertz","Logistic","Observed");C
matplot(1953:1988,C,lty=c(1,2,3,4),type="l",lwd=3,col=c(1,2,3,4),xlab="Years", ylab="Population", main="Fitting the Population from 1953-88 in 3 different curves")
legend("topright", legend=c("Modified","Gompertz","Logistic","Observed"),lty=c(1,2,3,4),col=c(1,2,3,4))

