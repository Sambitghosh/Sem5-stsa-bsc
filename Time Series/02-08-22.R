rm(list=ls())
x2007=c(41,41,44,40,39,29,26,25,28,32,39,42);x2007
x2008=c(43,42,46,43,39,28,25,24,24,31,37,42);x2008
x2009=c(39,39,38,36,31,23,23,23,26,32,37,39);x2009
x2010=c(43,39,42,40,35,22,23,21,25,32,33,36);x2010
x=c(x2007,x2008,x2009,x2010);x
A=matrix(x,byrow=T,ncol=12)
z=c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec");
plot(x,type="l")
rownames(A)=2007:2010;
colnames(A)=z;A


#Ratio to Moving Average Method:
decompose(ts(x, start = c(2007, 1), end = c(2010,4), frequency = 12))
?ts
a1=rep(0,37);a1
for(i in 7:43)
{
	for(j in 0:11)
		a1[i-6]=a1[i-6]+x[i-6+j];	
}
a1=a1/12;a1
ca1=rep(0,36);ca1
for(i in 2:37)
	ca1[i-1]=(a1[i-1]+a1[i])/2
ca1
T=c(rep(0,6),ca1,rep(0,6));T
length(T)
xsub=x[c(-1:-6,-43:-48)];xsub



#(a)Multiplicative model:
Iin=xsub*100/ca1;Iin
z1=c(rep(0,6),Iin,rep(0,6));z1
I=matrix(z1,byrow=F,ncol=4);I
Si=apply(I,1,sum)/3;Si		#Unadjusted Seasonal Indices
S=sum(Si);S
Sif=Si*(1200/S);Sif		#Adjusted Seasonal Indices
sum(Sif);				
plot(Sif,type="b")
St=data.frame("Month"=month.name, "USI" = Si, "ASI"=Sif);St
View(St)

#(b)Additive Model:
Iin=xsub-ca1;Iin
z1=c(rep(0,6),Iin,rep(0,6));z1
I=matrix(z1,byrow=F,ncol=4);I
Si=apply(I,1,sum)/3;Si		#Unadjusted Seasonal Indices
S=sum(Si);S
Sif=Si-S/12;Sif		#Adjusted Seasonal Indices
sum(Sif)
plot(Sif,type="b")
St=data.frame("Month"=month.name, "USI" = Si, "ASI"=Sif);St
View(St)






#Ratio to Trend Method:
x1=apply(A,1,mean);x

#Origin: midpoint of 2006, i.e. either 1st July or 31st June, 2006, unit : 1 year
L1=lm(x1 ~ I(1:4));L1
a0=L1$coefficients[1];a0
a1=L1$coefficients[2];a1

#Monthly Trend Values:
#Ti = a0 + a1/12*t has origin 1st July or 31st June, 2006. To make the origin and henceforth the remaining values at the center of the month, we do:
#Ti = a0 + a1/12*(t-/+0.5)
Ti=a0+(a1/12)*((6:53)+0.5);Ti
#Origin : midpoint July 2006, unit : 1 month
#If we took origin as  midpoint June 2006, unit : 1 month, i.e. if (t-0.5), then to get our values, i.e. from midpoint of January 2007, we take values from 7:54



#Multiplication Model:
#detrendisation:
dt=x*100/Ti;dt
D=matrix(dt, byrow = T, ncol = 12);D
Si = apply(D,2,mean);Si			#Unadjusted Seasonal Indices
Sif= Si*1200/sum(Si);Sif			#Adjusted Seasonal Indices
St=data.frame("Month"=month.name, "USI" = Si, "ASI"=Sif);St
View(St)
plot(1:12,Sif,type='l')

#Additive Model:
#detrendisation:
dt=x-Ti;dt
plot(1:48,dt,type='l')
D=matrix(dt, byrow = T, ncol = 12);D
Si = apply(D,2,mean);Si			#Unadjusted Seasonal Indices
Sif= Si - sum(Si)/12;Sif			#Adjusted Seasonal Indices
St=data.frame("Month"=month.name,"USI"=Si, "ASI"=Sif);St
View(St)
plot(1:12,Sif,type='l')