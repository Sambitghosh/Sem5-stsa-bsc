rm(list=ls())
setwd("C:/Users/user/Documents/Xaviers/Prac Sem 5/Time Series")
d1=read.csv("Book.csv")
total=d1[,2][1:468];total
d1
plot(d1[,2],type="l")
#there is a dominant upward trend
#there are seasonal fluctuations
# there are no cyclic pattern

M1=matrix(total[1:468],byrow=T,ncol=12);M1



yearly_avg=apply(M1,1,mean);yearly_avg
t1=1:39
plot(t1,yearly_avg,type="l",main="Yearly averages plot")
t=seq(7,474,1)

#linear
lm(yearly_avg~t1)
a0=-120.3
a1=345.1/12
Y=-120.3+345.1*t1
T1=a0+a1*(t-0.5)

MAPE1=mean(abs(yearly_avg-Y)/yearly_avg);MAPE1

#quadratic
lm(yearly_avg~t1+I(t1^2))
Y=993.958+182.077*t1+4.077*t1^2
a0=993.958
a1=182.077/12
a2=4.077/144
T2=a0+a1*(t-0.5)+a2*(t-0.5)^2

MAPE2=mean(abs(yearly_avg-Y)/yearly_avg);MAPE2
#hence we choose quadratic trend equation
detrended=total/T2*100;detrended

M2=matrix(detrended,ncol=12,byrow=T);M2

USI=apply(M2,2,mean);USI
sum(USI)->S

ASI=USI*1200/S;ASI
sum(ASI)


for(i in 1 :length(ASI))
{M1[,i]=M1[,i]/ASI[i]
}
deson=as.vector(t(M1));deson

plot(c(1:468),deson,type="l",main="Deseasonalised data")

#DETRENDATION
lm(deson~time)
Y1=
time=1:468
lm(deson~time+I(time^2))
trend=(1.079e+01)+(1.538e-01)*time+(2.866e-04)*time^2
residuals=deson/trend #removing the trend 

plot(residuals, type='l',main="Residual plot")
par(mfrow=c(2,2))
plot(total,type="l",main="Monthly electrical production",xlab="Months",ylab="Monthly electric production")
plot(ASI,type="b",main="Seasonal indices",xlab="Months",ylab="S_t")
matplot(1:468,matrix(c(deson,trend),ncol=2),type="l",main="Trend curve over deseasonalised data",ylab="values",xlab="months")
plot(residuals, type='l',main="Residual plot",xlab="Months")


#HIGHLIGHTS/STEPS

#plot the entire data
#comment on type of model and the diff components present
#with monthly data we first decide whether to use RTT or RTMA

#in RTMA we use decompose function..remember the data set is to be in the time series format
#from the output we get the ASI
#deseasonalise the original series
#having obtained the deseasonalised series we make a plot of the series and fit one apt trend eqn
#we obtain the deseasonalised detrended series i.e. the residual series by eliminating trend component from the deson series


#in RTT method we first obtain the yearly averages and fit an apt trend equation
#from the yearly trend equation we come back to the monthly trend eqn
#special attention to be given to the origin and unit
#next we obtain the monthly trend values and detrend the original series
#we obtain the avg corr to each month over the years from thye detrended series

#these averages should give us the USI.then get ASI
#next proceed as RTMA method



