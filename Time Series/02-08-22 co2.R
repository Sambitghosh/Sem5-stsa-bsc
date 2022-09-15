#HW for co2:

#Ratio to trend Method:
rm(list=ls())

A=decompose(co2)
unique(A$seasonal)
Seasonal_Indices = A$figure;Seasonal_Indices


yr=1959:1997
data = data.frame("Year" = rep(1959:1997, each = 12), "Months" = rep(month.name, length(yr)), "Co2" = array(co2))
View(data)
plot(data$Co2,type='l')

#Yearly trend equation:
yavg=aggregate(Co2~Year,data=data,mean)$Co2;yavg

#Linear trend fitting:
L1=lm(yavg ~ I(yr-1958));L1
a0=L1$coefficients[1];a0
a1=L1$coefficients[2];a1
		
#Linear Trend equation yearly on annual averages: T_t = a0 + a1*t, where t = 1,2,...   so origin = midpoint of 1958
#Monthly Trend values: Trend = a0 + (a1/12)t, t=1,2,.....,468
Ti=a0 + a1/12*((6:(12*39+5))+0.5);Ti
#Origin : 15th July, 1958
plot((1:(12*39)),Ti, type='l')


#Multiplication Model:
#detrendisation:
dt=data$Co2*100/Ti;dt
D=matrix(dt, byrow = T, ncol = 12);D
Si = apply(D,2,mean);Si			#Unadjusted Seasonal Indices
Sif= Si*1200/sum(Si);Sif		#Adjusted Seasonal Indices
St=data.frame("Month"=month.name, "Seasonal_Indices"=Sif);St
View(St)

#Additive Model:
#detrendisation:
dt=data$Co2-Ti;dt
D=matrix(dt, byrow = T, ncol = 12);D
Si = apply(D,2,mean);Si			#Unadjusted Seasonal Indices
Sif= Si - sum(Si)/12;Sif		#Adjusted Seasonal Indices
St=data.frame("Month"=month.name, "Seasonal_Indices"=Sif);St
View(St)





#Ratio to Moving Averages Method:
v=data$Co2;v
a1=rep(0,length(v)-(12-1));a1
for(i in 1:length(a1))
{
	a1[i] = mean(v[i:i+11])	
}
a1
length(a1)
Ti=rep(0,(length(a1)-1));Ti
for(i in 1:length(Ti))
	Ti[i]=(a1[i]+a1[i+1])/2
Ti
length(Ti)


#Multiplication Model:
#detrendisation:
dt=data$Co2*100/Ti;dt
plot(1:468,dt,type='l')
D=matrix(c(rep(0,6),dt,rep(0,6)), byrow = T, ncol = 12);D
Si = apply(D,2,sum)/(length(yr)-1);Si			#Unadjusted Seasonal Indices
Sif= Si*1200/sum(Si);Sif					#Adjusted Seasonal Indices
St=data.frame("Month"=month.name, "Seasonal_Indices"=Sif);St


#Additive Model:
#detrendisation:
dt=data$Co2-Ti;dt
plot(1:468,dt,type='l')
D=matrix(c(rep(0,6),dt,rep(0,6)), byrow = T, ncol = 12);D
Si = apply(D,2,sum)/(length(yr)-1);Si			#Unadjusted Seasonal Indices
Sif= Si - sum(Si)/12;Sif					#Adjusted Seasonal Indices
St=data.frame("Month"=month.name, "Seasonal_Indices"=Sif);St




