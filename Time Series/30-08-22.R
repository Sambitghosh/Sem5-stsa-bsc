#Ratio to Moving Averages Method (Additive Model):
rm(list=ls())
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

dt=data$Co2[7:462]-Ti;dt
length(dt)
D=matrix(c(rep(0,6),dt,rep(0,6)), byrow = T, ncol = 12);D
Si = apply(D,2,sum)/(length(yr)-1);Si			#Unadjusted Seasonal Indices
Sif= Si - sum(Si)/12;Sif	
S=rep(Sif,39);S

ds=v-S;ds





ds1=co2 - decompose(co2)$seasonal;
ds1
par(mfrow=c(2,1))
plot(ds1,type='l')
lm(ds1~c(1:468))
lm(v~c(1:468))
res=ds1-predict(lm(ds1~c(1:468)));res
plot(res)


plot(v,type='l')