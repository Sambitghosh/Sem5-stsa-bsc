rm(list=ls())
set.seed(seed=1234)


r=1000
n=1:2000;
a=c(0,0);
e=0.000005
for (i in n)
	a[i]=mean(abs(rbinom(r,1,1/i))>e) #mod(Xn-0) value of r reptition of n if > e
a
plot(n,a,type='l',ylab="probability")

#If Xi ~ U[0,theta], then max{X1,X2,X3,.....Xn} converges in probability to theta
rm(list=ls())
set.seed(seed=1234)
theta=1;
r=1000
n=seq(1,10000,100);n
a=rep(0,length(n));a
e=0.0005
#mean(replicate(r,ifelse(abs(max(runif(i,0,theta))-theta)>e,1,0)))
#abs(max(runif(100,0,theta))-theta)>e
for (i in 1:length(n))
{
	a[i]=mean(replicate(r,ifelse(abs(max(runif(n[i],0,theta))-theta)>e,1,0))) #mod(X(n)-theta) value of sample size of i if > e
}
a
plot(n,a,type='l',lwd=3,ylab="probability")
	

#If Xi ~ U[0,theta], then min{X1,X2,X3,.....Xn} converges in probability to 0
rm(list=ls())
set.seed(seed=1234)
theta=1;
r=1000
n=seq(1,10000,100);n
a=rep(0,length(n));a
e=0.0005

for (i in 1:length(n))
{
	a[i]=mean(replicate(r,ifelse(abs(min(runif(n[i],0,theta))-0)>e,1,0))) #mod(X(n)-theta) value of sample size of i if > e
}
a
plot(n,a,type='l',lwd=3,ylab="probability")
	





