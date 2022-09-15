rm(list=ls())
set.seed(seed=1234)
e=0.0001
R=100
n=seq(10,50000,1000);n
mu=1
#Normal(mu=6,sigma^2=1)
j=1;
#a = replicate(R,mean(rcauchy(100,mu,0.001)));a
p= rep(0,length(n));p
#mean(ifelse(abs(a-mu)>e,1,0))
for(i in n)
{
	a = replicate(R,mean(rnorm(i,mu,0.01)))
	p[j]=mean(ifelse(abs(a-mu)>e,1,0))
	print(p[j])
	j=j+1
}		
p
plot(n,p,type="b")


#Median
rm(list=ls())
set.seed(seed=1234)
e=0.0001
R=100
n=seq(10,10000,1000);n
mu=1
#Normal(mu=1,sigma=0.005)
j=1;
#a = replicate(R,mean(rnorm(100,mu,0.001)));a
#mean(ifelse(abs(a-mu)>e,1,0))
p= rep(0,length(n));p

for(i in n)
{
	a = replicate(R,median(rnorm(i,mu,0.005)))
	p[j]=mean(ifelse(abs(a-mu)>e,1,0))
	print(p[j])
	j=j+1
}		
p
plot(n,p,type="b")






rm(list=ls())
set.seed(seed=1234)
e=0.001
R=100
n=seq(1000,60000,1000);n
mu=0
j=1;
a = replicate(R,mean(rcauchy(1000,mu,0.001)));a
p= 0;p
mean(ifelse(abs(a-mu)>e,1,0))
for(i in n)
{
	a = replicate(R,mean(rcauchy(i,mu,0.005)))
	print(a)
	p[j]=mean(ifelse(abs(a-mu)>e,0,1))
	print(p[j])
	j=j+1
}		
p
plot(n,p,type="b")

mean(ifelse(abs(a-mu)>e,1,0))

#CONSISTENCY:
rm(list=ls())
set.seed(seed=1234)
e=0.001
R=100
n=seq(1000,60000,1000);n
j=1;
a1=0;
mu=6;
for(i in n)
{
	a1[j] = mean(rcauchy(i,mu,0.1))
	j=j+1;
}
a1
plot(n,a1,type='l')	










