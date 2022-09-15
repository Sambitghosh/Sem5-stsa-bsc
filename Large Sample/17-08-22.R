
rm(list=ls())
set.seed(seed=987654321)
k=seq(500,10000,25);k
e=0.0001
n=100;
A=matrix(0,nrow=length(k),ncol=n);A
f=1;
p=rep(0,length(k))
Pr = function(k)
{
	p=rbinom(1,1,0.5)
	if(p==0)
		return (0.5^k)
	else  return(-0.5^k)
}
for(i in k)
{
	A=matrix(0,nrow=i, ncol=n)
	for(j in 1:i)
	{
		A[j,]=replicate(n,Pr(j))
	}
	#print(A)
	v=apply(A,2,mean)
	#print(v)
	p[f] = mean(ifelse(abs(v)>e , 1,0))
	f=f+1
}
p
plot(k,p, type='l')





















sample_gen = function(k)
{
	r = runif(1,0,1)
	if(r < 0.5)
	{
		return(-(0.5)^k)
	}else
	{
		return((0.5)^k)
	}
}
n = seq(10,1000,10)
m = 0

for(i in 1:length(n))
{
	s = 0
	for(j in 1:n[i])
	{
		s[j] = sample_gen(j)
	}
	m[i] = mean(s)
}

plot(n, m, type = 'l', 
	main = 'WLLN for gien distribuiton',
	xlab = 'Sample sizes', ylab = 'Sample means',
	las = 1, font.main = 7, font.lab = 7, font.axis = 2,
	col = 'red', cex.main = 2, cex.lab = 1.2)







