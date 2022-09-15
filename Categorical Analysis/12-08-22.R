rm(list=ls())
set.seed(seed=987654321)
n=1000; 
mu=1; 			#
a=-2; 			#alpha
b=1.5; 		#beta
s=2;			#sigma
x=runif(n);x					#Generating n Uniform(0,1)
y=rnorm(n,a+b*x,s);y				#Generating n observation of Y
B=cov(x,y)/var(x);B				#Beta_hat
A=mean(y)-B*mean(x);A				#alpha_hat
I=matrix(c(n/s^2,sum(x)/s^2,sum(x)/s^2,sum(x^2)/s^2),nrow=2);I 
FIM=solve(I);FIM
SE_B=sqrt(FIM[2,2]);SE_B
qnorm(0.975)

ci= 0.05
B - qnorm(1-ci/2)*SE_B				#lower confidence interval
B + qnorm(1-ci/2)*SE_B				#upper confidence interval



#EMPIRICAL:



rm(list=ls())
set.seed(seed=987654321)
a=-2; 			#alpha
b=1.5; 			#beta
s=2;				#sigma

n1=c(20,50,seq(100,1000,100)); 				#n values		
bias = rep(0,length(n1));				#To store the empirical bias of Beta_hat for each n
se = rep(0,length(n1));					#To store the empirical standard error of Beta_hat for each n  
analse = rep(0,length(n1));				#To store the analytical standard error of Beta_hat for each n
mse = rep(0,length(n1));				#To store the empirical mean squared error of Beta_hat for each n
cov = rep(0,length(n1));				#To store the empirical coverage of Beta_hat for each n
lev = rep(0,length(n1));				#To store the empirical level of the given test for each n
pow = rep(0,length(n1));				#To store the empirical power of the given test for each n
R=1000; 							#The number of simulation for each n

j=1;
for(n in n1)
{
	B = rep(0,R);					#To store the beta hat of each simulation
	SE_B = rep(0,R);					#To store the standard error of beta hat of each simulation
	up = rep(0,R);					#To store the upper Confidence interval point of each simulation
	lp = rep(0,R);					#To store the lower Confidence interval point of each simulation
	ase = rep(0,R);					#To store the analytical Standard error interval point of each simulation
	ci= 0.05						#the coverage point which is also equal to the level of te test here
	for(i in 1:R)
	{
		x=runif(n);					#Generating n Uniform(0,1) covariate values (X)
		#I=matrix(c(n/s^2,sum(x)/s^2,sum(x)/s^2,sum(x^2)/s^2),nrow=2);
		FIM=solve(matrix(c(n/s^2,sum(x)/s^2,sum(x)/s^2,sum(x^2)/s^2),nrow=2))
		SE_B[i]=sqrt(FIM[2,2])				#Standard error of the Beta_hat
		y=rnorm(n,a+b*x,s)				#Generating n observation of Y
		y1 = rnorm(n,a, s)				#Generating n observation of Y for Beta = 0(Null)
		y2 = rnorm(n,a+x, s)				#Generating n observation of Y for Beta = 1(Alt)
		B[i]=cov(x,y)/var(x)				#Beta_hat
		#A=mean(y)-B*mean(x)				#alpha_hat
		ase[i] = sqrt(s^2/(n*((R-1)/R*var(x))));	
		lp[i] = B[i] - qnorm(1-ci/2)*SE_B[i]	#lower confidence interval
		up[i] = B[i] + qnorm(1-ci/2)*SE_B[i]	#upper confidence interval
		B1=cov(x,y1)/var(x)				#Beta_hat for Null hypothesis population
		B2=cov(x,y2)/var(x)				#Beta_hat for Alt hypothesis population
		lev[j] = lev[j] + ifelse(B1/SE_B[i] > qnorm(1-ci),1,0);
		pow[j] = pow[j] + ifelse(B2/SE_B[i] > qnorm(1-ci),1,0);
	}
	empirical_bias= mean(B-b)
	bias[j] = empirical_bias
	empirical_variance = (R-1)/R*var(B)
	empirical_se = sqrt(empirical_variance)
	se[j] = empirical_se
	empirical_mse = mean((B-b)^2)
	mse[j] = empirical_mse
	empirical_coverage = mean(ifelse(b > lp & b < up, 1, 0))
	cov[j] = empirical_coverage
	analse[j] = mean(ase);
	pow[j] = pow[j]/R;
	lev[j] = lev[j]/R;
	j = j+1
	
}

#Printing the graphs:
par(mfrow=c(3,2))
plot(n1,bias, main =  "Bias of Beta_hat for different n", xlab = "n", ylab = "Bias",type = 'b') 
plot(n1,mse, main =  "Mean Square error of Beta_hat for different n", xlab = "n", ylab = "MSE",type = 'b') 
plot(n1,cov, main =  "Coverage of Beta for different n", xlab = "n", ylab = "Coverage",type = 'b') 
plot(n1,se, main =  "Standard Error of Beta_hat for different n(Empirical)", xlab = "n", ylab = "Standard Error",type = 'b') 
plot(n1,lev, main =  "Level of the given test for different n(Empirical)", xlab = "n", ylab = "Level",type = 'b') 
plot(n1,pow, main =  "Power of the given test for different n(Empirical)", xlab = "n", ylab = "Power",type = 'b') 

d = data.frame(n = n1, Bias = bias, MSE = mse, SE_Analytical = analse, SE_Emperical = se, Coverage=cov, Level = lev, Power = pow)
View(d)




