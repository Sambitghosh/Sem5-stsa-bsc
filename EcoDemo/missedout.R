rm(list=ls())
prop_popn=rep(0.1,10)
cum_prop_popn=c(0,cumsum(prop_popn));cum_prop_popn
percent_exp_urban=c(2.72,4.17,5.27,6.36,7.51,8.82,10.44,12.59,15.97,26.14)
percent_exp_rural=c(3.29,4.79,5.87,6.9,7.88,9.19,10.63,12.5,15.37,23.48)
prop_exp_urban=percent_exp_urban/sum(percent_exp_urban)
prop_exp_rural=percent_exp_rural/sum(percent_exp_rural)
cum_prop_exp_urban=c(0,cumsum(prop_exp_urban));cum_prop_exp_urban
cum_prop_exp_rural=c(0,cumsum(prop_exp_rural));cum_prop_exp_rural
plot(cum_prop_popn,cum_prop_exp_urban,type="l",ylab="cumulative proportion of income",xlab="cumulative proportion of income",main="lorenz curve",col="red")
lines(cum_prop_popn,cum_prop_popn)
lines(cum_prop_popn,cum_prop_exp_rural,col="blue")
legend("topleft",fill=c("black","red","blue"),legend=c("line of equality","lorenz curve of urban","lorenz curve for rural"))
z_i_urban=cum_prop_exp_urban[-length(cum_prop_exp_urban)];z_i_urban
z_i1_urban=cum_prop_exp_urban[-1];z_i1_urban
g_urban=1-sum(0.1*(z_i_urban+z_i1_urban));g_urban

z_i_rural=cum_prop_exp_rural[-length(cum_prop_exp_rural)];z_i_rural
z_i1_rural=cum_prop_exp_rural[-1];z_i1_rural
g_rural=1-sum(0.1*(z_i_rural+z_i1_rural));g_rural



#Q2:
rm(list=ls())
percentofhousehold = c(9.67,34.36,38.78,8.45,5.18,1.64,1.07,0.85)
percentincomeshare = c(2.44,16.86,33.87,13.18,12.98,6.32,5.72,8.63)
annualdisposableincome = c("<500","500-1000","1000-2000","2000-3000","3000-5000","5000-7000","7000-10000",">10000")
data.frame(annualdisposableincome,percentofhousehold,percentincomeshare)

P_x = c(0,cumsum(percentofhousehold)/sum(percentofhousehold));
Y_x = c(0,cumsum(percentincomeshare)/sum(percentincomeshare));Y_x
matplot(P_x,cbind(P_x,Y_x), type = 'l')
LorentzArea = (1-sum((P_x[-1]-P_x[-9])*(Y_x[-1]+Y_x[-9])))/2;LorentzArea
GinniCoeff = 2*LorentzArea; GinniCoeff







