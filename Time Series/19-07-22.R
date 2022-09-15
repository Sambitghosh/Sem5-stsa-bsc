rm(list=ls())
D=(co2);D
dim(D)
plot(D,xlim=c(1955,1999),type='l')
yavg=aggregate(D,1,mean);yavg
#D1=matrix(co2, nrow=39)
#apply(D1,1, mean)

plot(1959:1997,yavg,type="b");
plot(predict(yavg,c(1959:1997)))


