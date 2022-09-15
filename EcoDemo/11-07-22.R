rm(list=ls())

#1.)
comm = c("Rice","Wheat","Pulses","Sugar","Salt","Milk","Oil","Fish","Egg","Tea");
price61 = c(0.75,0.46,1.03,1.25,0.15,1.20,3.85,3.55,2.05,0.06);
price71 = c(1.4,0.98,2.1,1.8,0.25,1.85,6.20,6.50,3.65,0.1);
aefm61 = c(40.2,15.8,12.6,7.2,0.6,19.4,14.2,30.8,5.2,10.3);
#aefm61: average expenditure/family/month in 61
d= data.frame(comm,price61,price71,aefm61);d
relprice = price71/price61;relprice
simpAM=mean(relprice);simpAM
simpGM=exp(mean(log(relprice)));simpGM
WtAM=weighted.mean(relprice,aefm61);WtAM
WtGM=exp(sum(log(relprice)*aefm61)/sum(aefm61));WtGM
simpHM=1/mean(price61/price71);simpHM
WtHM=1/weighted.mean(price61/price71,aefm61);WtHM
simpaggr = sum(price71)/sum(price61);simpaggr
wtaggr = weighted.mean(price71,(aefm61/price61))/weighted.mean(price61,aefm61/price61);wtaggr

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             