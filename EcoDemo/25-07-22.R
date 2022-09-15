rm(list=ls())
#Q1)
age=c("0","1-4","5-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-");
mpop=c(29.8,109.3,126.1,198.2,150.8,156.9,139.5,110.0,70.1,45.4,13.7)
fpop=c(28.5,104.9,120.7,189.7,142.7,151,138.3,106.7,80.9,54.5,18.1)
mdth=c(807,192,88,182,247,284,565,4230, 2083,3308,2195)
fdth=c(607,138,65,82,117,203,425,746,1464,2650,2621)
data=data.frame(age,mpop, fpop, mdth, fdth);data
mcdr=1000*(sum(mdth)/sum(mpop*1000));mcdr
fcdr=1000*(sum(fdth)/sum(fpop*1000));fcdr
msdr=mdth/(mpop);msdr    #1000*mdth/(mpop*1000)
fsdr=fdth/(fpop);fsdr
m=matrix(c(msdr,fsdr),ncol=2);m
matplot(m,type="l",lty=c(1,2))



#Q2)
age=c("0-5","5-10","10-20","20-40","40-60","60-80","80-")
popA=c(9736,8349,17101,37298,20746,6298,472)
ms=c(8.9,1.19,1.07,2.22,5.21,6.32,38.98)
CDRSt=37.4 #per thousand

CDRA=CDRSt*11/10;CDRA
c=CDRSt/(1000*sum(ms*popA)/sum(popA));c
StDRA=c*CDRA;StDRA


#Q3)

pop=c(20883,86376,161376,146641,157661,142341,115083,88220,55840,21751,3828) 	#Country population - take as standard
mA=c(26.884,1.027,0.4,0.801,1.289,2.921,7.940,18.643,41.93,90.794,185.648)		#age specific death rates of A per thousand
mB=c(43.182,2.136,0.786,1.907,3.078,4.962,10.352,20.812,41.934,91.313,199.942)	#age specific death rates of B per thousand
data=data.frame(msA,msB)
mA*pop/1000
StDRA=sum(1000*sum((mA/1000)*pop)/sum(pop));StDRA	#per thousand
StDRB=sum(1000*sum((mB/1000)*pop)/sum(pop));StDRB	#per thousand



