#Q-1)
rm(list=ls())
age = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")
births = c(2343,14541,16736,10210,5134,1422,93)
totalb = sum(births)
totalp = 2285800
cbr = totalb/totalp
femp1000 = c(84.79,70.01,72.66,75.92,75.1,71.62,66.66)
gfr = totalb/(1000*sum(femp1000));gfr  
asfr = births/(femp1000*1000);asfr
barplot(asfr,names.arg = age, cex.axis = 1, xlab = "Age")
plot(seq(17,47,5),asfr, type = 'b', xlab = "Age")		
tfr = sum(1000*asfr);tfr


#Q-2)
rm(list=ls())
age = c("15-19","20-24","25-29","30-34","35-39","40-44")
femp1000 = c(9,9.2,8.9,8.6,8.4,8.5)
births = c(140,1312,1067,771,468,160)
totalb = sum(births)
survfact = c(0.92,0.914,0.908,0.891,0.878,0.869);survfact
gfr = totalb/(1000*sum(femp1000));gfr  
asfr = births/(femp1000*1000);asfr
barplot(asfr,names.arg = age, cex.axis = 1, xlab = "Age")
plot(seq(17,42,5),asfr, type = 'b', xlab = "Age")		
tfr = sum(1000*asfr);tfr
grr = tfr*0.487;grr
nrr = sum(births/(femp1000*1000)*0.487*survfact);nrr