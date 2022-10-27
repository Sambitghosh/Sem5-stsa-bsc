PROBLEM SET 3:
#Q2(d)
rm(list=ls())
I1995=c(212.45,328.06,345.89,173.41,201.35)
w=c(65.3,4.8,8.5,7.6,13.8)
D=data.frame(Groupindex=I1995,Weight = w)
CLI1995=weighted.mean(I1995,w);CLI1995
Change = CLI1995*2400/100;Change
#His new salary in 1995 is Rs. 4950 but his expense at par with the spending on items in 1981 is Rs. 5369.23

#Expenditure in each group buying as same as 1981 wrt the need of the question:
exp = (Change/sum(w))*w;exp
exp[length(exp)] = 4950 - sum(exp[-length(exp)])
NewWt = exp/(sum(exp))*100
D$NewWeight = NewWt
View(D)





#PROBLEM SET 4:
#Q2:
rm(list=ls())
percentofhousehold = c(9.67,34.36,38.78,8.45,5.18,1.64,1.07,0.85)
percentincomeshare = c(2.44,16.86,33.87,13.18,12.98,6.32,5.72,8.63)
annualdisposableincome = c("<500","500-1000","1000-2000","2000-3000","3000-5000","5000-7000","7000-10000",">10000")
View(data.frame(annualdisposableincome,percentofhousehold,percentincomeshare))

P_x = c(0,cumsum(percentofhousehold)/sum(percentofhousehold));
Y_x = c(0,cumsum(percentincomeshare)/sum(percentincomeshare));Y_x
matplot(P_x,cbind(P_x,Y_x), type = 'l')
LorentzArea = (1-sum((P_x[-1]-P_x[-9])*(Y_x[-1]+Y_x[-9])))/2;LorentzArea
GinniCoeff = 2*LorentzArea; GinniCoeff

