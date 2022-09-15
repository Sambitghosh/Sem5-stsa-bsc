rm(list=ls())

p0=c(2,5,1.5,10,8)
p1=c(2.5,4,2,12,5.5)
q0=c(4000,500,1500,250,2500)
q1=c(4500,800,900,260,5000)
AMpricerel=mean(p1/p0);AMpricerel
P01L=weighted.mean(p1,q0)/weighted.mean(p0,q0);P01L
Q01L=weighted.mean(q1,p0)/weighted.mean(q0,p0);Q01L
P01Pa=weighted.mean(p1,q1)/weighted.mean(p0,q1);P01Pa
Q01Pa=weighted.mean(q1,p1)/weighted.mean(q0,p1);Q01Pa
P01F=sqrt(P01Pa*P01L);P01F
Q01F=sqrt(Q01Pa*Q01L);Q01F
P01EM=weighted.mean(p1,q0+q1)/weighted.mean(p0,q0+q1);P01EM
Q01EM=weighted.mean(q1,p0+p1)/weighted.mean(q0,p0+p1);Q01EM
Value=mean(p1*q1)/mean(q0*p0);Value

#Checking time and factor reversal test of Fisher's index number:
P10L=weighted.mean(p0,q1)/weighted.mean(p1,q1);P10L
Q10L=weighted.mean(q0,p1)/weighted.mean(q1,p1);Q10L
P10Pa=weighted.mean(p0,q0)/weighted.mean(p1,q0);P10Pa
Q10Pa=weighted.mean(q0,p0)/weighted.mean(q1,p0);Q10Pa
sqrt(P01L*P10L*P01Pa*P10Pa)        #(P01F*P10F)
sqrt(Q01L*Q10L*Q01Pa*Q10Pa)	     #(Q01F*Q10F)
#So, yes fischer's index number satisfies time reversal test

sqrt(P01L*Q01L*P01Pa*Q01Pa)	     #(P01F*Q01F)
#equal to Value, So, fischer satisfies factor reversal test



#Checking time and factor reversal test of Edgeworth-Marshall's index number:
P10EM=weighted.mean(p0,q0+q1)/weighted.mean(p1,q0+q1);P10EM
Q10EM=weighted.mean(q0,p0+p1)/weighted.mean(q1,p0+p1);Q10EM
P01EM*P10EM
Q01EM*Q10EM
#Time reversal test

P01EM*Q01EM                          
#Factor Reversal test

