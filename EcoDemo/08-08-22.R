rm(list=ls())
#Problem Set-2:

#Q1)Average per capita consumption of cereals and prices of cereals in rural India for 4 time periods T1, T2, T3, T4
p0=c(37,37,32)			#Rs./Kg
p1=c(39,32,20)
p2=c(52,41,31)
p3=c(52,44,36)
q0=c(8.69,1.51,7.66)		#Kg/month
q1=c(7.94,2.73,8.19)
q2=c(7.58,2.43,7.75)
q3=c(8.28,2.78,7.71)
laas = function(u1,u2,v)
{
	sum(u1*v)/sum(u2*v)
}
I03dash = laas(p1,p0,q0)*laas(p2,p1,q1)*laas(p3,p2,q2);I03dash
I03=laas(p3,p0,q0);I03
#Note since Laspeyre's index number does not satisfy time circular test I03dash != I03




#Q2)Price and quantities of jute and tea for 4 successive years. Compute fixed base and chain index indices using laspeyres,paasche, EM and fischer
#quantity in '000 metric tonnes
#price in '0000 Rs/'000 tons
#Time period :-> 0: 1965, 1: 1966, 2: 1967, 3: 1968

p0=c(202,577)
p1=c(320,767)
p2=c(311,884)
p3=c(302,799)
q0=c(871,199)
q1=c(706,179)
q2=c(724,214)
q3=c(627,288)
index = function(u1,u2,v)
{
	sum(u1*v)/sum(u2*v)
}
I03dashlas=index(p1,p0,q0)*index(p2,p1,q1)*index(p3,p2,q2);I03dashlas
I03las=index(p3,p0,q0);I03las
I03dashpas=index(p1,p0,q1)*index(p2,p1,q2)*index(p3,p2,q3);I03dashpas
I03pas=index(p3,p0,q3);I03las
I03dashEM=index(p1,p0,(q1+q0)/2)*index(p2,p1,(q0+q1)/2)*index(p3,p2,(q3+q2)/2);I03dashEM
I03EM=index(p3,p0,q3+q0);I03EM
I03dashfis=sqrt(I03dashlas*I03dashpas);I03dashfis
I03fis=sqrt(I03las*I03pas);I03fis
