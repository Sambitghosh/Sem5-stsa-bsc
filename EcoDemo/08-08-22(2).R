rm(list=ls())
#Problem Set 3:
#Q1)
#Groups:Food, Clothing and Footwear, Housing, Transport and vehicles, Durable household goods, Other items


w=c(350,156,187,108,76,123)
I2004= c(117,113,118,112,102,121)		#Group Index for 6 commodities for current year 2004 and base year 2000
#(a):
weighted.mean(I2004,w);
#115.375 units of money in current year 2004 can purchase the same amount of quantity as 100 units of money in base year 2000	
#(b):
x=c(120,118,117,111,125)
z=(1000*119.5-sum(w[-3]*x))/187			#Group index for Housing in 2005
I2005=c(x[1:2],z,x[3:5]);I2005

#(c):
#Family spends Rs. 5000 per month in 2004 and their foods consumed and amount are same for 2005
#So, we find the money spent in 2000 over same pattern of food, i.e. 5000*(100/117). Now, we find pice in 2005 = 5000*(100/117)*(120/100)
monthly_price_in_2005_on_food = 5000*(120/117); monthly_price_in_2005_on_food
So, our new price is Rs. 5128.205 on food per month in 2005


#(d):



#Q2):
#1981 is base year


I1995=c(212.45,328.06,345.89,173.41,201.35)
w=c(65.3,4.8,8.5,7.6,13.8)
#(a):
CLI1995=weighted.mean(I1995,w);CLI1995

#(b):
100/CLI1995;

#(c):
#1981: 24 units are bought by 2400
#1995: 24 units are bought by CLI1995*2400/100
CLI1995*2400/100
#His salary is 4950 < 5396.228

#(d):
#Given: expenditure 


			