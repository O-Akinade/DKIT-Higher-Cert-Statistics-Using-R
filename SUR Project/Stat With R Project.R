#Environment codes#####

mydata=read.csv("Survey_Data1.csv")
str(mydata)
attach(mydata)

#Year_Birth####

hist(Year_Birth)
mean(Year_Birth)
median(Year_Birth)


#Education####

#class(Education)
education= as.factor(Education)
education

plot(education,main = paste("Plot of Education"))

prop.table(table(education))


#Marital_Status Relations####

#class(Marital_Status)
marital_status= as.factor(Marital_Status)
marital_status

plot(marital_status,main = paste("Plot of Marital Service"))

prop.table(table(marital_status))


#Marital_Status VS Income
boxplot(Income~marital_status,ylab=" ($)",
          col=rep(c(2:6),each=1))

#Marital_Status VS Income
boxplot(Income~Education,ylab=" ($)",
        col=rep(c(2:6),each=1))


#Marital_Status VS year Birth
boxplot(Year_Birth~marital_status,ylab="Years",
        col=rep(c(2:6),each=1))

#Marital_Status VS Children

MS_C=table(Children,marital_status)
MS_C

barplot(t(MS_C), xlab="Marital Status", main = "Number of
Children in each Marital Status", col=c(2:6),
        beside=TRUE)
 legend("topright",legend=c("Divorced","Married","Single","Together","Widow"),fill=c(2:6))


 
 
 
 
#Income####
 
inc=boxplot(Income, xlab = "Income ($)")
inc
 
pairs(~Income + Wines_Products+Fruits_Products+Meat_Products+Fish_Products+Sweet_Products,
       data=mydata)
 
 
#Children####

hist(Children)

prop.table(table(Children))
Children
#0          1          2          3 
#0.27919799 0.51027569 0.18796992 0.02255639 




#Produts####

boxplot(Wines_Products, Fruits_Products, Meat_Products, Fish_Products, Sweet_Products,ylab="Total",col=c(2:7),
        main = "product Purchases Breakdown", names=c("Wines_Products", "Fruits_Products", "Meat_Products", "Fish_Products", "Sweet_Products"))

hist(MntFishProducts)




#Children VS Product Bought####

pairs(~Children + Wines_Products+ Fruits_Products+ Meat_Products+ Fish_Products+ Sweet_Products,
      data=mydata)


#Deals_Claimed ####

dc=boxplot(Deals_Claimed, ylab= ("Totla Claimed"))
dc

pairs(~Deals_Claimed + Wines_Products+ Fruits_Products+ Meat_Products+ Fish_Products+ Sweet_Products,
      data=mydata)
#Product Bought##


#Purchase Methods####

boxplot(PurchasesFromStore,PurchasesFromCatalog,PurchasesFromWeb,col=c(2:7),
        main = "product Purchases method", names=c("PurchasesFromStore","PurchasesFromCatalog","PurchasesFromWeb"))


sum_wp=sum(PurchasesFromWeb)
sum_wp
sum_cp=sum(PurchasesFromCatalog)
sum_cp
sum_sp=sum(PurchasesFromStore)
sum_sp

purchase_no=sum(sum_wp,sum_cp,sum_sp)
purchase_no

wp_prop=(sum_wp/purchase_no*100)
wp_prop

cp_prop=(sum_cp/purchase_no*100)
cp_prop

sp_prop=(sum_sp/purchase_no*100)
sp_prop

wp_prop+cp_prop+sp_prop

#Most of the product are sold in store(46%) follow by website Purchases(33%) then catalog purchases(21%).


#1 sample hypothesis tests to for means####

#Ho : The income mean of the Data sample is the income mean of population that buys from the business
#H1: The income mean of the Data sample is not the income mean of population that buys from the business

#Assumptions of the one-sample

#Independent random sample = True
#Large sample size = True
#Normally distributed population:
hist(Income)
#=true
#Standard deviation of the population known= False

#this mean A T-Test will be done

#N = 2196

#mean = 51530.28
Income_mean=mean(Income)
Income_mean

#sample_SD = 20584.98
Income_sample_SD = sd(Income)
Income_sample_SD

help(t.test)


t_income = t.test(Income, mu=51484.11)
t_income
t_income$p.value
t_income$conf.int

#p.value > 0.05 we fail to reject the H0

#> t_income$p.value
#[1] 0.9999929
#> t_income$conf.int
#[1] 50622.67 52345.54
#attr(,"conf.level")
#[1] 0.95

t.test(Income, mu=51484.11, alternative="two.sided")

#1 sample hypothesis tests to for proportions####

#Ho= product are sold in store makes up 46% of the total population purchases 
#H1= product are sold in store does not makes up 46% of the total population purchases

#Ho: p = 0.46
#hi: p not = 0.46
#p is the true portion of product brought from the store

sum_wp=sum(NumWebPurchases)
sum_wp
sum_cp=sum(PurchasesFromCatalog)
sum_cp
sum_sp=sum(NumStorePurchases)
sum_sp

purchase_no=sum(sum_wp,sum_cp,sum_np)
purchase_no

p = 0.46
n = purchase_no
n
phat = sum_sp /n

phat

#np>=10, n(1-p)>=10

n*p
n*(1-p)

#Assumptions:
#Observations are independent = True
#Sample size is large enough = True

SE=sqrt(p*(1-p)/n)
SE

z= (phat-p)/SE
z

#since 2-sided, it can be more extreme in both directions
#P(z<-1.331762) or P(z>1.331762)

p.value<-2*pnorm(z)
p.value

#p.value>0.05, fail to reject H0. 


#2 sample hypothesis tests to for means independent sample####

# Research Question:
#Does customer with Married and Together marital statues have the same income mean?

#Null hypothesis: the population mean income mean is the same in both groups 
#H0: µ1 = µ2
#or equivalently
#H0: µ1 - µ2 = 0
#Alternative hypothesis: the population mean income mean is not the same in both groups 
#H1: µ1 ¹ µ2
#or equivalently
#H0: µ1 - µ2 ¹ 0
# µ1, µ2 = the population mean with marital statues Married  and those with marital statues Together, respectively


income_married= Income[marital_status=="Married"]
income_married
hist(income_married)

income_Together= Income[marital_status=="Together"]
income_Together
hist(income_Together)

boxplot(income_Together,income_married)

#The sample 
sd(income_married)
sd(income_Together)
var(income_married)
var(income_Together)

#Assumption = Meet
#Two samples must be independent and random = True 
#The underlying populations must not be skewed (normally distributed)=True
#The two population must be equal spread=True

t.test(income_married, income_Together)

#p-value=0.98>0.05, therefore fail to reject, and based on data, no evidence to suggest a difference.
#95% confidence interval, 95% the true population mean difference  for Income between Married and Together marital statues lies between  -2177.326 and 2141.297
#Confidence interval also leads to the same conclusion as it does cover 0

#2 sample hypothesis tests to for means/proportions independent sample####


#H0: no.of children  and martial status are independent of each other
#H1: no.of children  and martial status are dependent of each other

marital_childrens_table= table(marital_status,Childrens)
marital_childrens_chisq<-chisq.test(marital_childrens_table)
str(marital_childrens_chisq)
marital_childrens_chisq$expected
marital_childrens_chisq$p.value

###looking expected values I have enough enough data to
#meet my assumptions as expected values >=5

marital_childrens_chisq$p.value

#p-value=0.038 < 0.05,  no.of children  and martial status are independent of each other are disease independent.





