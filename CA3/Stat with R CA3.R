A = rbinom(10000,25,0.16)
head(A)
table(A)
table(A)/10000  

barplot(table(A)/10000,main="PDF", xlab="X",
        ylab="Probability",col=2)
mean(A)

cumsum(table(A))/10000

round(pbinom(0:25,25,0.16),4)

plot(0:13,cumsum(table(A))/10000,type='b',
       xlab="X",ylab="Probability", main="CDF")
 
dbinom(10,25,0.16)

pbinom(1,25,0.16)

dbinom(25,25,0.16)


pbinom(5,25,0.16) - pbinom(1,25,0.16)

library(MASS)
str(Melanoma)
Melanoma


mean(Melanoma$age)
round(sd(Melanoma$age),0)

(65-52)/17
