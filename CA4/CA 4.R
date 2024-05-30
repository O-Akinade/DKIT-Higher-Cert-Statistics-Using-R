library(MASS)
str(survey)
survey
attach(survey)

help(survey)

#Question1


#Wr.Hnd vs Sex: Numerical Continuous Vs Categorical

mwr_hnd= Wr.Hnd[Sex=="Male"]
mwr_hnd

mean(mwr_hnd,na.rm = TRUE)
hist(mwr_hnd)

fwr_hnd= Wr.Hnd[Sex=="Female"]
fwr_hnd

mean(fwr_hnd,na.rm = TRUE)
hist(fwr_hnd)

mfwr_hnd=boxplot(mwr_hnd,fwr_hnd,names=c("Male","Female"),ylab= "Centimetres",col=4:2, main = paste("Boxplot of Male Vs Female Writing Hand Span"))

mfwr_hnd



#Pulse vs Sex: Numerical Continuous Vs Categorical

m_Pulse= Pulse[Sex=="Male"]
m_Pulse

hist(m_Pulse)

f_Pulse= Pulse[Sex=="Female"]
f_Pulse

hist(f_Pulse)

mf_pulse=boxplot(m_Pulse,f_Pulse,names=c("Male","Female"),ylab= "Pulse",col=4:2, main = paste("Boxplot of Male Vs Female Pulse"))

mf_pulse



#Age vs Sex: Numerical Continuous Vs Categorical

m_age= Age[Sex=="Male"]
m_age

f_age= Age[Sex=="Female"]
f_age

mf_age=boxplot(m_age,f_age,names=c("Male","Female"),ylab= "Age(years)",col=4:2, main = paste("Boxplot of Male Vs Female Age"))

mf_age



#Clap vs Sex: Categorical Vs Categorical

mf_clap_table=table(Clap,Sex)
mf_clap_table

mf_clap= barplot(t(mf_clap_table),xlab= "Hand On Top When Clap", 
                 main = "Frequency Hand On Top When Clap of Male Vs Female", 
                 col = 2:3,beside = TRUE)
legend("bottomright",legend=c("Female","Male"),fill=c(2,3))

mf_clap


#Exer vs Sex: Categorical Vs Categorical

mf_exer_table=table(Exer,Sex)
mf_exer_table

mf_exer= barplot(t(mf_exer_table),xlab= "Frequently of Exercises", 
                 main = "How Frequently do Male Vs Female Exercises", 
                 col = 2:3,beside = TRUE)
legend("bottomright",legend=c("Female","Male"),fill=c(2,3))

mf_exer


#Smoke vs Sex: Categorical Vs Categorical

mf_smoke_table=table(Smoke,Sex)
mf_smoke_table

mf_smoke= barplot(t(mf_smoke_table),xlab= "Frequently of smokeing", 
                 main = "How Frequently do Male Vs Female smokeing", 
                 col = 2:3,beside = TRUE)
legend("topright",legend=c("Female","Male"),fill=c(2,3))

mf_smoke


#W.Hnd vs Sex: Categorical Vs Categorical

mf_W.Hnd_table=table(W.Hnd,Sex)
mf_W.Hnd_table

mf_W.Hnd= barplot(t(mf_W.Hnd_table),xlab= "Right Vs Left Wrighting Hand ", 
                 main = "Right Vs Left", 
                 col = 2:3,beside = TRUE)
legend("bottomright",legend=c("Female","Male"),fill=c(2,3))

mf_W.Hnd


#Q2 

#H0: mu_male= mu_female
#H0: mu_male - mu_female=0 
#mu_male is true mean writing hand span for male
#mu_female is true mean writing hand span for female

#H1: mu_male not equal mu_female
#H1: mu_male - mu_female != 0 

mwr_hnd= Wr.Hnd[Sex=="Male"]
mwr_hnd

fwr_hnd= Wr.Hnd[Sex=="Female"]
fwr_hnd


#happy with the assumptions of normality and equal var.
mfwr_hnd=boxplot(mwr_hnd,fwr_hnd,names=c("Male","Female"),ylab= "Centimetres",col=4:2, main = paste("Boxplot of Male Vs Female Writing Hand Span"))

mfwr_hnd


t.test(mwr_hnd, fwr_hnd)


#pvalue=2.2e-16 < 0.05, therefore reject H0 and conclude
#that there is a signficant difference between true mean male writing hand span
#and true mean female writing hand span.
#95% CI tells us that 95% the true population mean difference for
#male and female writing hand span lie within 19.74188cm and 17.59576cm




#Q3

#H0: p_male = p_females, p_males is population proportion
# of males smoking habit and  p_females is population 
#proportion of females smoking habit
#H0: sex and smoking habit are independent of each other
#H1: sex and smoking habit are dependent of each other

mf_smoke_table=table(Smoke,Sex)
mf_smoke_table


smoke_chisq<-chisq.test(mf_smoke_table)
smoke_chisq$expected
#happy with assumptions as exp >=5

smoke_chisq
#
#p-value=0.3139 >0.05, so fail to reject H0 that sex and
#site are independent

prop.test(mf_smoke_table)


#Q4

#Exer VS Wr.Hnd

plot(Exer,Wr.Hnd,
ylab= "Centimetres",col=4:2, main = paste("Boxplot of Exer VS Student Writing Hand Span"))

#Exer VS Height

plot(Exer,Height,
ylab= "Centimetres",col=4:2, main = paste("Boxplot of Exer VS Student Height"))

#Exer VS Age
plot(Exer,Age,
ylab= "Years",col=4:2, main = paste("Boxplot of Exer VS Student Age"))

#Exer VS Pulse
plot(Exer,Pulse,
ylab= "Beats / Minute",col=4:2, main = paste("Boxplot of Exer VS Student Pulse"))


#Q5


#H0: Exercises habit and Pulse are independent of each other
#H1: Exercises habit and Pulse are dependent of each other

Exer_Pulse_table=table(Pulse,Exer)
Exer_Pulse_table


Exer_Pulse_table_chisq<-chisq.test(Exer_Pulse_table)
Exer_Pulse_table_chisq$expected
#happy with assumptions as exp >=5

Exer_Pulse_table_chisq
#
#p-value=0.3918 >0.05, so fail to reject H0 that  Exercises habit and
#Pulse are independent

prop.test(mf_smoke_table)
























