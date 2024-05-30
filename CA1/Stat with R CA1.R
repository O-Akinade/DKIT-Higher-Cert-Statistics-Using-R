#vector creation
solar.radiation=c(12.3, 11.4, 7.1, 9.2, 10.3, 10.8, 9.5, 11.8)

#Mean of solar.radiation
mean_solar.radiation = mean (solar.radiation)
mean_solar.radiation

#Median of solar.radiation
median_solar.radiation = median (solar.radiation)
median_solar.radiation

#variance of solar.radiation
var_solar.radiation = var(solar.radiation)
var_solar.radiation

# +10 = sr10
sr10= solar.radiation + 10
sr10

#Mean of sr10
mean_sr10= mean (sr10)
mean_sr10

#Median of sr10
median_sr10 = median (sr10)
median_sr10

#variance of sr10
var_sr10=var(sr10)
var_sr10

#different of Solar.radiation to sr10
mean_sr10 - mean_solar.radiation
median_sr10 - median_solar.radiation 
var_sr10 - var_solar.radiation
2.828571 - 2.828571

#solar.radiation x -2 (sr2)
sr2= solar.radiation * -2
sr2

#Mean of sr2
mean_sr2 = mean(sr2)
mean_sr2

#Median of sr2
median_sr2 = median(sr2)
median_sr2

#Variance of sr2
var_sr2= var(sr2)
var_sr2

#different of Solar.radiation to sr2
mean_solar.radiation - mean_sr2
median_solar.radiation - median_sr2
var_solar.radiation - var_sr2

#histogram
hist_sola = hist(solar.radiation, main="Solar Radiation", col= c("blue"))
hist_sr10 = hist(sr10, main="Solar Radiation + 10", col= c("grey"))
hist_sr2 = hist(sr2, main="Solar Radiation x -2", col= c("yellow"))


#Question 3

library("MASS")
data("Melanoma")

Melanoma
help("Melanoma")

dim(Melanoma)
names(Melanoma)
str(Melanoma)
typeof(Melanoma)
Melanoma$sex



time.freq=table(Melanoma$time)
status.freq=table(Melanoma$status)
sex.freq=table(Melanoma$sex)
age.freq=table(Melanoma$age)
year.freq=table(Melanoma$year)
thickness.freq=table(Melanoma$thickness)
ulcer.freq=table(Melanoma$ulcer)



#plot of time
boxplot(Melanoma$time)
print(boxplot(Melanoma$time))
boxplot(Melanoma$time,ylab="Lenght of Survival (Days)", main= "Boxplot Of Length Of Survival")

#Stat of Melanoma$time
mean(Melanoma$time)
median(Melanoma$time)
var(Melanoma$time)

#plot of Status
status.percent=round(100*status.freq/sum(status.freq),0)
status.pie.label=paste(status.percent,"%",sep="")
pie(status.freq, main="Patients Status", labels=c(status.pie.label), col=rainbow(length(status.freq)))
legend("bottomright", c("Died By Melanoma", "Alive", "Died By Other Causes"),fill=rainbow(length(status.freq)))


#plot of sex
sex.percent= round(100*sex.freq/sum(sex.freq),0)
sex.pie.label=paste(sex.percent,"%",sep="" )
sex.pie.label
pie(sex.freq, main="Pie Chart of Sex", labels=c(sex.pie.label), col=rainbow(length(sex.freq)))
legend("bottomright", c("Female", "Male"), fill=rainbow(length(sex.freq)))



#plot of age
boxplot(Melanoma$age)
print(boxplot(Melanoma$age))
boxplot(Melanoma$age,ylab="Age in years", main= "Boxplot of Age Distrubution")

#Stat of Melanoma$age
mean(Melanoma$age)
median(Melanoma$age)
var(Melanoma$age)

#plot of year
boxplot(Melanoma$year)
print(boxplot(Melanoma$year))
boxplot(Melanoma$year,ylab="Year of Operation", main= "Boxplot of Operation Year")

#Stat of Melanoma$year
mean(Melanoma$year)
median(Melanoma$year)
var(Melanoma$year)

#plot of thickness
boxplot(Melanoma$thickness)
print(boxplot(Melanoma$thickness))
boxplot(Melanoma$thickness,ylab="Tumour Thickness(mm)", main= "Boxplot of Tumour Thickness")

#Stat of Melanoma$thickness
mean(Melanoma$thickness)
median(Melanoma$thickness)
var(Melanoma$thickness)

#plot of Ulcer
ulcer.percent=round(100*ulcer.freq/sum(ulcer.freq),0)
ulcer.percent
ulcer.pie.label=paste(ulcer.percent,"%",sep="")
pie(ulcer.freq, main="Patients With Ulcer", labels=c(ulcer.pie.label), col=rainbow(length(ulcer.freq)))
legend("bottomright", c("Absence", "Present"),fill=rainbow(length(ulcer.freq)))










