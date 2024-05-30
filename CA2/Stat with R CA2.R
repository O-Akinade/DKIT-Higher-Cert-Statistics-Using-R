library(MASS)
str(Melanoma)
Melanoma
help("Melanoma")


sex = factor(Melanoma$sex,labels = c("Female", "Male"))
status = factor(Melanoma$status,labels = c( "Dead","Alive","Dead_Other "))
ulcer = factor(Melanoma$ulcer,labels = c( "Absence","Presence"))
time=Melanoma$time
age=Melanoma$age
year=Melanoma$year
thickness=Melanoma$thickness



#Melanoma$sex
sex
ftable(sex)
#Melanoma$status
status
ftable(status)
#Melanoma$ulcer
ulcer
ftable(ulcer)
#Melanoma$time
time

#Melanoma$age
age

#Melanoma$year
year

#Melanoma$thickness
thickness

#Question 1a.

#Tables (Status VS Time, Sex, Age, Year, Thickness, Ulcer)

status_time =table(status,time)
status_time_add =addmargins(table(status,time))
status_time
status_time_add



status_age =table(status,age)
status_age_add =addmargins(table(status,age))
status_age
status_age_add


status_year =table(status,year)
status_year_add =addmargins(table(status,year))
status_year
status_year_add


status_thickness =table(status,thickness)
status_thickness_add =addmargins(table(status,thickness))
status_thickness
status_thickness_add


status_sex =table(status,sex)
status_sex_add=addmargins(table(status,sex))
status_sex
status_sex_add
prop.table(status_sex_add)
prop.table(status_sex_add,1)
prop.table(status_sex_add,2)



status_ulcer =table(status,ulcer)
status_ulcer_add =addmargins(table(status,ulcer))
status_ulcer
status_ulcer_add
prop.table(status_ulcer_add)
prop.table(status_ulcer_add,1)
prop.table(status_ulcer_add,2)

#Question 1a..
#Plot of Tables (Status VS Time, Sex, Age, Year, Thickness, Ulcer)

status_time_plot = plot(status,time, xlab="Status",
main="Status VS Time",col=3:5)
legend("topleft",legend=c("Dead","Alive", "Dead_Other"),fill=3:5)


status_age_plot = plot(status,age, xlab="Status",
main="Status VS Age",col=3:5)
legend("topleft",legend=c("Dead","Alive", "Dead_Other"),fill=3:5)

status_year_plot = plot(status,year, xlab="Status",
main="Status VS Year",col=3:5)
legend("topleft",legend=c("Dead","Alive", "Dead_Other"),fill=3:5)

status_year_plot = plot(status,thickness, xlab="Status",
main="Status VS Thickness",col=3:5)
legend("topleft",legend=c("Dead","Alive", "Dead_Other"),fill=3:5)

status_sex =table(status,sex)
status_sex_add=addmargins(table(status,sex))
status_sex
status_sex_add

status_ulcer =table(status,ulcer)
status_ulcer_add =addmargins(table(status,ulcer))
status_ulcer
status_ulcer_add

y




# Question 1d
#Factor(sex,status,ulcer)
sex = factor(Melanoma$sex,labels = c("Female", "Male"))
status = factor(Melanoma$status,labels = c( "Dead","Alive","Other_Dead "))
ulcer = factor(Melanoma$ulcer,labels = c( "Absence","Presence"))

#Flat table of Status, Ulcer and sex
#Melanoma$sex
ftable(sex)
#Melanoma$status
ftable(status)
#Melanoma$ulcer
ftable(ulcer)

# All Together
ftable(status,ulcer,sex)

plot(yrs.since.phd,yrs.service)



#status: Categorical(normal)
#sex: Categorical (normal)
#year: Categorical (ordinal)
#ulcer: Categorical(normal)


#time: Numerical (continuous) 
#age: Numerical (continuous)
#thickness: Numerical (continuous)

plot(time,age,main="Survival time in days(Time) VS Patients Age in Year (Age)")

plot(time,thickness,main="Survival time in days(Time) VS Tumour Thickness in mm (Thickness)")

plot(age,thickness,main="Patients Age in Year (Age) VS Tumour Thickness in mm (Thickness)")

plot(age,time,main="Patients Age in Year (Age) VS Survival time in days(Time)")

plot(thickness,time,main=" Tumour Thickness in mm (Thickness) VS Survival time in days(Time)")

plot(thickness,age,main=" Tumour Thickness in mm (Thickness) VS Patients Age in Year (Age)")



ftable(status,ulcer,sex)
