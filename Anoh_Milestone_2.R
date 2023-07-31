---
title: "Larissa Anoh Milestone 2"
output: word_document
date: "2022-11-27"
---

#Install some packages
install.packages("FSA")
install.packages("FSAdata")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("moments")
install.packages("BSDA")




#load some packages
library(FSA)
library(FSAdata)
library(magrittr) 
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(plotrix) 
library(moments)
library(readr)
library(BSDA)



#import dataset
dfpbc <- read.csv("deaths-from-preterm-birth-complications.csv")
dfpbc




#Rename entity variable into country variable
dfpbc <- rename(dfpbc, c(Entity = "Country", Deaths...Neonatal.preterm.birth...Sex..Both...Age..Under.5..Rate. = "Deaths.Neonatal.preterm.birth"))
dfpbc


## Subsetting the Canada Country data set
Canada <- subset(dfpbc, subset=(dfpbc$Country=="Canada"))
Canada


#Find average mean of Canada
mean(Canada$Deaths.Neonatal.preterm.birth)



#Find a correlation of death and year in Canada
cor(Canada$Deaths.Neonatal.preterm.birth, Canada$Year)



#Find standard deviation of death neonatal preterm birth in Canada
sd(Canada$Deaths.Neonatal.preterm.birth)


#Find standard deviation of years in Canada
sd(Canada$Year)




#Conducting the Z.test
z.test(Canada$Deaths.Neonatal.preterm.birth, Canada$Year, alternative = "two.sided", mu = 25, sigma.x = 8.80, sigma.y = 3.60, conf.level = 0.95)



#Conducting the t.test
t_test <- t.test(Canada$Deaths.Neonatal.preterm.birth, mu = 20)
t_test





## Subsetting the  Country of United States data set
UnitedStates<- subset(dfpbc, subset=(dfpbc$Country=="United States"))
UnitedStates




#Conducting the t.testwith mu = 40
t.test(UnitedStates$Deaths.Neonatal.preterm.birth, mu = 40)





#Conducting the t.test with mu = 25
t_test <- t.test(UnitedStates$Deaths.Neonatal.preterm.birth, Canada$Deaths.Neonatal.preterm.birth, mu = 25)
t_test



#Extract the P-value from t.test
result_p.value <- t_test$p.value
result_p.value



