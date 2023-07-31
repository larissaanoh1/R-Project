
title: "Larissa Anoh"
"R practice 5"



Part1

#load the packages
library(plyr)
library(FSA)
library(FSAdata)
library(magrittr)
library(plotrix)
library(moments)
library(dplyr)


#install readxl package and load the package
install.packages("readxl")
library(readxl)

#load the dataset
patient <- read_excel("Northeastern University/Fall 2022/ALY6010 - Probability Theory and Introductory Statistics/Module 5/Module 5-6 R Practice Assignment-Data.xls")
patient

#summary the patient dataset
summary(patient)

#view if there is any missing data
library(skimr)
skim(patient)

#clean the dataset
patient1 <- na.omit(mutate_all(patient, ~ifelse(. %in% c("N/A", "null", ""),  NA, .)))
patient1

#predicted values
fit <- lm(patient1$hrt_months~patient1$outcome, data = patient1)
fitted(fit)

#error term
residuals(fit)

#plot the hrt_months and outcome in patient dataset and add line
plot(patient1$hrt_months, patient1$outcome, xlab = "hrt_months", ylab = "outcome", main = "Title of graph is “Plot 1: Patient ", pch = 16)
abline(fit)

#install and load corrplot packages 
install.packages("corrplot")
library(corrplot)

#create correlation matrix with hrt_months, wbc, age and outcome by using pearson method
cor(data.frame(patient1$hrt_months, patient1$wbc, patient1$age, patient1$outcome), method = "pearson")

#create correlation matrix with time, smoker, outcome and hrt_months by using pearson method
cor(data.frame(patient1$time, patient1$smoker, patient1$outcome, patient1$hrt_months), method = "pearson")


#create correlation matrix with hrt_months, wbc, age and outcome by using pearson method
corrplot(corr = cor(data.frame(patient1$hrt_months, patient1$wbc, patient1$age, patient1$outcome)))

#install ggcorrplot packages
install.packages("ggcorrplot")

#load ggcorrplot packages
library(ggcorrplot)


#visualize correlation matrix with hrt_months, wbc, age and outcome in the patient dataset
ggcorrplot(cor(data.frame(patient1$hrt_months, patient1$age, patient1$smoker, patient1$wbc)), lab = TRUE, title = "Heat map Correlation for patients output")

#subset the dataset by extracting the Gabapentin in the treatment column
ggcorrplot(cor(data.frame(Gabapentin_treat$hrt_months, Gabapentin_treat$age, Gabapentin_treat$smoker, Gabapentin_treat$wbc)), lab = TRUE, title = "Heat map Correlation for Gabapentin treatment")





Part2

#regression model patient dataset by using hrt_months and outcome as predictors
regression1 <- lm(patient1$hrt_months~patient1$outcome, data = patient1)

#view regression model summary
summary(regression1)


#regression model patient dataset by using hrt_months and age as predictors
regression2 <- lm(patient1$hrt_months~patient1$age, data = patient1)
regression2 <- lm(patient1$outcome~patient1$hrt_months, data = patient1)
summary(regression2)
#view regression model summary
summary(regression2)

#multiple regression model patient dataset by using age, outcome, wbc as predictors
regression3 <- lm(formula = patient1$hrt_months~patient1$age + patient1$outcome + patient1$wbc, data = patient1)

#view regression model summary
summary(regression3)


