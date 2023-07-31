#install readxl package and load the package
install.packages("fastDummies")



#load the packages
library(plyr)
library(FSA)
library(FSAdata)
library(magrittr)
library(plotrix)
library(moments)
library(dplyr)
library(readxl)
library(knitr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(fastDummies)

#load the dataset
patient <- read_excel("C:\\Users\\anohl\\OneDrive\\Documents\\Northeastern University\\Fall 2022\\ALY6010 - Probability Theory and Introductory Statistics\\Module 6\\Module 5-6 R Practice Assignment-Data.xls")
patient

#clean the dataset
patient1 <- na.omit(mutate_all(patient, ~ifelse(. %in% c("N/A", "null", ""),  NA, .)))
patient1


#create Dummy variable with 
patient_treat_dum <- dummy_cols(patient1, select_columns = 'treatment', remove_selected_columns = TRUE)
patient_treat_dum


#scatterplot with multiple regression lines
fit1 <- lm(outcome~hrt_months+treatment,data=patient1)
equation1=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]+coef(fit1)[3]}

ggplot(patient1, aes(y = outcome, x = hrt_months, color = treatment)) + 
  geom_point() +
  stat_function(fun = equation1,
                geom = "line",
                color = scales::hue_pal()(2)[1]) +
  stat_function(fun = equation2,
                geom = "line",
                color = scales::hue_pal()(2)[2]) + 
  labs("Predicting outcome using treatment and hrt_months")

#extract each subset of each treatment which is equal to one 
Gabapentin_dumtreat <- subset(patient_treat_dum, subset=(patient_treat_dum$treatment_Gabapentin=="1"))
Gabapentin_dumtreat
Placebo_dumtreat <- subset(patient_treat_dum, subset=(patient_treat_dum$treatment_Placebo=="1"))
Placebo_dumtreat

#plot for each dummy subset

plot_gabapentin_dummy <- ggplot(Gabapentin_dumtreat, 
                                aes(x = hrt_months, y = outcome,
                                    colour = time))+ 
  geom_point() + geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Predicting outcome with dummy Gabapentin treatment")
plot_gabapentin_dummy

plot_placebo_dummy <- ggplot(Placebo_dumtreat, 
                             aes(x = hrt_months, y=outcome,
                                 colour=time))+ geom_point() + 
  geom_smooth(method='lm', se=TRUE) + 
  labs(title="Predicting outcome with dummy Placebo treatment")

plot_placebo_dummy
