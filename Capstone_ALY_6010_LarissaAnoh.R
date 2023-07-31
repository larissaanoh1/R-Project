

#Install Bsome packages

install.packages("BSDA")
install.packages("ggpubr")
install.packages("lubridate")

#load the packages

library(plyr)
library(FSA)
library(FSAdata)
library(magrittr)
library(plotrix)
library(moments)
library(dplyr)
library(readr)
library(BSDA)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(skimr)
library(ggpubr)
library(lubridate)

#Import weather dataset
weather <- read.csv("C:\\Users\\anohl\\OneDrive\\Documents\\Northeastern University\\Fall 2022\\ALY6010 - Probability Theory and Introductory Statistics\\Final project\\weatherHistory.csv")
weather

#Rename some variables
weather <- weather %>% rename(Temperature = Temperature..C., Wind_Speed = Wind.Speed..km.h., Apparent_Temp = 	Apparent.Temperature..C., Wind_Bear = Wind.Bearing..degrees.,	Visibility = 	Visibility..km., Pressure_mill = Pressure..millibars.)

#view if there is any missing data
skim(weather)

#Create plot to have better view and understanding of our data
par(mfrow=c(2,2))
barplot(table(weather$Summary), main = "Weather Summary", xlab = "summary", ylab = "Frequency", col = c("green", "yellow", "orange"))

barplot(table(weather$Precip.Type), main = "Type of Weather", xlab = "Weather Type", ylab = "Frequency", col = c("green", "yellow", "orange"))

barplot(table(weather$Daily.Summary), main = "Weather daily Summary", xlab = "Daily summary", ylab = "Frequency", col = c("green", "yellow", "orange"))


#create a boxplot
par(mfrow=c(2,2))
boxplot(weather$Temperature~weather$Summary, data=weather, main="Different boxplots for Summary based on Temperature", xlab="Temperature", ylab="Daily.Summary", col= c("orange", "red", "yellow"), border="brown")

boxplot(weather$Temperature~weather$Precip.Type, data=weather, main="Different boxplots for precipitation type based on temperature", xlab="Temperature", ylab="Daily.Summary", col= c("orange", "red", "yellow"), border="brown")




#Perform Men foreach variables
mean(weather$Temperature)
mean(weather$Apparent_Temp)
mean(weather$Humidity)
mean(weather$Wind_Speed)
mean(weather$Wind_Bear)
mean(weather$Visibility)
mean(weather$Pressure_mill)


#Perform standard deviation for some variables
sd(weather$Temperature)
sd(weather$Apparent_Temp)
sd(weather$Humidity)
sd(weather$Wind_Speed)
sd(weather$Wind_Bear)
sd(weather$Visibility)
sd(weather$Pressure_mill)

#Perform median for some variables
median(weather$Temperature)
median(weather$Apparent_Temp)
median(weather$Humidity)
median(weather$Wind_Speed)
median(weather$Wind_Bear)
median(weather$Visibility)
median(weather$Pressure_mill)


#Perform miniimum for some variables
min(weather$Temperature)
min(weather$Apparent_Temp)
min(weather$Humidity)
min(weather$Wind_Speed)
min(weather$Wind_Bear)
min(weather$Visibility)
min(weather$Pressure_mill)

#Perform maximum for some variables
max(weather$Temperature)
max(weather$Apparent_Temp)
max(weather$Humidity)
max(weather$Wind_Speed)
max(weather$Wind_Bear)
max(weather$Visibility)
max(weather$Pressure_mill)

#Perform variance for some variables
var(weather$Temperature)
var(weather$Apparent_Temp)
var(weather$Humidity)
var(weather$Wind_Speed)
var(weather$Wind_Bear)
var(weather$Visibility)
var(weather$Pressure_mill)


# t-test for the independent variable
t_test <- t.test(weather$Temperature, mu = 10, var.equal = FALSE)
t_test

#paired t-test with mu equal to 15
t_test <- t.test(weather$Temperature, weather$Apparent_Temp, mu = 15, paired = TRUE)
t_test

#create correlation matrix with Temperature, Apparent_Temp, Humidity, pressure, Wind_Speed
cor(data.frame(weather$Temperature,weather$Apparent_Temp, weather$Humidity, weather$Pressure_mill, weather$Wind_Speed), method = "pearson")

#create correlation matrix with Temperature, Humidity, Visibility, Wind_Bear
cor(data.frame(weather$Temperature, weather$Humidity, weather$Visibility, weather$Wind_Bear), method = "pearson")


#visualize correlation matrix withweather$Temperature, Wind_Speed, Pressure_mill, Visibility, Wind_Bear
corrplot(corr = cor(data.frame(weather$Temperature, weather$Wind_Speed, weather$Pressure_mill, weather$Visibility, weather$Wind_Bear)))

#visualize correlation matrix with Temperature, Wind_Speed, Humidity, Apparent_Temp, Wind_Bear
ggcorrplot(cor(data.frame(weather$Temperature, weather$Wind_Speed, weather$Humidity, weather$Apparent_Temp, weather$Wind_Bear)), lab = TRUE)

#simple linear regression with the independent variable (Temperature) and dependent variable (Apparent Temperature))
regression_model1 <- lm(weather$Apparent_Temp~weather$Temperature, data = weather)
summary(regression_model1)

#Multiple linear regression with variables
regression_model2 <- lm(weather$Apparent_Temp~weather$Temperature + weather$Humidity + weather$Wind_Speed, data = weather)
summary(regression_model2)


#Linear regression graph
p1<-ggplot(weather, aes(x=Temperature, y= Apparent_Temp))+
  geom_point()
p1+geom_smooth(method="lm", col="red") + 
  stat_regline_equation(label.x = 10, label.y = 250, col="red")+
  theme_bw() +
  labs(title = "Linear regression of Apparent_Temp vs Temperature",
       x = "Temperature",
       y = "Apparent_Temp")







