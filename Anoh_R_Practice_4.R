---
title: "Larissa Anoh Module 4 practice"
output: word_document
date: "2022-11-28"
---

  
Question1

#Install.package
install.packages("MASS")



#load package
library(MASS)



#Loading of the cats dataset


data(cats)
cats


# First six head of the dataset


head(cats)
tail(cats)

#Scatter plot for cats
plot(cats$Hwt, cats$Bwt, col = cats$Sex, main = "Heart weight vs Boday weight of cats")


# Subsetting the male cats data set


malecats <- subset(cats, subset=(cats$Sex=="M"))
malecats



#
tail(cats)





tail(cats)



# Subsetting the female cats data set
femalecats <- subset(cats, subset=(cats$Sex=="F"))
femalecats






#conduct t.test 
t.test(malecats$Bwt, femalecats$Bwt)



#Conduct t.test with a confidencial level of 99
t.test(malecats$Bwt, femalecats$Bwt, conf.level = 0.99)



Question2



# Student data 
Student <- c("student1", "student2", "student3", "student4", "student5", "student6", "student7", "student8", "student9", "student10")



#week before workshop data 
weekbef <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
weekbef



#week after workshop data 
weekfollow <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
weekfollow



#create data frame with above data
dfmeditation <- data.frame(Student, weekbef, weekfollow)
dfmeditation 



#conduct t.test
t.test(dfmeditation$weekbef, dfmeditation$weekfollow, paired = TRUE, alternative = "two.sided")


#Conduct t.test with a confidencial level of 99
t.test(dfmeditation$weekbef, dfmeditation$weekfollow, paired = TRUE, alternative = "two.sided", conf.level = 0.99)









