#Larry McQuaid
#MET CS 555
#Due: 2/5/19
rm(list=ls())

#library
library(tidyverse)
library(asbio)

#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
calories <- read.csv("CS_555_Assignment_2.csv")

colnames(calories)

#1) Summarize the data by whether children participated in the meal preparation or not. Use an
#appropriately labelled table to show the results.
aggregate(calories$Data, by=list(calories$Participants), FUN=summary)

#Create two Boxplots side-by-side for participants vs. non-participants. How are these two sub-sets are
#different in their means and variation.
boxplot(calories$Data~calories$Participants)
(min(calories$Data[calories$Participants=="Non-Participant"])) - (max(calories$Data[calories$Participants=="Non-Participant"]))
(min(calories$Data[calories$Participants=="Participant"])) -(max(calories$Data[calories$Participants=="Participant"]))


#2)Create two Histograms side-by-side for participants vs. non-participants. 
#Describe the shape of each distribution and comment on the similarity (or lack thereof) 
#between the distributions in each population. 
study <- calories$Data[calories$Participants=="Participant"]
non.study <- calories$Data[calories$Participants=="Non-Participant"]

par(mfrow = c(1, 2))
hist(study, main = "Histogram of Participants", col = blues9)
hist(non.study, main= "Histogram of Non-Participants", col = blues9)


t.test(study, non.study, alternative = "greater")

#4)	Are the assumptions of the test used in (3) met? How do you know?
sd(study)
sd(non.study)

length(study) + length(non.study)





help(t.test)

a <- 153
s <- 
n <- 10
x.bar <- 104
z <- (x.bar - a) / (s/sqrt(n))
z
2*pnorm(-abs(z))

n <- 100
ci <- c(55, 65)
x.bar <- mean(ci)

x <- 83 - 90
z <- 10/(sqrt(16))

x/z

1.645

(38 - 12) + (1.645 * (sqrt((((15**2)/9))+((10**2/ 11)))))
(38 - 12) - (1.645 * (sqrt((((15**2)/9))+((10**2/ 11)))))
sqrt(34.09)
pnorm(1.35, mean = 50)
help(pnorm)

qt(0.975, 20)

help(qt)
                      