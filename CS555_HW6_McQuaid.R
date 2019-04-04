#Larry McQuaid
#MET CS 555
#Due: 3/5/19
rm(list=ls())

#packages
#install.packages("car")
#install.packages('aod')
#install.packages('pROC')

#library
library(tidyverse)
library(asbio)
library(car)
library(aod)
library(pROC)

#Save the data to a file (excel or CSV file) and read it into R memory for analysis.
#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
body.temp <- read.csv("CS_555_Assignment_6.csv")

attach(body.temp)

#1. We are interested in whether the proportion of men and women with body temperatures greater
#than or equal to 98.6 degrees Fahrenheit are equal. Therefore, we need to dichotomize the body
#temperature variable. Create a new variable, called "temp_level" in which temp_level = 1 if body
#temperature >= 98.6 and temp_level=0 if body temperature < 98.6. (3 point)
#Summarize the data relating to body temperature level by sex. 

body.temp$temp_level <- ifelse(body.temp$temp >=98.6,
                               c(1),
                               c(0))

temp.gender <- table(Temp = temp_level, Gender=sex)
rownames(temp.gender) <- c("Below Average", 'Above Average')
colnames(temp.gender) <- c('Male', 'Female')
temp.gender

#2. Calculate the risk difference. Formally test (at the ??=.05 level) whether the proportion of people with
#higher body temperatures (greater than or equal to 98.6) is the same across men and women, based on
#this effect measure. Do females have higher body temperatures than males? (4.5 points)

prop.males.above <- temp.gender[2,1]/colSums(temp.gender)[1]
prop.males.above
  
prop.females.above <- temp.gender[2,2]/colSums(temp.gender)[2]
prop.females.above

#risk difference
risk.dif <- (temp.gender[2,1]+temp.gender[2,2])/130
risk.dif

test.stat <- (prop.females.above - prop.males.above) / (sqrt((risk.dif*(1-risk.dif)*((1/65)+(1/65)))))
test.stat

prop.test(x = temp.gender[1,], n=colSums(temp.gender), correct=FALSE, alternative ='two.sided' )

#3.	Perform a logistic regression with sex as the only explanatory variable. Formally test (at 
#the ??=.05 level) if the odds of having a temperature greater than or equal to 98.6 is the same 
#between males and females. Include the odds ratio for sex and the associated 95% confidence 
#interval based on the model in your summary and interpret this value. What is the c-statistic for 
#this model? (5.5 points)

#create dummy variables
body.temp$men_dummy <- ifelse(sex== 1,1,0)
body.temp$women_dummy <- ifelse(sex== 2, 1, 0)


#regression coefficient
m <- glm(body.temp$temp_level ~ body.temp$sex, family=binomial )
summary(m)

#exp(coef(m))
exp(cbind(OR=coef(m), confint.default(m)))
roc(temp_level~sex+heartRate)


body.temp$prob_temp <- predict(m, type='response')
g <- roc(temp_level ~ prob_temp)
g

#4.	Perform a multiple logistic regression predicting body temperature level from sex and 
#heart rate. Summarize briefly the output from this model. Give the odds ratio for sex and heart
#rate (for a 10 beat increase). What is the c-statistic of this model? (5 points)

m1 <- glm(temp_level~sex + heartRate, family = binomial)
summary(m1)
wald.test(b=coef(m1), Sigma=vcov(m1), Terms = 2:3)
exp(coef(m1)[3]*10)
roc(temp_level~sex+heartRate)

#5. hich model fit the data better? Support your response with evidence from your output. Present
#the ROC curve for the model you choose. (2 points)

temp.gender
plot(g, main="Area Under Curve = 0.7929")

     
------------------------------------
((22/63)-(6/45)) *100

x <-(22+6)/(63+45)
n <- (63+45)

((22/63)-(6/45)) / (sqrt((x*(1-x)*((1/63)+(1/45)))))

p1 <- (22/63)
p2 <- (6/45)

x - 1.96 * sqrt((x*(1-x))/n)
x + 1.96 * sqrt(((x*(1-x))/n))

prop.test(28, 108, p=x, conf.level=0.95, correct=FALSE)

((22/63)-(6/45)) - 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45))
((22/63)-(6/45)) + 1.96 * sqrt(((p1*(1-p1))/63) + ((p2*(1-p2))/45))
 

exp(.835)/exp(1.835)
