#Larry McQuaid
#MET CS 555
#Due: 2/19/19
rm(list=ls())

#library
library(tidyverse)
library(asbio)

#(1) Save the data to a file (excel or CSV file) and read it into R memory for analysis. (Q1 - 2 points)
#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
occupations <- read.csv("CS_555_Assignment_4.csv")

attach(occupations)

cor(Score, Income)

rsq <- function (x, y) cor(x, y) ^ 2
rsq(Score, Education)
occupations <- data.frame(occupations)

#dependent variable is score
#education, income, and women in workforce are explanatory
m <- lm(formula = Score ~ (Education + Income +  WorkforceWomen), data=occupations)
summary(m)

influence(m)

#1. To get a sense of the data, generate a scatterplot to examine the association between 
#prestige score and years of education. Briefly describe the form, direction, and strength of 
#the association between the variables. Calculate the correlation. (3 points)
score.education <- lm(Score ~ Education)
score.education
plot(Education, Score, main = "Scatterplot of Prestige Score vs. Education")
abline(score.education)
cor(Education, Score)

#2. Perform a simple linear regression that predicts prestige score from education. Generate a residual
#plot. Assess whether the model assumptions are met (linearity, constant variance and normality). Are
#there any outliers or influence points? If so, identify them by ID and comment on the effect of each on
#the regression. (4 points)
score.education <- lm(Score ~ Education)
score.education
Residual <- resid(score.education)
plot(Education, Residual, main = "Residual Plot for the Regression of Prestige Score on Education")
abline(0,0)

hist(Residual, main= "Histogram of Residuals for the Regression of Prestige Score on Education")
OutVals = boxplot(Residual, plot=FALSE)$out
data.frame(OutVals)


#(3) Calculate the least squares regression equation that predicts prestige from education, income and
#percentage of women. Formally test whether the set of these predictors are associated with prestige at
#the ?? = 0.05 level. (4 points)
m <- lm(formula = Score ~ (Education + Income +  WorkforceWomen), data=occupations)
summary(m)

qf(.95, df1 = 3, df2 = 98)

summary(m)

#(4) If the overall model was significant, summarize the information about the contribution of each
#variable separately at the same significance level as used for the overall model (no need to do a formal
#5-step procedure for each one, just comment on the results of the tests). Provide interpretations for
#any estimates that were significant. Calculate 95% confidence intervals where appropriate. (4 points)
qt(0.975, df=98)
confint(m, level = .95)
summary(m)

#(5) Generate a residual plot showing the fitted values from the regression against the residuals.
#Is the fit of the model reasonable? (2 points)

resid(m)
plot(fitted(m), resid(m), axes = TRUE, frame.plot=TRUE, xlab="Fitted Values", ylab= "Residual")
abline(h=0)

hist(resid(m), xlab = "Residuals", main = "Histogram for the Residual Model")


#(6) Are there any outliers in X direction? Are any influence points? (3 points)
occupations.1 <- occupations[-c(52), ]
m.1 <- lm(formula = Score ~ (Education + Income +  WorkforceWomen), data=occupations.1)
summary(m)
summary(m.1)

1.2*70
1.19*20 - 4.94
(20/3) / (180/45)
qf(.95, df1 = 2, df2 = 17)
qt(0.95, df=17)

4.18 + (0.04 * 28) + (0.159 * 25)


20 - (-4.94+ 1.19*20)
60 - (1.2*70 - 20)
