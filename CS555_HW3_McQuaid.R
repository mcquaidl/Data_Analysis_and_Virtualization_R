#Larry McQuaid
#MET CS 555
#Due: 2/12/19
rm(list=ls())

#library
library(tidyverse)
library(asbio)

#(1) Save the data to a file (excel or CSV file) and read it into R memory for analysis. (Q1 - 2 points)
#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
fish.mercury <- read.csv("CS_555_Assignment_3.csv")

attach(fish.mercury)

fish.meals <- NumFishMeals
total.mercury <- TotalMercury

#(2) To get a sense of the data, generate a scatterplot (using an appropriate window, label the axes, and
#title the graph). Consciously decide which variable should be on the x -axis and which should be on the
#y-axis. Using the scatterplot, describe the form, direction, and strength of the association between the
#variables. (Q2 - 3 points)

plot(fish.meals, total.mercury, main = "Scatterplot of Number of Fish Meals vs. Mercury Levels", xlab= "Number of Fish Meals",
     ylab = "Mercury Levels", xlim = c(0, 40), ylim = c(0, 30), pch=1, col='blue')
cor(fish.mercury)

#(3) Calculate the correlation coefficient. What does the correlation tell us? (Q3 - 2 points)
cor(fish.mercury)

#(4) Find the equation of the least squares regression equation, and write out the equation. Add the
#regression line to the scatterplot you generated above. (Q4 - 4 points)
#beta_1 calculation
r <- c(0.7256283)
s.x <- c(sd(fish.meals))
s.y <- c(sd(total.mercury))
beta.1 <- r * (s.y/s.x)
beta.1

#beta_0 calculation
x.bar <- c(mean(fish.meals))
y.bar <- c(mean(total.mercury))
beta.0 <- y.bar - (beta.1 * x.bar)
beta.0

#setting up linear model to put in regression line
my.model <- lm(fish.mercury$TotalMercury ~ fish.mercury$NumFishMeals)
abline(my.model)

#(6) Calculate the ANOVA table and the table which gives the standard error of ??^1 (hat beta 1) .
#Formally test the hypothesis that beta_1 = 0 using either the F-test or the t-test at the alpha level
#a=0.10. Either way, present your results using the 5 step procedure as in the course notes. Within your
#conclusion, calculate the R2 (R squared) value and interpret this. 
anova(my.model)
summary(my.model)

#calculate the F-distribution with 1, 98 degrees of freedom and associated with alpha = 0.10
qf(.90, df1=1, df2 = 98)


------------------------------------------------------------------------------
#Quiz 3


1000 - (38 * 20)
38*20
x <- c(1, 2, 2, 3, 4)
y <- c(1, 2, 3, 2, 4)

cor(x, y)

plot(x, y, main = "Scatterplot of Number of Fish Meals vs. Mercury Levels", xlab= "Number of Fish Meals",
     ylab = "Mercury Levels", pch=1, col='blue')

-------------------------------------------------------------------------------------------
fish.meals <- fish.mercury$NumFishMeals
fish.meals

total.mercury <- fish.mercury$TotalMercury
total.mercury

cor(fish.mercury)

#beta_1 calculation
r <- c(0.7256283)
s.x <- c(sd(fish.meals))
s.y <- c(sd(total.mercury))
beta.1 <- r * (s.y/s.x)
beta.1

#beta_0 calculation
x.bar <- c(mean(fish.meals))
y.bar <- c(mean(total.mercury))
beta.0 <- y.bar - (beta.1 * x.bar)
beta.0

#r-squared function
rsq <- function (x, y) cor(x, y) ^ 2
rsq(fish.meals, total.mercury)


#Setting up ANOVA table (the "Pr(>|t|) intercept is the p-value for beta_1 = 0)
my.model <- lm(fish.mercury$TotalMercury ~ fish.mercury$NumFishMeals)
anova(my.model)
summary(my.model)

#Confidence interval (fish.mercury$numfishmeals is the lower bound confidence interval for beta_1)
confint(my.model, level = .90)
