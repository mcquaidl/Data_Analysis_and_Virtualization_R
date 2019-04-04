#Larry McQuaid
#MET CS 555
#Due: 2/26/19
rm(list=ls())

#library
library(tidyverse)
library(asbio)
install.packages("car")
library(car)

#(1) Save the data to a file (excel or CSV file) and read it into R memory for analysis. (Q1 - 2 points)
#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
students <- read.csv("CS_555_Assignment_5.csv")

attach(students)


#1. How many students are in each group? Summarize the data relating to both test score and age by the 
#student group (separately). Use appropriate numerical and/or graphical summaries. (3 points)

#Summary of the data
summary(students)
aggregate(iq, by=list(group), summary)
aggregate(age, by=list(group), summary)

#Graphical summary
par(mfrow = c(1, 2))
boxplot(iq~group, data=students, main="IQ by Student Group", xlab="group",  ylab="IQ")
boxplot(age~group, data=students, main="Age by Student Group", xlab="group",  ylab="Age")
par(mfrow = c(1, 1))

#2. Do the test scores vary by student group? Perform a one way ANOVA using the aov or Anova
#function in R to assess. Summarize the results using the 5 step procedure. If the results of the
#overall model are significant, perform the appropriate pairwise comparisons using Tukey's
#procedure to adjust for multiple comparisons and summarize these results. (7 points) 

#check to see if the variable group is a factor variable.
is.factor(group)

#Get test statitic
qf(.95, df1=2, df2=42)

#Compute one way ANOVA test
m <- aov(iq~group, data=students)
summary(m)

#Tukeys Test of Honest Significance
TukeyHSD(m)


#3.	Create an appropriate number of dummy variables for student group and re-run the one-way 
#ANOVA using the lm function with the newly created dummy variables. Set chemistry students as the 
#reference group. Confirm if the results are the same. What is the interpretation of the beta estimates 
#from the regression model? (4 points)
students$g0 <- ifelse(group=='Chemistry student', 1, 0) #don't need to create, but did for consistency
students$g1 <- ifelse(group=='Math student', 1, 0)
students$g2 <- ifelse(group=='Physics student', 1, 0)
my.model<- lm(iq ~ students$g1 + students$g2, data= students)
summary(my.model)


#4.4.	Re-do the one-way ANOVA adjusting for age. Focus on the output relating to the comparisons 
#of test score by student type. Explain how this analysis differs from the analysis in step 2 above 
#(not the results but how does this analysis differ in terms of the questions it answers as opposed 
#to the one above). Did you obtain different results? Summarize briefly (no need to go through the 
#5-step procedure here). (6 points)

Anova(lm(iq~group + age), type=3)
qf(.95, df1=1, df2=42)


-------------------------------------------------------------------
  
qf(.95, df1=4, df2=200)

mean(iq == group['Chemistry student'])
chemistry <- (iq[group =='Chemistry student'])
physics <- (iq[group =='Physics student'])
x <- mean(chemistry) - mean(physics)
abs(x)

y <- sd(chemistry) - sd(physics)
abs(y)

chemistry.age <- (age[group =='Chemistry student'])
physics.age <- (age[group =='Physics student'])

z <- sd(chemistry.age) - sd(physics.age)
abs(z)


# Check if the variable group is factor variable in R.
# If not make it a factor variable
is.factor(iq)
factor(iq)
factor(group)
# aov( data $ response ~ data $ group )
m <- aov(students$iq~students$group, data = students)
summary(m)

pairwise.t.test(students$iq, students$group, p.adj="Tukey")
help("pairwise.t.test")

TukeyHSD(m)

is.factor(group)
students$g0 <- ifelse(group=='Chemistry student', 1, 0)
students$g1 <- ifelse(group=='Math student', 1, 0)
students$g2 <- ifelse(group=='Physics student', 1, 0)
my.model<- lm(age ~ students$g1 + students$g2, data= students)
summary(my.model)


c <- Anova(lm(iq~group+age), type=3)
summary(c)

a <- lm(iq~group+age)
summary(a)




