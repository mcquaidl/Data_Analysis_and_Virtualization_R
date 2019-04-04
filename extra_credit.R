#Larry McQuaid
#MET CS 555
rm(list=ls())

#Save the data to a file (excel or CSV file) and read it into R memory for analysis.
#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
heart <- read.csv("heart.csv")

attach(heart)

m <- lm(cp ~ (age + sex + chol), data=heart)
qf(.95, df1 = 3, df2 = 46)
summary(m)
