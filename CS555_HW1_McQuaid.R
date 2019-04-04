#Larry McQuaid
#MET CS 555
#Due: 1/29/19

library(tidyverse)
install.packages("swirl")

#Setting & Getting Working Directory
setwd("C:/Users/n0236192/Documents/CS_555")
c.difficile <- read.csv("CS_555_Assignment_1.csv", header = FALSE)
c.difficile <- as.numeric(unlist(c.difficile))

#Part 1: General Data Gathering
#summary(c.difficile)
#sd(c.difficile)
#quantile(c.difficile)
#pnorm(7, 6, 3, TRUE)
#1 - pnorm(7, 6,0.94, TRUE, log.p = FALSE) #for sampels find st dev of the st dev

#Part 2
#1) Make a histogram of the duration of days of hospital stays. Ensure the histogram is labelled
#appropriately. Use a width of 1 day

hist(c.difficile, 
     probability=T, 
     col='lightblue', 
     main='Histogram of c.difficile Cases',
     xlab = 'Duration of Hospital Stays',
     breaks = seq(8, 27, by=1),
     xlim = c(9, 27),
     ylim = c(0, 0.2))

table(c.difficile)

#2)  Create a Boxplot for the duration of days of hospital stays.

boxplot(c.difficile, col=hcl(60), xaxt = "n", 
                      xlab = "c.difficile Data Set", staplewex = 1,horizontal = TRUE, axes = FALSE)
        text(x = fivenum(c.difficile), labels = fivenum(c.difficile), y=1.3, srt=90)
        text(x = fivenum(c.difficile), labels=c("Min","Lower Hinge", "Median",
                                       "Upper Hinge", "Max"),y=.7, srt=90)

        
IQR(c.difficile)

help(IQR)


s <- c(8, 4, 10, 4, 14, 26)
mean(s)
summary(s)

1- pnorm(273, 266, 5)

1- ((pnorm(261, 266, 5)) + (1-pnorm(271, 266, 5)))

1- ((pnorm(60, 64, 2)) + (1-pnorm(68, 64, 2)))

help(qnorm)

qnorm(.97, 0, 1)
pnorm(2.00, 2.30, 0.20)

pnorm(984, 1000, 30)
