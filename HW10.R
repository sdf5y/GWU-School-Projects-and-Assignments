setwd("C:/Users/18045/Documents/R/Statistic Class/HW10")
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

confint(1.05, 'hours', level=0.95)

lm(group_1A, 23)


x_1 <- 0
x_2 <- 1
u_1 <- 20+10*(x_1)+3*(x_2)+2*(x_1)*(x_2)
group_1A <- 20+3*(x_2)
group_1B <- 20+10+3*(x_2)+2*(x_2)
plot(~ 20+3*(x_2))




u_2 <- 20+10*(x_1)+3*(x_2)-2*(x_1)*(x_2)
u_3 <- 20-10*(x_1)-3*(x_2)+2*(x_1)*(x_2)

groupa

plot(u_1,u_2,u_3)
