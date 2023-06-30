setwd("C:/Users/18045/Documents/R/Statistic Class/HW8")
library(dplyr)
library(ggplot2)
library(readr)
library(dplyr)
library(readxl)
#install.packages('boot')
library(car)
library(boot)

#17.16  Using this data set, create a histogram and Normal 
#quantile plot of the seed counts from Plant 1746. Do the same for Plant 1748. 

#import data 
seed_factory <- data.frame(read.csv("ex17-16seedcnt.csv"))

seed_factory_1746 <- (filter(seed_factory, Plant == 1746))
seed_factory_1748 <- (filter(seed_factory, Plant == 1748))

plot_hist_1746 <- hist(seed_factory_1746$SeedCount, 
                       xlab = 'Seed Count',
                       ylab = 'Frequency',
                       main = "1746 Seed Freq.")

plot_q_1746 <- qqnorm(seed_factory_1746$SeedCount,
       ylab = "Seed Count")
qqline(seed_factory_1746$SeedCount, 
       col = "red")

plot_hist_1748 <- hist(seed_factory_1748$SeedCount, 
                       xlab = 'Seed Count',
                       ylab = 'Frequency',
                       main = '1748 Seed Freq.')

plot_q_1748 <- qqnorm(seed_factory_1748$SeedCount,
       ylab = "Seed Count")
qqline(seed_factory_1748$SeedCount,
       col = "red")

wilcox.test(seed_factory_1746$SeedCount, alternative = "two.sided")
wilcox.test(seed_factory_1748$SeedCount, alternative = "two.sided")


#17.18 Question

asia <- data.frame(read.csv("ex17-18asia.csv"))

GDP_level <- c(5.31,4.02,2.06,4.77,5.75,0.80, 0.73, -5.25, 2.55, 0.29,5.83,5.28,3.05,6.09,5.58,3.33,5.35)
Countries <- c('Cambodia', 'Papua New Guinea', 'Kiribati', 'Philippines', 'Laos', 'Solomon Islands', 'Federated States of Micronesia', 'Timor-Leste', 'Mongolia', 'Vanuatu', 'Myanmar', 'Vietnam', 'Pakistan', 'India', 'Bangladesh', 'Sri Lanka', 'Bhutan')

asia <- data.frame(Countries, GDP_level)

asia$Rank <- rank(asia$GDP_level)

sum(asia$Rank[13:17])

wilcox.test(asia$GDP_level[13:17], alternative = "two.sided")
t.test(asia$GDP_level[13:17], alternative = "two.sided")

#20.18--------------------------------------
#import data 
erp_df <- data.frame(read.csv("ex17-20erp.csv"))
erp_df$Rank <- rank(erp_df$ERP)

wilcox.test(erp_df$ERP, alternative = "two.sided")
t.test(erp_df$ERP)
mean(erp_df$ERP)

#20.24 --------And 20.38---------------
#import data 
safety_df <- data.frame(read.csv("ex17-23fsafety.csv"))

unique(safety_df$sfair)
unique(safety_df$srest)

nfair <- safety_df %>% count(safety_df$sfair)

nrest <- safety_df %>% count(safety_df$srest)

plot(nfair)
plot(nrest)
 
t.test(nfair$n)
t.test(nrest$n)

#17.40 --------And 17.42---------------
#import data 
radon <- data.frame(read.csv("ex17-40radon.csv"))
hist(radon$radon,
     xlab = "Index",
     ylab = "Concentration picocuries",
     main = 'Radon Levels',)

wilcox.test(radon$radon, alternative = "two.sided")
wilcox.test(radon$radon, alternative = 'less')

avg_radon <- mean(radon$radon)
sample.n <- length(radon$radon)
sample.sd <- sd(radon$radon)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se

lower.bound <- avg_radon - margin.error
upper.bound <- avg_radon + margin.error
print(c(lower.bound,upper.bound))


#3 ---------------------------------------------------------------

x = rweibull(70,1.3,0.6)
x

x1<-rweibull(70,1.1,0.9)
x1

x2<-rweibull(70,1,4)
x2

plot(x)
plot(x1)
plot(x2)

wilcox.test(x, alternative = "two.sided")
t.test(x, alternative = "two.sided")

wilcox.test(x1, alternative = "two.sided")
t.test(x1, alternative = "two.sided")

wilcox.test(x2, alternative = "two.sided")
t.test(x2, alternative = "two.sided")

#4--------------------------------

#import data 
worldbank_df <- data.frame(read.csv("1934f837-c930-4285-b356-b38a6772b52d_Data.csv",
                                    na.strings=c("..",NA)))
#rename columns
New_colnames <- c("Country",   "Country Code",   "Series Name",    "Series Code",    "2019", "2020")
colnames(worldbank_df) <- New_colnames
colnames(worldbank_df)

#make wide table
wb.wide <- reshape(worldbank_df, idvar = "Country", v.names= c("2019", "2020"),
                  timevar = "Series Code", direction = "wide")
wb.wide1 <- wb.wide[,-c(3,8,9)]

wb.long<-reshape(wb.wide1, idvar = "Country",
                 varying = list(c(3,4),c(5,6)), v.names = c('Trade', 'Income'),
                 direction = "long")
wb.long$year= wb.long$time+2018

wb.long <- wb.long %>% 
  filter(!(is.na(wb.long$Trade) & is.na(wb.long$Income))) 

wb.long.2019 <- wb.long %>% 
  filter(wb.long$year == 2019) 

wb.long.2020 <- wb.long %>% 
  filter(wb.long$year == 2020) 

#part e - plot and ShapiroWilk test

legend_colors <- c("2019" = "Blue", "2020" = "Orange")

ggplot() + 
  geom_line(data= wb.long.2019,
            aes(x= Trade, 
                y= Income),
            color='Blue') + 
  geom_line(data= wb.long.2020, 
            aes(x= Trade, 
                y= Income),
            color='Orange') +
           labs(title = "Trade V. Income",
             x = "Dollars",
             y = "Dollars")+
  scale_color_manual(values = legend_colors) + 
  theme_bw()

shapiro.test(wb.long.2019$Trade)
shapiro.test(wb.long.2019$Income)
shapiro.test(wb.long.2020$Trade)
shapiro.test(wb.long.2020$Income)

ks.test(wb.long.2019$Trade, rpois)
?ks.test

t.test(wb.long.2019$Trade, alternative = "two.sided")
t.test(wb.long.2019$Income, alternative = "two.sided")
t.test(wb.long.2020$Trade, alternative = "two.sided")
t.test(wb.long.2020$Income, alternative = "two.sided")

wilcox.test(wb.long.2019$Trade, wb.long.2019$Income, paired = TRUE, alternative = "two.sided")
wilcox.test(wb.long.2020$Trade, wb.long.2020$Income, paired = TRUE, alternative = "two.sided")

# Boot strap --------------------------------
samplemean <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}
samplemedian <- function(x, d) {
  return(median(x[d], na.rm=TRUE))
}
b.mean <- boot(data= wb.long.2019$Trade, 
              statistic=samplemean, R=10000)
b.median <- boot(data=wb.long.2019$Trade, 
                statistic=samplemean, R=10000)

bci.mean<-boot.ci(b.mean, conf = 0.95, 
                  type="bca",
                  t = NULL)
bci.median<-boot.ci(b.median, conf = 0.95, 
                    type="bca")
bci.mean$bca
bci.median$bca
