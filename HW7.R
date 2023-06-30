setwd("C:/Users/18045/Documents/R/Statistic Class/HW7")
library(dplyr)
library(ggplot2)
library(cowplot)

v <- rbinom(200, 154, prob = .8)
v
summary(v)
sd(v)

mpg <- data.frame(37.2, 21,17.4, 24.9, 27, 36.9, 38.8, 35.3, 32.3, 23.9, 19, 26.1, 25.8, 41.4, 34.4, 32.5, 25.3, 26.5, 28.2, 22.1)
sd(mpg)
summary(mpg)
mean(mpg)
sum(mpg)/20

iodine <- c(69.55, 69.41, 69.98, 69.26, 70.71, 70.22, 69.59, 69.94, 70.66, 70.72, 71.18, 70.16, 69.83, 70.47, 69.86, 70.1, 70.22, 70.67, 70.81, 70.38, 69.24, 70.48, 68.78, 69.67, 69.88)

summary(iodine)
sd(iodine)

stderror <- sd(iodine)/sqrt(length(iodine))
stderror

t.test(iodine, conf.level = .90)
t.test(iodine, conf.level = .95)

x <- dnorm(85, mean = 612, sd=1327)

t.test(x, conf.level = .99)

#Code for Question 2(1). Generating random t distribution observations 
gen_t03<-data.frame(rt(50000,3))
gen_t03

gen_t10<-data.frame(rt(50000,10))
gen_t10

gen_t30<-data.frame(rt(50000,30))
gen_t30

gen_t60<-data.frame(rt(50000,60))
gen_t60

x<- rnorm(50000)
plot(x, dnorm(x, 0, 1), type = "h")


#Code for Question 2(s)
gen_t03_95 <-rt(50000, 3, 0.95)
gen_t03_95 

install.packages("BSDA")
library(BSDA)

summary(gen_t03)
sd(gen_t03)

test_3 <- z.test(gen_t03, 
                 Norm,
                 alternative='two.sided', 
                 mu= mean(gen_t03), 
                 sigma.x = sd(gen_t03),
                 sigma.y = sd(Norm),
                 conf.level=.95)
test_3
test_10 <- z.test(gen_t10, 
                 Norm,
                 alternative='two.sided', 
                 mu= mean(gen_t10), 
                 sigma.x = sd(gen_t10),
                 sigma.y = sd(Norm),
                 conf.level=.95)
test_10
test_30 <- z.test(gen_t30, 
                  Norm,
                  alternative='two.sided', 
                  mu= mean(gen_t30), 
                  sigma.x = sd(gen_t30),
                  sigma.y = sd(Norm),
                  conf.level=.95)
test_30
test_60 <- z.test(gen_t60, 
                  Norm,
                  alternative='two.sided', 
                  mu= mean(gen_t60), 
                  sigma.x = sd(gen_t60),
                  sigma.y = sd(Norm),
                  conf.level=.95)
test_60

d <- density(test_3$estimate)
plot(d)

d1 <- density(test_10$estimate)
plot(d1)

d60 <- density(test_60$estimate)
plot(d60)




library(ggplot2)

n.obs <- 50000
df1 <- 3
df2 <- 10
df3 <- 30
df4 <- 60

gt3 <- rt(n.obs,df1)
sim1 <- as.data.frame( matrix(gt3, df1, n.obs) )

p.data.sim1<- ggplot(as.data.frame(t(sim1))[1], aes(x=V1))+
  geom_density(color="orange", fill="pink") +
  labs(x="Degree of Freedom", y = "Density")
p.data.sim1

gt10 <- rt(n.obs,df2)
sim2 <- as.data.frame( matrix(gt10, df2, n.obs) )

p.data.sim2<- ggplot(as.data.frame(t(sim2))[1], aes(x=V1))+
  geom_density(color="orange", fill="pink") +
  labs(x="Degree of Freedom", y = "Density")

p.data.sim2


gt30 <- rt(n.obs,df3)
sim3 <- as.data.frame( matrix(gt30, df3, n.obs) )

p.data.sim3<- ggplot(as.data.frame(t(sim3))[1], aes(x=V1))+
  geom_density(color="orange", fill="pink") +
  labs(x="Degree of Freedom", y = "Density")

p.data.sim3

gt60 <- rt(n.obs,df4)
sim4 <- as.data.frame( matrix(gt60, df4, n.obs) )

p.data.sim4<- ggplot(as.data.frame(t(sim4))[1], aes(x=V1))+
  geom_density(color="orange", fill="pink") +
  labs(x="Degree of Freedom", y = "Density")

p.data.sim4


plot_grid(p.data.sim1, p.data.sim2,
          p.data.sim3, p.data.sim4, ncol=2)


combo <- data.frame(value = c(gt3,
                              gt10,
                              gt30, 
                              gt60),
                    group = c("gt3",
                               "gt10",
                               "gt30",
                               "gt60"))
                  
ggplot(combo,
       aes(x = value,
            fill = group))+ 
         geom_density(alpha = .4)+
  xlim(-4,4)


#Code for Question 3(2) determining the two tail at 95% scores at each degrees of freedom 
library(readxl)
install.packages('Quandl')
library(Quandl)
download.file(url = "https://static.quandl.com/EIA+codes/EIA_ELEC_codes.txt",
              destfile = "C:/Users/18045/Documents/R/Statistic Class/HW7/HW_7.txt")

hw_7 <- read.csv("HW_7.txt")
hw_7

#read in datasets
# East North Central : Residential : Annual::Million Kilowatthours
# East South Central : Residential : Annual::Million Kilowatthours

e.sale.res.ENC<- Quandl("EIA/ELEC_SALES_ENC_RES_A")
e.sale.res.ENC
summary(e.sale.res.ENC)

e.sale.res.ESC<- Quandl("EIA/ELEC_SALES_ESC_RES_A")
e.sale.res.ESC
summary(e.sale.res.ESC)

# Examine plot for normality
library("car")
install.packages('car')
qqPlot(e.sale.res.ENC$Value, 
       xlab = "Quantile ", 
       ylab = "Value")

qqPlot(e.sale.res.ESC$Value)
?qqPlot

# Run a one sample t-test
t.test(e.sale.res.ENC$Value, mu=189599)
?t.test

# Create indicator variables
e.sale.res.ENC$Region<-"East North Central"
e.sale.res.ESC$Region<-"East South Central"

# Append datasets
e.sale.all<- as.data.frame( rbind(e.sale.res.ENC, e.sale.res.ESC) )

# Two independent sample t.test
t.test(Value~ Region, data=e.sale.all, paired = FALSE )
?t.test

# Paired t.test
t.test(Value~ Region, data=e.sale.all, paired = TRUE )


