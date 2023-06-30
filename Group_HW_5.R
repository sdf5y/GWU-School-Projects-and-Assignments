library(ggplot2)

#Question 4 - Binomial Distribution - 

rand_bin<-data.frame(matrix(NA,100000,3))
rand_bin
colnames(rand_bin)<-c('b.2', 'b.4', 'b.7')

#Binomial distribution with a probability of 0.2
rand_bin$b.2<-rbinom(100000,25,0.2)

#Binomial Distribution with a probability of 0.4
rand_bin$b.4<-rbinom(100000,25,0.4)

#Binomial distribution with a probability of 0.7
rand_bin$b.7<-rbinom(100000,25,0.7)

#Reshaping for graphic purposes 

library(reshape)
rand_bin_long<-melt(rand_bin)

#Question 4 (b) - Plotting the binomial distribution 
p.bin<-ggplot(rand_bin_long, aes(x=value, fill = variable))+
  geom_density(alpha = .25)+
  geom_vline(xintercept = c(mean(rand_bin$b.2),
                            mean(rand_bin$b.4), mean(rand_bin$b.7)), col = "red")+
  ggtitle("Kernel Density of Simulated Binomial Distributions")+
  labs(x="Random Variable X", y = "Density (Probability)")
p.bin


#-------------------------------------------------------------------------------------------

#Question 5 - Poisson Distribution 

rand_pois<-data.frame(matrix(NA,100000,3))
rand_pois
colnames(rand_pois)<-c('p.5', 'p2', 'p24')

#Poisson distribution with mean 0.5
rand_pois$p.5<-rpois(100000,0.5)

#Poisson distribution with mean 2
rand_pois$p2<-rpois(100000,2)

#Poisson distribution with mean 24
rand_pois$p24<-rpois(100000,24)

#Reshaping for graphic purposes 

rand_pois_long<-melt(rand_pois)

#Question 5(b) plotting for poisson distribution 

p.pois<-ggplot(rand_pois_long, aes(x=value, fill = variable))+
  geom_density(alpha = .25)+
  geom_vline(xintercept = c(mean(rand_pois$p.5), mean(rand_pois$p2),
                            mean(rand_pois$p24)), col = "blue")+
  ggtitle("Kernel Density Of simulated Poisson Distributions")+
  labs(x="Random Variable X", y = "Density (Probability)")
p.pois


#Question 5(c) code 
rand_pois$pois2c<- rand_pois$p2 - mean(rand_pois$p2)
rand_pois$pois.5<-rand_pois$p.5 - mean(rand_pois$p.5)
rand_pois$pois24<-rand_pois$p24 - mean(rand_pois$p24)

#Plotting the above variables - 
s.pois<-ggplot(rand_pois_long, aes(x=value, fill = variable))+
  geom_density(alpha = .25)+
  geom_vline(xintercept = c(rand_pois$pois2c, rand_pois$pois.5, rand_pois$pois24), col = "green")+
  ggtitle("Plot")+
  labs(x="Random", y = "Mean")
s.pois


#--------------------------------------------------------------------------------------
#Question 6 -  Normal Distribution 
rand_norm<-data.frame(matrix(NA,100000,4))
colnames(rand_norm)<-c('3 and 3', '-3 and 3', '-3 and 6', '0 and 1')

#Question 6 (a)- 
#Normal Distribution with mean 3 and SD 3
rand_norm$`3 and 3`<-rnorm(100000,3,3)

#Normal Distribution with mean as -3 and SD 3
rand_norm$`0 and 1`<-rnorm(100000,-3,3)

#Normal Distribution with mean as -3 and SD as 6
rand_norm$`-3 and 6`<-rnorm(100000,-3,6)

#Normal Distribution with mean as 0 and SD as 1 
rand_norm$`0 and 1`<-rnorm(100000,0,1)


#Reshaping for graphic purposes 
rand_norm_long<-melt(rand_norm)

#Question 6(a) plotting for Normal Distribution 

p.norm<-ggplot(rand_norm_long, aes(x=value, fill = variable))+
  geom_density(alpha = .25)+
  geom_vline(xintercept = c(mean(rand_norm$`3 and 3`), mean(rand_norm$`-3 and 3`), mean(rand_norm$`-3 and 6`), 
                            mean(rand_norm$`0 and 1`)), col = "yellow")+
  ggtitle("Kernel Density of Simulated for Normal Distributions")+
  labs(x = "Random Variable X", y = "Density (Probability)")
p.norm


#Code for 6(b)
rand_norm$norm3_3<-rand_norm$`3 and 3` - mean(rand_norm$`3 and 3`)
rand_norm$norm_neg3_3<-rand_norm$`-3 and 3` - mean(rand_norm$`-3 and 3`)
rand_norm$norm_neg3_6<-rand_norm$`-3 and 6` - mean(rand_norm$`-3 and 6`)
rand_norm$norm0_1<-rand_norm$`0 and 1` - mean(rand_norm$`0 and 1`)

#Plotting the above observations 
n.norm<-ggplot(rand_norm_long, aes(x=value, fill = variable))+
  geom_density(alpha = .25)+
  geom_vline(xintercept = c(rand_norm$norm3_3, rand_norm$norm_neg3_3, rand_norm$norm_neg3_6, rand_norm$norm0_1), col = "skyblue2")+
  ggtitle("Normal Plot")+
  labs(x = "Normal", y = "Mean")
n.norm

colors()
