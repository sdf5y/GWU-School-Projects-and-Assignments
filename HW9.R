setwd("C:/Users/18045/Documents/R/Statistic Class/HW9")
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
#install.packages('haven')
library(haven)
#install.packages("stringr")
library("stringr")  
#install.packages("qacBase")
library('qacBase')

#11.12

maletip <- read.csv('ex11-12tipmale.csv')

maleyes <- maletip %>%
  filter(Tip == 'Yes')

maleno <- maletip %>%
  filter(Tip == 'No')

chisq.test(maleno$Count,maleyes$Count)


#11.34

Survey_date <- c(2001,2005,2009,2013,2018)
Broadband <- c(.06, .33, .63, .7, .83)
CountS <- c(135,743,1418,1575,1868)

q1134 <- data.frame(Survey_date, Broadband, CountS)

chisq.test(q1134$Survey_date, q1134$CountS)

#import data 
ATP_W92 <- read_sav("ATP W92.sav")
cols_ATP_W92 <- colnames(ATP_W92)

#Question 2------------------------------------------------------------------------

#Rename variables
ATP_W92 <- ATP_W92 %>% 
  mutate(INSTN_LGECRP_W92 = factor(INSTN_LGECRP_W92,
                             levels = c(1,2,99),
                             labels = c('Positive effect','Negative effect','Refused')))

ATP_W92 <- ATP_W92 %>% 
  mutate(INSTN_LBRUN_W92 = factor(INSTN_LBRUN_W92,
                                 levels = c(1,2,99),
                                 labels = c('Positive effect','Negative effect','Refused')))
#create DataFrames, plot, and test

q2_df <- data.frame(ATP_W92$INSTN_LGECRP_W92)
q2_1 <- data.frame(summary(q2_df$ATP_W92.INSTN_LGECRP_W92))

q2_df <- data.frame(ATP_W92$INSTN_LBRUN_W92)
q2_2 <- data.frame(summary(q2_df$ATP_W92.INSTN_LBRUN_W92))

q2_df['Response'] <- c('Positive effect','Negative effect','Refused','NA\'s')

q2_df <- data.frame(q2_1, q2_2)
summary(q2_df$ATP_W92.INSTN_LGECRP_W92)

names(ATP_W92)[12] <- 'Large Corporations'
names(ATP_W92)[14] <- 'Unions'

ATP_W92$Large_Corporations <- ATP_W92$INSTN_LGECRP_W92
ATP_W92$Unions <- ATP_W92$INSTN_LBRUN_W92

crosstab(ATP_W92, Large_Corporations, Unions,
         type="freq", plot=TRUE, chisquare=TRUE, na.rm = FALSE)

colnames(ATP_W92)

q2_dff <- q2_df[,-3]

plot(q2_dff,
     xlab = 'value',
     ylab = 'Count',
     main = "Positive and Negative Survey Says...")

text(x=3500, y= 3000, labels = 'Positive effect')

chisq.test(q2_df$summary.q2_df.ATP_W92.INSTN_LGECRP_W92., q2_df$summary.q2_df.ATP_W92.INSTN_LBRUN_W92.)

#remove NAs

q2_df <- q2_df[-4,]

chisq.test(q2_df$summary.q2_df.ATP_W92.INSTN_LGECRP_W92., q2_df$summary.q2_df.ATP_W92.INSTN_LBRUN_W92.)

#Question 3------------------------------------------------------------------------

#Rename variables
ATP_W92 <- ATP_W92 %>% 
  mutate(GLBLZE_W92 = factor(GLBLZE_W92,
                             levels = c(1,2,99),
                             labels = c('Gained more from increased trade','Lost more from increased trade','Refused')))

ATP_W92 <- ATP_W92 %>% 
  mutate(IL_IMM_PRI_W92 = factor(IL_IMM_PRI_W92,
                                 levels = c(1,2,3,99),
                                 labels = c('Stronger security and enforcement', 'immigrants already here to be citizens with requirements', 'Both need equal priority','Refused')))
#create DataFrames

q3_1 <- data.frame(summary(ATP_W92$GLBLZE_W92))

q3_2 <- data.frame(summary(ATP_W92$IL_IMM_PRI_W92))

q3_df <- data.frame(ATP_W92$GLBLZE_W92, ATP_W92$IL_IMM_PRI_W92)

datatable <- matrix(q3_df, nrow=7,ncol=2)

plot(combo$Count,
     xlab = Response,
     ylab = 'Count',
     main = "The U.S. has...")

chisq.test(q3_df$ATP_W92.GLBLZE_W92, q3_df$ATP_W92.IL_IMM_PRI_W92)

#Replace values with names and include NA values

q3_df_no <- na.omit(q3_df) 

chisq.test(q3_df_no$, q3_df_no$ATP_W92.IL_IMM_PRI_W92)

cols_ATP_W92

#table and plot

ATP_W92$Trade <- ATP_W92$GLBLZE_W92
ATP_W92$Immigration <- ATP_W92$IL_IMM_PRI_W92

crosstab(ATP_W92, Trade, Immigration,
         type="freq", plot=TRUE, chisquare=TRUE, na.rm = TRUE)

#Question 4------------------------------------------------------------------------

#Rename variables
ATP_W92 <- ATP_W92 %>% 
  mutate(REPRSNTREP_W92 = factor(REPRSNTREP_W92,
                                   levels = c(1,2,3,4),
                                   labels = c('Very well','Somewhat well','Not too well','Not at all well')))

ATP_W92 <- ATP_W92 %>% 
  mutate(NATPROBS_c_W92 = factor(NATPROBS_c_W92,
                                  levels = c(1,2,3,4),
                                  labels = c('A very big problem','A moderately big problem','A small problem','Not a problem at all')))
                        #-----------------------
ATP_W92 <- ATP_W92 %>% 
  mutate(GOVSIZE1_W92 = factor(GOVSIZE1_W92,
                                 levels = c(1,2),
                                 labels = c('A smaller government providing fewer services','A bigger government providing more services')))

ATP_W92 <- ATP_W92 %>% 
  mutate(BILLION_W92 = factor(BILLION_W92,
                                 levels = c(1,2,3),
                                 labels = c('A good thing for the country','A bad thing for the country','Neither a good thing or a bad thing')))

#create DataFrames, plot, and test

q4_1df <- data.frame(ATP_W92$REPRSNTREP_W92, ATP_W92$NATPROBS_c_W92)
q4_1 <- data.frame(summary(ATP_W92$REPRSNTREP_W92))
q4_12 <- data.frame(summary(ATP_W92$NATPROBS_c_W92))

q4_2df <- data.frame(ATP_W92$GOVSIZE1_W92, ATP_W92$BILLION_W92)
q4_21 <- data.frame(summary(ATP_W92$GOVSIZE1_W92))
q4_22 <- data.frame(summary(ATP_W92$BILLION_W92))

names(q4_1) <- 'GOP Rep'
names(q4_12) <- 'Nat. Problems'
names(q4_21) <- 'Govt Size'
names(q4_22) <- 'Billionaries'

q4_df <- data.frame(q4_1df, q4_2df)

names(q4_df)[1] <- 'GOP Rep'
names(q4_df)[2] <- 'Nat. Problems'
names(q4_df)[3] <- 'Govt Size'
names(q4_df)[4] <- 'Billionaries'


q4_df <- q4_df %>%
  filter(`GOP Rep` == 'Very well') %>%
  filter(`Nat. Problems`== 'A very big problem')


plot(q4_df$`Govt Size`,
     xlab = 'Govt Size',
     ylab = 'Count',
     main = "Govt Size")

plot(q4_df$Billionaries,
     xlab = 'Billionaries',
     ylab = 'Count',
     main = "Billionaries")

chisq.test(q4_df$`Govt Size`,q4_df$Billionaries)

ATP_W92$GOP_Rep <- ATP_W92$REPRSNTREP_W92
ATP_W92$Nat_Problems <- ATP_W92$NATPROBS_c_W92
ATP_W92$Govt_Size <- ATP_W92$GOVSIZE1_W92
ATP_W92$Billionaries <- ATP_W92$BILLION_W92

crosstab(ATP_W92, Govt_Size, Billionaries,
         type="freq", plot=TRUE, chisquare=TRUE, na.rm = FALSE)
