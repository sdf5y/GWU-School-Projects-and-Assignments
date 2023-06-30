setwd('/Users/naissapierre/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/ECON 6374/Data Science Project')
library(readr)
library(data.table)
library(readxl)
library(tidyverse)
library(formattable)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)
library(corrplot)

# Import 2019 Data Set =================================================================

LAR2019 <- fread(input = "2019_public_lar_csv.csv")

# Keep data set with only MA state ======================================================

LAR2019_MA <- subset(LAR2019, LAR2019$state_code == "MA") 

# Import 2020 Data Set ==================================================================

#allocating more R memory in R
Sys.setenv('R_MAX_VSIZE'=32000000000000)
Sys.getenv('R_MAX_VSIZE')

# install.packages("usethis")
library(usethis) 
usethis::edit_r_environ()

LAR2020 <- fread(input = "2020_lar_csv.csv")

LAR2020_MA <- subset(LAR2020, LAR2020$state_code == 'MA')

LAR19_20_MA <- rbind(LAR2019_MA, LAR2020_MA)

#### 3 #### ============================================================================

# c ====================================================================================

LAR19_20_MA <- LAR19_20_MA %>%
  mutate(MA_County = str_sub(county_code,3,-1))
 
# d ====================================================================================
LAR19_20_MA$loan_amount <- currency(LAR19_20_MA$loan_amount,symbol = "$",digits = 0L,
                                    format = "f",big.mark = ",",sep = "")

#### 4 #### ============================================================================
# a.i ----------------------------------------------------------------------------------

LAR19_20_MA <- LAR19_20_MA %>%
  mutate(loan_type = factor(loan_type, 
                     levels = c(1,2,3,4),
                     labels = c('Conventional', 'FHA','VA','RHS or FSA')))

head(LAR19_20_MA$loan_type)

# a # ==================================================================================

Tbl4a <- LAR19_20_MA %>% group_by(loan_type) %>%
  dplyr::summarise(Min = min(loan_amount),
                   Mean = mean(loan_amount), 
                   Max = max(loan_amount))
Tbl4a

# b # ==================================================================================

Graph4b <- LAR19_20_MA %>% group_by(activity_year,loan_type) %>%
  dplyr::summarise(Min = min(loan_amount),
                   Mean = mean(loan_amount), 
                   Max = max(loan_amount))
Graph4b$activity_year <- as.factor(Graph4b$activity_year)

ggplot(Graph4b,aes(fill= loan_type, y = as.numeric(round(Mean/1000)), x = activity_year)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  labs(title = "Average Loan Values",
       y = 'Average Loan Value (thousands)',
       x = 'Year',
       fill = 'Loan Type')+
  geom_text(aes(label = as.numeric(round(Mean/1000))), 
            colour = "white", size = 3,
            vjust = 1.5, 
            position = position_dodge(.9)) +
  scale_fill_manual(values=c('navajowhite2','tan3','burlywood4','sienna4'))

# c # ==================================================================================
LAR19_20_MA <- LAR19_20_MA %>%
  mutate(manufactured_home_secured_property_type = factor(manufactured_home_secured_property_type, 
                            levels = c(1,2,3,1111),
                            labels = c('Manufactured Home and Land', 
                                       'Manufactured Home and Not Land',
                                       'Not Applicable',
                                       'Exempt')))

Data_1 <- data.frame(read.csv("MA_LAR_2019_2020.csv", na.strings = c("Exempt", "NA")))

Data_1 <- Data_1 %>%
  mutate(manufactured_home_secured_property_type = factor(manufactured_home_secured_property_type, 
                                                          levels = c(1,2,3,1111),
                                                          labels = c('Manufactured Home and Land', 
                                                                     'Manufactured Home and Not Land',
                                                                     'Not Applicable',
                                                                     'Exempt')))

# c # ==================================================================================

Tbl4c <- Data_1 %>% group_by(activity_year,manufactured_home_secured_property_type) %>%
  dplyr::summarise(Mean = mean(combined_loan_to_value_ratio,na.rm = TRUE))

Tbl4c$Mean <- suffix(round(Tbl4c$Mean, digits = 2), suffix = '%', na.text = 'NA')

Tbl4c <- pivot_wider(Tbl4c, values_from = 'Mean', names_from = 'activity_year')
names(Tbl4c)[names(Tbl4c) == '2019'] <- 'Mean 2019'
names(Tbl4c)[names(Tbl4c) == '2020'] <- 'Mean 2020'
names(Tbl4c)[names(Tbl4c) == 'manufactured_home_secured_property_type'] <- 'Property Type'
Tbl4c

# d.i # ================================================================================

LAR19_20_MA <- LAR19_20_MA %>%
  mutate(action_taken = factor(action_taken,
                               levels = c(1,2,3,4,5,6,7,8),
                               labels = c('Loan Originated',
                                          'Application Approved but Not Accepted',
                                          'Application Denied',
                                          'Application Withdrawn by Applicant',
                                          'File Closed for Incompleteness',
                                          'Purchased Loan',
                                          'Preapproval Request Denied',
                                          'Preapproval Request Approved but Not Accepted')))

head(LAR19_20_MA$action_taken)

Tbl4d <- table(LAR19_20_MA$action_taken, LAR19_20_MA$derived_sex)
Tbl4d 

# d.ii # ===============================================================================

P.Sex.NA <- suffix(round(sum(Tbl4d[,4])/sum(Tbl4d)*100,digits = 2),suffix = '%')
print(paste('The percentage not available is', P.Sex.NA))

library(randtests)
runs.test(Tbl4d[,4])
# This tests the null hypothesis that they are mutually independent and random.
# Since p > 5%, we fail to reject the null hypothesis that they are random. 

# e # ==================================================================================
Tbl4e <- round(prop.table(table(LAR19_20_MA$action_taken, LAR19_20_MA$derived_race)),5)*100
Tbl4e <- suffix(round(Tbl4e,digits = 2),suffix = '%')
Tbl4e

#### 5 #### ============================================================================

Tbl5a <- LAR19_20_MA %>% group_by(manufactured_home_secured_property_type) %>%
  dplyr::summarise(Min = min(loan_amount),
                   Mean = mean(loan_amount), 
                   Max = max(loan_amount)) %>%
  dplyr::rename('Property Type' = 'manufactured_home_secured_property_type')
Tbl5a 

Graph5b <- LAR19_20_MA %>% group_by(activity_year,manufactured_home_secured_property_type) %>%
  dplyr::summarise(Min = min(loan_amount),
                   Mean = mean(loan_amount), 
                   Max = max(loan_amount))
Graph5b$activity_year <- as.factor(Graph5b$activity_year)

ggplot(Graph5b,aes(fill= manufactured_home_secured_property_type, 
                   y = as.numeric(round(Mean/1000)), 
                   x = activity_year)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  labs(title = "Averages Loan Value",
       y = 'Average Loan Value (thousands)',
       x = 'Year',
       fill = 'Property Type')+
  geom_text(aes(label = as.numeric(round(Mean/1000))), 
            colour = "white", size = 3,
            vjust = 1.5, 
            position = position_dodge(.9))+
  scale_fill_manual(values=c('#fbb4b9','#f768a1','#ae017e','#7a0177'))

Tbl5c <- LAR19_20_MA %>% group_by(activity_year,manufactured_home_secured_property_type) %>%
  dplyr::summarise(Mean = mean(loan_amount)) %>%
  dplyr::rename('Average Loan Amount' = 'Mean') %>%
  dplyr::rename('Property Type' = 'manufactured_home_secured_property_type') %>%
  dplyr::rename('Year' = 'activity_year')

Tbl5c <- pivot_wider(Tbl5c, values_from = 'Average Loan Amount', names_from = 'Year')

# d # 

LAR19_20_MA <- LAR19_20_MA %>%
  mutate(preapproval = factor(preapproval,
                              levels = c(1,2),
                              labels = c('Preapproval requested',
                                         'Preapproval not requested')))

head(LAR19_20_MA$preapproval)
Table_5d <- table(LAR19_20_MA$action_taken, LAR19_20_MA$preapproval)
Table_5d

Graph5d <- LAR19_20_MA %>% group_by(action_taken,preapproval) %>%
  dplyr::summarise(Count = length(action_taken))

Graph5d <- pivot_wider(Graph5d, names_from = 'preapproval', values_from = 'Count')

Graph5d1 <- Graph5d %>% select(action_taken,`Preapproval requested`) %>%
  na.omit()

Graph5d2 <- Graph5d %>% select(action_taken,`Preapproval not requested`) %>%
  na.omit()


Graph1 <- ggplot(Graph5d1,
                 aes(fill= action_taken, 
                   y = `Preapproval requested`, 
                   x = action_taken)) + 
  theme(axis.text.x=element_blank())+
  geom_bar(position = 'dodge', stat = "identity") + 
  labs(y = 'Frequency',
       x = 'Preapproval',
       fill = 'Action Taken')+
  geom_text(aes(label = `Preapproval requested`), 
            colour = "white", size = 3,
            vjust = 1.5, 
            position = position_dodge(.9))+
  scale_fill_manual(values=c('#081d58', #loan or
                             '#225ea8', 
                             '#006837', #request denied
                             '#004529')) + #preaproval app na 
  scale_y_continuous(labels = comma)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

Graph1

Graph2 <- ggplot(Graph5d2,aes(fill= action_taken, 
                              y = `Preapproval not requested`, 
                              x = action_taken)) + 
    theme(axis.text.x=element_blank())+
    geom_bar(position = 'dodge', stat = "identity") + 
    labs(y = 'Frequency',
         x = 'Preapproval',
         fill = 'Action Taken')+
    geom_text(aes(label = `Preapproval not requested`), 
              colour = "white", size = 3,
              vjust = 1.5, 
              position = position_dodge(.9))+
    scale_fill_manual(values=c('#081d58', 
                               '#225ea8', 
                               '#1d91c0', 
                               '#7fcdbb',
                               '#41ab5d',
                               '#238443')) + 
  scale_y_continuous(labels = comma)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


title <- ggdraw() + 
  draw_label("Frequency of Each Actions with Preapproval",
             fontface = 'bold',
             x = 0,
             hjust = 0) 

Graphs <- plot_grid(Graph1,Graph2)
plot_grid(title,Graphs, ncol = 1,rel_heights = c(0.1,1))

#### 6 #### ============================================================================
ConventionalLAR <- subset(LAR19_20_MA, LAR19_20_MA$loan_type == "Conventional")

#### 7 #### ----------------------------------------------------------------------------

Q7Data <- ConventionalLAR %>% select(1,100,13,24, 47, 17, 41)

Q7Data <- Q7Data %>% mutate(interest_rate = replace(interest_rate, 
                                          interest_rate %in% 'Exempt', NA))
Q7Data <- Q7Data %>% mutate(interest_rate = as.numeric(interest_rate))

Q7Data <- Q7Data %>% mutate(debt_to_income_ratio = replace(debt_to_income_ratio, 
                                                           debt_to_income_ratio %in% 'Exempt', NA))
Q7Data <- Q7Data %>% mutate(MA_County = replace(MA_County,
                                                MA_County %in% NA, 'NA'))

Q7Data$activity_year <- as.factor(Q7Data$activity_year)
Q7Data$MA_County <- as.factor(Q7Data$MA_County)

# a # ==================================================================================
df7 <- aggregate(Q7Data$interest_rate, 
                 na.rm=TRUE, 
                 list(Q7Data$activity_year,Q7Data$MA_County), 
                 FUN = mean)
colnames(df7) <- c('Year', 'County', 'AverageInterestRate')

# b # ==================================================================================

tbl <- Q7Data %>% group_by(MA_County, activity_year) %>%
  dplyr::count(action_taken == 'Loan Originated')

tbl$MA_County[58:61]<-'NA'

## method using a custom function ## ---------------------------------------------------

percentage <- function(n) {
  counter = 1 #initialize
  LoanOriginated <- c() #create variable
  if(counter != 57){
  for (i in n) {
  x <- n[counter+1]
  y <-n[counter]
  z <- x/(x+y)
  LoanOriginated <- c(LoanOriginated, z)
  counter <- counter + 2
  if(counter==57){for(i in 57:57){
    z <- 0
    LoanOriginated <- c(LoanOriginated, z)
    counter <- counter + 1}}
  }
  }  
  LoanOriginated <- LoanOriginated %>% na.omit()
  return(data.frame(LoanOriginated))
}

df7$b <- percentage(tbl$n)*100
df7

# c # =================================================================================

tblc <- Q7Data %>% group_by(MA_County, activity_year) %>%
  dplyr::count(debt_to_income_ratio < 40) %>%
  na.omit()

p40 <- function(n) {
  counter = 1 #initialize
  DebtToIncomeGT40 <- c() #create variable
  if(counter != 57){
    for (i in n) {
      x <- n[counter+1]
      y <-n[counter]
      z <- x/(x+y)
      DebtToIncomeGT40 <- c(DebtToIncomeGT40, z)
      counter <- counter + 2
      if(counter==57){for(i in 57:57){
        z <- 1
        DebtToIncomeGT40 <- c(DebtToIncomeGT40, z)
        counter <- counter + 1}}
    }
  }  
  DebtToIncomeGT40 <- DebtToIncomeGT40 %>% na.omit()
  return(data.frame(DebtToIncomeGT40))
}

df7$c <- p40(tblc$n)*100

# d # =================================================================================

Q7Data <- Q7Data %>%
  mutate(loan_purpose = factor(loan_purpose,
                               levels = c(1,2,31,32,4,5),
                               labels = c("Home Purchase",
                                          "Home Improvement",
                                          "Refinancing",
                                          "Cash-Out Refinancing",
                                          "Other Purpose",
                                          "Not Applicable")))

Q7Data$loan_purpose <- fct_other(Q7Data$loan_purpose, keep = 'Home Purchase')

tbld <- as.data.frame(table(Q7Data$MA_County,Q7Data$activity_year,Q7Data$loan_purpose))

tbld <- pivot_wider(tbld, names_from = 'Var3', values_from = 'Freq')

tbld <- arrange(tbld,Var1)
tbld <- tbld[-c(30),]

names(tbld)[names(tbld) == 'Var1'] <- 'County'
names(tbld)[names(tbld) == 'Var2'] <- 'Year'

df7$d <- tbld$`Home Purchase`/(tbld$Other+tbld$`Home Purchase`)*100
names(df7)[names(df7) == 'd'] <- 'Percent Home Purchase'

# e # =====================================================================================

Q7Data <- Q7Data %>%
  mutate(occupancy_type = factor(occupancy_type,
                               levels = c(1,2,3),
                               labels = c("Principal Residence",
                                          "Second Residence",
                                          "Investment Property")))

Q7Data$occupancy_type <- fct_other(Q7Data$occupancy_type, 
                                   keep = 'Investment Property')

tble <- as.data.frame(table(Q7Data$MA_County,Q7Data$activity_year,Q7Data$occupancy_type))

tble <- pivot_wider(tble, names_from = 'Var3',values_from = 'Freq')

tble <- arrange(tble,Var1)

tble <- tble[-c(30),]

names(tble)[names(tble) == 'Var1'] <- 'County'
names(tble)[names(tble) == 'Var2'] <- 'Year'

df7$e <- tble$`Investment Property`/(tble$Other+tble$`Investment Property`)*100
names(df7)[names(df7) == 'e'] <- 'Percent Investment Property'

#### 8 #### --------------------------------------------------------------------------------------------

tbl8 <- ConventionalLAR %>% group_by(MA_County,activity_year) %>%
  dplyr::count(action_taken)

names(tbl8)[names(tbl8) == 'n'] <- 'Count'
tbl8 <- pivot_wider(tbl8, names_from = 'activity_year', values_from = 'Count')

tbl8$action_taken <- fct_other(tbl8$action_taken, keep = c('Loan Originated',
                                                           'Application Approved but Not Accepted',
                                                           'Purchased Loan',
                                                           'Preapproval Request Approved but Not Accepted'))

tbl8$action_taken <- fct_recode(tbl8$action_taken,
                                Approved = 'Loan Originated',
                                Approved = 'Application Approved but Not Accepted',
                                Approved = 'Purchased Loan',
                                Approved = 'Preapproval Request Approved but Not Accepted',
                                NotApproved = 'Other')

tbl8 <- tbl8 %>%
  group_by(MA_County, action_taken) %>% 
  dplyr::summarise(across(c('2019', '2020'), sum, na.rm=TRUE))

tbl8 <- subset(tbl8, action_taken=='Approved')
tbl8 <- subset(tbl8, select = -c(action_taken))

tbl8$Difference <- ((tbl8$`2020`-tbl8$`2019`)/tbl8$`2019`)*100

tbl8c <- subset(tbl8, select = c(MA_County,Difference)) %>% 
  arrange(-abs(Difference))

tbl8c <- tbl8c[1:10,]

tbl8c$Difference <- suffix(round(tbl8c$Difference, digits = 2),suffix = '%')
tbl8c

#### 9 #### --------------------------------------------------------------------------------------------

library(devtools)
library(blsAPI) 

NLP.API.Key <- '7ddf17e5bf7c4abe92b07c4f78072676'
#series identifier: LAUST2500000000000

payload <- list(
  'seriesid' = c('LAUCN250010000000004', 
                 'LAUCN250030000000004', 
                 'LAUCN250050000000004',
                 'LAUCN250070000000004',
                 'LAUCN250090000000004',
                 'LAUCN250110000000004',
                 'LAUCN250130000000004',
                 'LAUCN250150000000004',
                 'LAUCN250170000000004',
                 'LAUCN250190000000004',
                 'LAUCN250210000000004',
                 'LAUCN250230000000004',
                 'LAUCN250250000000004',
                 'LAUCN250270000000004'),
  'startyear' = 2019,
  'endyear' = 2020,
  'calculations' = TRUE,
  'annualaverage' = TRUE,
  'catalog' = TRUE,
  'registrationKey' = NLP.API.Key)
BLSData <- blsAPI(payload, return_data_frame = TRUE)

BLSData <- BLSData[BLSData$periodName == "Annual", ]

BLSData <- BLSData %>%
  mutate(MA_County = str_sub(seriesID,8,10))
BLSData <- subset(BLSData, select = -c(seriesID,period,periodName))
names(BLSData)[names(BLSData) == 'year'] <- 'date'
names(BLSData)[names(BLSData) == 'value'] <- 'UELevel'

#### 10 #### -------------------------------------------------------------------------------------------

library(fredr)
NLP.FRED.Key <- '2188667b23d9842eb3b6230c35f3c692'
fredr_set_key(NLP.FRED.Key)

# EQUIFAX # ===============================================================================

EQFX.Data <- {fredr( #001 '19
    series_id = 'EQFXSUBPRIME025001', 
    observation_start = as.Date('2019-10-01'), 
    observation_end = as.Date('2019-12-31')) %>% 
  add_row(
    fredr( #001 '20
      series_id = 'EQFXSUBPRIME025001',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #003 '19
      series_id = 'EQFXSUBPRIME025003',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #003 '20
      series_id = 'EQFXSUBPRIME025003',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #005 '19
      series_id = 'EQFXSUBPRIME025005',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #005 '20
      series_id = 'EQFXSUBPRIME025005',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #007 '19
      series_id = 'EQFXSUBPRIME025007',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #007 '20
      series_id = 'EQFXSUBPRIME025007',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #009 '19
      series_id = 'EQFXSUBPRIME025009',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #009 '20
      series_id = 'EQFXSUBPRIME025009',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #011 '19
      series_id = 'EQFXSUBPRIME025011',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #011 '20
      series_id = 'EQFXSUBPRIME025011',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #013 '19
      series_id = 'EQFXSUBPRIME025013',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #013 '20
      series_id = 'EQFXSUBPRIME025013',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #015 '19
      series_id = 'EQFXSUBPRIME025015',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #015 '20
      series_id = 'EQFXSUBPRIME025015',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #017 '19
      series_id = 'EQFXSUBPRIME025017',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #017 '20
      series_id = 'EQFXSUBPRIME025017',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #019 '19
      series_id = 'EQFXSUBPRIME025019',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #019 '20
      series_id = 'EQFXSUBPRIME025019',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #021 '19
      series_id = 'EQFXSUBPRIME025021',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #021 '20
      series_id = 'EQFXSUBPRIME025021',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #023 '19
      series_id = 'EQFXSUBPRIME025023',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #023 '20
      series_id = 'EQFXSUBPRIME025023',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #025 '19
      series_id = 'EQFXSUBPRIME025025',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #025 '20
      series_id = 'EQFXSUBPRIME025025',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #027 '19
      series_id = 'EQFXSUBPRIME025027',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #027 '20
      series_id = 'EQFXSUBPRIME025027',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31')))
}
  
EQFX.Data <- EQFX.Data %>%
  mutate(MA_County = str_sub(series_id, 16,-1))

EQFX.Data <- subset(EQFX.Data, select = -c(realtime_start,realtime_end))
EQFX.Data <- subset(EQFX.Data, select = -c(series_id))
EQFX.Data$date <- as.numeric(format(EQFX.Data$date, "%Y"))
names(EQFX.Data)[names(EQFX.Data) == 'value'] <- 'ECSB660'

# Prev Hosp Adm # =========================================================================

PHA.Data <- {fredr( #001 '19
    series_id = 'DMPCRATE025001', 
    observation_start = as.Date('2019-10-01'), 
    observation_end = as.Date('2019-12-31')) %>% 
  add_row(
    fredr( #001 '20
      series_id = 'DMPCRATE025001',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #003 '19
      series_id = 'DMPCRATE025003',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #003 '20
      series_id = 'DMPCRATE025003',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #005 '19
      series_id = 'DMPCRATE025005',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #005 '20
      series_id = 'DMPCRATE025005',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #007 '19
      series_id = 'DMPCRATE025007',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #007 '20
      series_id = 'DMPCRATE025007',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #009 '19
      series_id = 'DMPCRATE025009',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #009 '20
      series_id = 'DMPCRATE025009',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #011 '19
      series_id = 'DMPCRATE025011',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #011 '20
      series_id = 'DMPCRATE025011',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #013 '19
      series_id = 'DMPCRATE025013',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #013 '20
      series_id = 'DMPCRATE025013',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #015 '19
      series_id = 'DMPCRATE025015',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #015 '20
      series_id = 'DMPCRATE025015',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #017 '19
      series_id = 'DMPCRATE025017',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #017 '20
      series_id = 'DMPCRATE025017',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #019 '19
      series_id = 'DMPCRATE025019',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #019 '20
      series_id = 'DMPCRATE025019',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #021 '19
      series_id = 'DMPCRATE025021',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #021 '20
      series_id = 'DMPCRATE025021',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #023 '19
      series_id = 'DMPCRATE025023',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #023 '20
      series_id = 'DMPCRATE025023',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #025 '19
      series_id = 'DMPCRATE025025',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #025 '20
      series_id = 'DMPCRATE025025',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #027 '19
      series_id = 'DMPCRATE025027',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #027 '20
      series_id = 'DMPCRATE025027',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31')))
}
# most recent data from 2015 so it's all NA

# Housing Inventory # =====================================================================

HI.Data <- {fredr( #001 '19
  series_id = 'ACTLISCOU25001', 
  observation_start = as.Date('2019-10-01'), 
  observation_end = as.Date('2019-12-31')) %>% 
  add_row(
    fredr( #001 '20
      series_id = 'ACTLISCOU25001',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #003 '19
      series_id = 'ACTLISCOU25003',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #003 '20
      series_id = 'ACTLISCOU25003',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #005 '19
      series_id = 'ACTLISCOU25005',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #005 '20
      series_id = 'ACTLISCOU25005',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
#  add_row(series_id = 'ACTLISCOU25007', 
#          date = as.Date('2019-10-01'), 
#          value = NA,
#          realtime_start = NA,
#          realtime_end = NA) %>% #no data for this county
#  add_row(series_id = 'ACTLISCOU25007', 
#          date = as.Date('2020-10-01')) %>% #no data for this county
  add_row(
    fredr( #009 '19
      series_id = 'ACTLISCOU25009',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #009 '20
      series_id = 'ACTLISCOU25009',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #011 '19
      series_id = 'ACTLISCOU25011',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #011 '20
      series_id = 'ACTLISCOU25011',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #013 '19
      series_id = 'ACTLISCOU25013',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #013 '20
      series_id = 'ACTLISCOU25013',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #015 '19
      series_id = 'ACTLISCOU25015',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #015 '20
      series_id = 'ACTLISCOU25015',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #017 '19
      series_id = 'ACTLISCOU25017',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #017 '20
      series_id = 'ACTLISCOU25017',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
#  add_row(series_id = 'ACTLISCOU25019',
#          date = as.Date('2019-10-01')) %>% # no data for this county
#  add_row(series_id = 'ACTLISCOU25019', 
#          date = as.Date('2020-10-01')) %>% # no data for this county
  add_row(
    fredr( #021 '19
      series_id = 'ACTLISCOU25021',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #021 '20
      series_id = 'ACTLISCOU25021',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #023 '19
      series_id = 'ACTLISCOU25023',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #023 '20
      series_id = 'ACTLISCOU25023',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #025 '19
      series_id = 'ACTLISCOU25025',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #025 '20
      series_id = 'ACTLISCOU25025',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #027 '19
      series_id = 'ACTLISCOU25027',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #027 '20
      series_id = 'ACTLISCOU25027',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31')))
}
  
HI.Data <- HI.Data %>%
  mutate(MA_County = str_sub(series_id,12,-1))
HI.Data <- subset(HI.Data, select = -c(realtime_start,realtime_end,series_id))
HI.Data$date <- as.numeric(format(HI.Data$date, "%Y"))

HI.Data <- HI.Data %>%
  group_by(MA_County, date) %>% 
  dplyr::summarise(across(value, mean, na.rm=FALSE))

names(HI.Data)[names(HI.Data) == 'value'] <- 'AL_Count'

# Average Commute Times # ================================================================

ACT.Data <- {fredr( #001 '19
  series_id = 'B080ACS025001', 
  observation_start = as.Date('2019-10-01'), 
  observation_end = as.Date('2019-12-31')) %>% 
  add_row(
    fredr( #001 '20
      series_id = 'B080ACS025001',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #003 '19
      series_id = 'B080ACS025003',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #003 '20
      series_id = 'B080ACS025003',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #005 '19
      series_id = 'B080ACS025005',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #005 '20
      series_id = 'B080ACS025005',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row( 
    fredr( #007 '19
      series_id = 'B080ACS025007',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #007 '20
      series_id = 'B080ACS025007',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #009 '19
      series_id = 'B080ACS025009',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #009 '20
      series_id = 'B080ACS025009',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #011 '19
      series_id = 'B080ACS025011',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #011 '20
      series_id = 'B080ACS025011',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #013 '19
      series_id = 'B080ACS025013',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #013 '20
      series_id = 'B080ACS025013',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #015 '19
      series_id = 'B080ACS025015',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #015 '20
      series_id = 'B080ACS025015',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #017 '19
      series_id = 'B080ACS025017',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #017 '20
      series_id = 'B080ACS025017',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row( 
    fredr( #019 '19
      series_id = 'B080ACS025019',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #019 '20
      series_id = 'B080ACS025019',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #021 '19
      series_id = 'B080ACS025021',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #021 '20
      series_id = 'B080ACS025021',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #023 '19
      series_id = 'B080ACS025023',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #023 '20
      series_id = 'B080ACS025023',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #025 '19
      series_id = 'B080ACS025025',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #025 '20
      series_id = 'B080ACS025025',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31'))) %>% 
  add_row(
    fredr( #027 '19
      series_id = 'B080ACS025027',
      observation_start = as.Date('2019-10-01'),
      observation_end = as.Date('2019-12-31'))) %>% 
  add_row(
    fredr( #027 '20
      series_id = 'B080ACS025027',
      observation_start = as.Date('2020-10-01'),
      observation_end = as.Date('2020-12-31')))
}
  
ACT.Data <- ACT.Data %>%
  mutate(MA_County = str_sub(series_id,11,-1))
ACT.Data <- subset(ACT.Data, select = -c(realtime_start,realtime_end,series_id))
ACT.Data$date <- as.numeric(format(ACT.Data$date, "%Y"))
names(ACT.Data)[names(ACT.Data) == 'value'] <- 'Commute.mins'

# Comb Violent and Property Crime # =====================================================
 
# No data available for Massachusetts 

#### 11 #### --------------------------------------------------------------------------------

# Data was imported using API Key and is already limited to MA State

df10 <-  merge(EQFX.Data, ACT.Data, by = c('MA_County','date'))
df10 <- left_join(df10, 
                  HI.Data, 
                  by = NULL, 
                  copy = FALSE, 
                  suffix = c(".df10", ".HI.Data"), 
                  keep = FALSE, 
                  na_matches = "na")
df10$PHA <- c(rep(NA,28))
df10$Crime <- c(rep(NA,28))
# They are NA but created the empty columns anyways

#### 13 #### -----------------------------------------------------------------------------

# Population # ===========================================================================

Census.Data <- {fredr( #001 '19
  series_id = 'MABARN1POP', 
  observation_start = as.Date('2019-10-01'), 
  observation_end = as.Date('2019-12-31')) %>% 
    add_row(
      fredr( #001 '20
        series_id = 'MABARN1POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #003 '19
        series_id = 'MABERK2POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #003 '20
        series_id = 'MABERK2POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #005 '19
        series_id = 'MABRIS5POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #005 '20
        series_id = 'MABRIS5POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #007 '19
        series_id = 'MADUKE7POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #007 '20
        series_id = 'MADUKE7POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #009 '19
        series_id = 'MAESSE9POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #009 '20
        series_id = 'MAESSE9POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #011 '19
        series_id = 'MAFRAN1POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #011 '20
        series_id = 'MAFRAN1POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #013 '19
        series_id = 'MAHAMP0POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #013 '20
        series_id = 'MAHAMP0POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #015 '19
        series_id = 'MAHAMP5POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #015 '20
        series_id = 'MAHAMP5POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #017 '19
        series_id = 'MAMIDD7POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #017 '20
        series_id = 'MAMIDD7POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #019 '19
        series_id = 'MANANT9POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #019 '20
        series_id = 'MANANT9POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #021 '19
        series_id = 'MANORF1POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #021 '20
        series_id = 'MANORF1POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #023 '19
        series_id = 'MAPLYM3POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #023 '20
        series_id = 'MAPLYM3POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #025 '19
        series_id = 'MASUFF5POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #025 '20
        series_id = 'MASUFF5POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #027 '19
        series_id = 'MAWORC7POP',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #027 '20
        series_id = 'MAWORC7POP',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31')))
}

#because of the formatting we had to manually rename them all
Census.Data$series_id <- fct_recode(Census.Data$series_id,
                                    '001' = 'MABARN1POP', 
                                    '003' = 'MABERK2POP',
                                    '005' = 'MABRIS5POP',
                                    '007' = 'MADUKE7POP',
                                    '009' = 'MAESSE9POP',
                                    '011' = 'MAFRAN1POP',
                                    '013' = 'MAHAMP0POP',
                                    '015' = 'MAHAMP5POP',
                                    '017' = 'MAMIDD7POP',
                                    '019' = 'MANANT9POP',
                                    '021' = 'MANORF1POP',
                                    '023' = 'MAPLYM3POP',
                                    '025' = 'MASUFF5POP',
                                    '027' = 'MAWORC7POP')
names(Census.Data)[names(Census.Data) == 'series_id'] <- 'MA_County'
Census.Data <- subset(Census.Data, select = -c(realtime_start,realtime_end))
Census.Data$date <- as.numeric(format(Census.Data$date, "%Y"))
names(Census.Data)[names(Census.Data) == 'value'] <- 'Population.Thou'

# SNAP # =================================================================================

SNAP.Data <- {fredr( #001 '19
      series_id = 'CBR25001MAA647NCEN', 
      observation_start = as.Date('2019-10-01'), 
      observation_end = as.Date('2019-12-31')) %>% 
    add_row(
      fredr( #001 '20
        series_id = 'CBR25001MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #003 '19
        series_id = 'CBR25003MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #003 '20
        series_id = 'CBR25003MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #005 '19
        series_id = 'CBR25005MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #005 '20
        series_id = 'CBR25005MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #007 '19
        series_id = 'CBR25007MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #007 '20
        series_id = 'CBR25007MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #009 '19
        series_id = 'CBR25009MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #009 '20
        series_id = 'CBR25009MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #011 '19
        series_id = 'CBR25011MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #011 '20
        series_id = 'CBR25011MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #013 '19
        series_id = 'CBR25013MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #013 '20
        series_id = 'CBR25013MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #015 '19
        series_id = 'CBR25015MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #015 '20
        series_id = 'CBR25015MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #017 '19
        series_id = 'CBR25017MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #017 '20
        series_id = 'CBR25017MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #019 '19
        series_id = 'CBR25019MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #019 '20
        series_id = 'CBR25019MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #021 '19
        series_id = 'CBR25021MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #021 '20
        series_id = 'CBR25021MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #023 '19
        series_id = 'CBR25023MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #023 '20
        series_id = 'CBR25023MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #025 '19
        series_id = 'CBR25025MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #025 '20
        series_id = 'CBR25025MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #027 '19
        series_id = 'CBR25027MAA647NCEN',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #027 '20
        series_id = 'CBR25027MAA647NCEN',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31')))
}

SNAP.Data <- SNAP.Data %>%
  mutate(MA_County = str_sub(series_id,6,8))
SNAP.Data <- subset(SNAP.Data, select = -c(realtime_start,realtime_end,series_id))
SNAP.Data$date <- as.numeric(format(SNAP.Data$date, "%Y"))
names(SNAP.Data)[names(SNAP.Data) == 'value'] <- 'SNAP.Participants'

# High School Grads # ====================================================================

HSG.Data <- {fredr( #001 '19
      series_id = 'HC01ESTVC1625001', 
      observation_start = as.Date('2019-10-01'), 
      observation_end = as.Date('2019-12-31')) %>% 
    add_row(
      fredr( #001 '20
        series_id = 'HC01ESTVC1625001',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #003 '19
        series_id = 'HC01ESTVC1625003',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #003 '20
        series_id = 'HC01ESTVC1625003',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #005 '19
        series_id = 'HC01ESTVC1625005',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #005 '20
        series_id = 'HC01ESTVC1625005',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #007 '19
        series_id = 'HC01ESTVC1625007',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #007 '20
        series_id = 'HC01ESTVC1625007',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #009 '19
        series_id = 'HC01ESTVC1625009',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #009 '20
        series_id = 'HC01ESTVC1625009',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #011 '19
        series_id = 'HC01ESTVC1625011',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #011 '20
        series_id = 'HC01ESTVC1625011',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #013 '19
        series_id = 'HC01ESTVC1625013',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #013 '20
        series_id = 'HC01ESTVC1625013',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #015 '19
        series_id = 'HC01ESTVC1625015',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #015 '20
        series_id = 'HC01ESTVC1625015',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #017 '19
        series_id = 'HC01ESTVC1625017',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #017 '20
        series_id = 'HC01ESTVC1625017',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #019 '19
        series_id = 'HC01ESTVC1625019',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #019 '20
        series_id = 'HC01ESTVC1625019',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #021 '19
        series_id = 'HC01ESTVC1625021',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #021 '20
        series_id = 'HC01ESTVC1625021',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #023 '19
        series_id = 'HC01ESTVC1625023',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #023 '20
        series_id = 'HC01ESTVC1625023',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #025 '19
        series_id = 'HC01ESTVC1625025',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #025 '20
        series_id = 'HC01ESTVC1625025',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #027 '19
        series_id = 'HC01ESTVC1625027',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #027 '20
        series_id = 'HC01ESTVC1625027',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31')))
}

HSG.Data <- HSG.Data %>%
  mutate(MA_County = str_sub(series_id,14,-1))
HSG.Data <- subset(HSG.Data, select = -c(realtime_start,realtime_end,series_id))
HSG.Data$date <- as.numeric(format(HSG.Data$date, "%Y"))
names(HSG.Data)[names(HSG.Data) == 'value'] <- 'HSGraduates' #percentage

# Income Inequality # ====================================================================

II.Data <- {fredr( #001 '19
  series_id = '2020RATIO025001', 
  observation_start = as.Date('2019-10-01'), 
  observation_end = as.Date('2019-12-31')) %>% 
    add_row(
      fredr( #001 '20
        series_id = '2020RATIO025001',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #003 '19
        series_id = '2020RATIO025003',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #003 '20
        series_id = '2020RATIO025003',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #005 '19
        series_id = '2020RATIO025005',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #005 '20
        series_id = '2020RATIO025005',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #007 '19
        series_id = '2020RATIO025007',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #007 '20
        series_id = '2020RATIO025007',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #009 '19
        series_id = '2020RATIO025009',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #009 '20
        series_id = '2020RATIO025009',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #011 '19
        series_id = '2020RATIO025011',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #011 '20
        series_id = '2020RATIO025011',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #013 '19
        series_id = '2020RATIO025013',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #013 '20
        series_id = '2020RATIO025013',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #015 '19
        series_id = '2020RATIO025015',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #015 '20
        series_id = '2020RATIO025015',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #017 '19
        series_id = '2020RATIO025017',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #017 '20
        series_id = '2020RATIO025017',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row( 
      fredr( #019 '19
        series_id = '2020RATIO025019',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #019 '20
        series_id = '2020RATIO025019',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #021 '19
        series_id = '2020RATIO025021',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #021 '20
        series_id = '2020RATIO025021',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #023 '19
        series_id = '2020RATIO025023',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #023 '20
        series_id = '2020RATIO025023',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #025 '19
        series_id = '2020RATIO025025',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #025 '20
        series_id = '2020RATIO025025',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31'))) %>% 
    add_row(
      fredr( #027 '19
        series_id = '2020RATIO025027',
        observation_start = as.Date('2019-10-01'),
        observation_end = as.Date('2019-12-31'))) %>% 
    add_row(
      fredr( #027 '20
        series_id = '2020RATIO025027',
        observation_start = as.Date('2020-10-01'),
        observation_end = as.Date('2020-12-31')))
}

II.Data <- II.Data %>%
  mutate(MA_County = str_sub(series_id,13,-1))
II.Data <- subset(II.Data, select = -c(realtime_start,realtime_end,series_id))
II.Data$date <- as.numeric(format(II.Data$date, "%Y"))
names(II.Data)[names(II.Data) == 'value'] <- 'Inc.Ineq.Ratio'


# Final Table # ==========================================================================

df13 <- merge(Census.Data, II.Data, by = c('MA_County','date'))
df13 <- left_join(df13, 
                  HSG.Data, 
                  by = NULL, 
                  copy = FALSE, 
                  suffix = c(".df13", ".HSG.Data"), 
                  keep = FALSE, 
                  na_matches = "na") 
df13 <- left_join(df13, 
                  SNAP.Data, 
                  by = NULL,
                  copy = FALSE, 
                  suffix = c(".df13", ".SNAP.Data"), 
                  keep = FALSE, 
                  na_matches = "na") # missing 2020 data

#### 15 #### -----------------------------------------------------------------------------

df15 <- merge(BLSData,df10, by = c('MA_County','date') )

df15 <- merge(df15, df13, by = c('MA_County','date') )

## We are missing data for Crime and Preventable Hospital Admissions for 
## MA because there are none, specifically the Hospital Admissions Data for MA 
## ends in 2015. We are also missing the SNAP Participation numbers for 
## 2020 because they are not available. We are lastly missing Active Listing Count for 
## Dukes County and Nantucket County/town because they are not available. 
## We spoke to you in class about excluding the Hospital data since we had done it 
## prior to the email about the new data to use, so we will be using Graduation data
## in it's place for leveling. 

#### 16 #### -----------------------------------------------------------------------------

# Hospital Admissions Data does not exist for 2019 or 2020,
# so we will complete this using Grad Rates. For 
# Graduation Rates we binned it based on the natural jenks
# of the distribution. 

df16 <- df15 %>%
  mutate(Grad.Rate = case_when(HSGraduates < 87 ~ 'Low',
                               HSGraduates < 92.6 ~ 'Middle',
                               HSGraduates >= 92.6 ~ 'High')) %>%
  relocate(Grad.Rate, .after = HSGraduates)

Loan.Data <- Q7Data %>% group_by(MA_County, activity_year) %>%
  dplyr::count(action_taken == 'Loan Originated') 

Loan.Data <- Loan.Data[Loan.Data$`action_taken == "Loan Originated"` == TRUE, ]
Loan.Data <- Loan.Data[!Loan.Data$MA_County == 'NA',]
Loan.Data <- subset(Loan.Data, select = -c(`action_taken == "Loan Originated"`))
names(Loan.Data)[names(Loan.Data) == 'activity_year'] <- 'date'
names(Loan.Data)[names(Loan.Data) == 'n'] <- 'Loan.Count'

df16 <- merge(df16, Loan.Data, by = c('MA_County','date') ) %>%
  relocate(Loan.Count, .before = Population.Thou)

df16 <- df16 %>% mutate(LoansPerCapita = df16$Loan.Count/df16$Population.Thou) %>%
  relocate(LoansPerCapita, .after = Population.Thou)

#### 17 #### -----------------------------------------------------------------------------

plot17 <- ggplot(HI.Data,
                 aes(x = AL_Count), 
                 y =..density..) +
  geom_histogram(aes(x = AL_Count,y =..density..),
                 binwidth = 250, 
                 color = 'orange', 
                 fill = 'pink')+ 
  geom_density(kernel = 'gaussian',
               color = 'darkorange',
               fill = 'deeppink',
               alpha = 0.27) +
  annotate(geom = 'text', 
           x = 2355, y = 0.00026, 
           label = 'Kernel Density', 
           color = 'darkorange') +
  labs(title = "Housing Inventory Histogram",
       y = 'Density',
       x = 'Active Listings') +
  scale_y_continuous(labels = label_comma()) +
  geom_vline(aes(xintercept=mean(AL_Count),
                 color='Mean'), 
             linetype="dashed", size=.5) +
  geom_vline(aes(xintercept = median(AL_Count),
                 color='Median'),
             linetype='dashed', size=.5) +
  scale_color_manual(name = "Statistics", 
                     values = c(Mean = "coral1", 
                                Median = "coral3")) +
  stat_function(fun = dnorm,
                args = list(mean = mean(HI.Data$AL_Count),
                            sd = sd(HI.Data$AL_Count)),
                color = 'cornsilk4') +
  annotate(geom = 'text', x = 1750, y = 0.0005, label = 'Normal', color = 'cornsilk4')

plot17

## The mean and median, which can be seen on the graph with dashed lines, are not equal.
## This means this is not a normal distribution, however, they are close. The median, 
## which is more resistant to outliers, is lower than the mean, which is farther to the 
## right, matching the skew of the distribution.

Ref.Data <- ConventionalLAR %>% 
  select(MA_County,
         activity_year,
         loan_purpose) 
Ref.Data <- Ref.Data %>% 
  mutate(loan_purpose = factor(loan_purpose,
                               levels = c(1,2,31,32,4,5),
                               labels = c("Home Purchase",
                                          "Home Improvement",
                                          "Refinancing",
                                          "Cash-Out Refinancing",
                                          "Other Purpose",
                                          "Not Applicable")))
Ref.Data$loan_purpose <- fct_other(Ref.Data$loan_purpose,
                                   keep = c('Refinancing','Cash-Out Refinancing'),
                                   other_level = 'Not Refinanced') 
Ref.Data$loan_purpose <- fct_recode(Ref.Data$loan_purpose,
                                    Refinanced = 'Refinancing',
                                    Refinanced = 'Cash-Out Refinancing') 
Ref.Data <- as.data.frame(table(Ref.Data$MA_County,
                                Ref.Data$activity_year,
                                Ref.Data$loan_purpose))
Ref.Data <- pivot_wider(Ref.Data, names_from = 'Var3', values_from = 'Freq')

Ref.Data <- arrange(Ref.Data,Var1)
Ref.Data <- Ref.Data[-c(29,30),]

names(Ref.Data)[names(Ref.Data) == 'Var1'] <- 'MA_County'
names(Ref.Data)[names(Ref.Data) == 'Var2'] <- 'date'
Ref.Data$Percentage <- Ref.Data$Refinanced/
  (Ref.Data$Refinanced+Ref.Data$`Not Refinanced`)*100
Ref.Data <- Ref.Data %>% select(-c('Refinanced','Not Refinanced'))

Ref.Data <- merge(Ref.Data,df16, by = c('MA_County','date'))

Ref.Data <- Ref.Data %>% select(MA_County, date, Percentage, Grad.Rate)

ggplot(Ref.Data,aes(fill= Grad.Rate, y = Percentage, x = date)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  labs(title = "Loan Refinancing Percentages",
       y = 'Loan Refinancing (Percent)',
       x = 'Year',
       fill = 'Overall Graduation Rate') +
  geom_text(aes(label = round(Percentage, digits = 2)), 
            colour = "white", size = 3,
            vjust = 1.5,
            position = position_dodge(.9)) +
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#225ea8'))

## This is the requested table, however since the labeling is not legible 
## and the bar chart really only shows the max, below, we will plot it so 
## the labeling is legible.

Ref.Plot.Data <- Ref.Data %>% group_by(date,Grad.Rate) %>%
  dplyr::summarise(Mean = max(Percentage))

ggplot(Ref.Plot.Data,aes(fill= Grad.Rate, y = Mean, x = date)) + 
  geom_bar(position = 'dodge', stat = "identity") + 
  labs(title = "Loan Refinancing Percentages",
       y = 'Loan Refinancing (Percent)',
       x = 'Year',
       fill = 'Overall Graduation Rate')+
  geom_text(aes(label = round(Mean, digits = 2)), 
            colour = "white", size = 3,
            vjust = 1.5, 
            position = position_dodge(.9)) +
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#225ea8'))

#### 18 #### -----------------------------------------------------------------------------

df18 <- subset(df16, df16$date == "2019")

df18 <- df18 %>% mutate(UELevel = log(as.numeric(UELevel))) %>%
  dplyr::rename('log_UELevel' = 'UELevel') %>%
  mutate(ECSB660 = log(ECSB660)) %>%
  dplyr::rename('log_ECSB660' = 'ECSB660') %>%
  mutate(Commute.mins = log(Commute.mins)) %>%
  dplyr::rename('log_Commute.mins' = 'Commute.mins') %>%
  mutate(AL_Count = log(AL_Count)) %>%
  dplyr::rename('log_AL_Count' = 'AL_Count') %>%
  mutate(Loan.Count = log(Loan.Count)) %>%
  dplyr::rename('log_Loan.Count' = 'Loan.Count') %>%
  mutate(Population.Thou = log(Population.Thou)) %>%
  dplyr::rename('log_Population.Thou' = 'Population.Thou') %>%
  mutate(LoansPerCapita = log(LoansPerCapita)) %>%
  dplyr::rename('log_LoansPerCapita' = 'LoansPerCapita') %>%
  mutate(Inc.Ineq.Ratio = log(Inc.Ineq.Ratio)) %>%
  dplyr::rename('log_Inc.Ineq.Ratio' = 'Inc.Ineq.Ratio') %>%
  mutate(HSGraduates = log(HSGraduates)) %>%
  dplyr::rename('log_HSGraduates' = 'HSGraduates') %>%
  mutate(SNAP.Participants = log(SNAP.Participants)) %>%
  dplyr::rename('log_SNAP.Participants' = 'SNAP.Participants') 

LoanAmount.Data <- ConventionalLAR %>% select(MA_County,activity_year,loan_amount)

LoanAmount.Data <- LoanAmount.Data %>% group_by(MA_County,activity_year) %>%
  dplyr::summarise(Avg.Loan.Amount = mean(loan_amount))  
  
LoanAmount.Data <- subset(LoanAmount.Data, LoanAmount.Data$activity_year == '2019') 

LoanAmount.Data <- LoanAmount.Data[-c(15,16),]

LoanAmount.Data <- LoanAmount.Data %>%
  dplyr::rename('date'='activity_year')

LoanAmount.Data$date <- as.character(LoanAmount.Data$date)

df18 <- left_join(df18, 
                  LoanAmount.Data, 
                  by = NULL,
                  copy = FALSE, 
                  suffix = c(".df18", ".LoanAmount.Data"), 
                  keep = FALSE, 
                  na_matches = "na")

df18 <- df18 %>% 
  relocate(Avg.Loan.Amount, .after = log_LoansPerCapita) %>%
  mutate(Avg.Loan.Amount = log(Avg.Loan.Amount)) %>%
  dplyr::rename('log_Avg.Loan.Amount' = 'Avg.Loan.Amount')

CorMatrix1.Data <- df18 %>% 
  select(-c(log_Loan.Count,log_LoansPerCapita,MA_County,date,PHA,Crime,Grad.Rate))

CorMatrix1 <- round(cor(CorMatrix1.Data[ ,colnames(CorMatrix1.Data) != 'log_Avg.Loan.Amount'],  
                        CorMatrix1.Data$log_Avg.Loan.Amount,use='pairwise.complete.obs'),
                    digits = 2L)
CorMatrix1

CorMatrix2.Data <- df18 %>% 
  select(-c(MA_County,date,PHA,Crime,Grad.Rate))

CorMatrix2 <- round(cor(CorMatrix2.Data[ ,colnames(CorMatrix2.Data) != 'log_Avg.Loan.Amount'],  
                        CorMatrix2.Data$log_Avg.Loan.Amount, use='pairwise.complete.obs'),
                    digits = 2L)

CorMatrix2 

corrplot(CorMatrix1)
corrplot(CorMatrix2)

AllCorr <- cor(CorMatrix2.Data, method = c("spearman"),use='pairwise.complete.obs')
corrplot(AllCorr)

## Number of Loans Per Capita has the highest Correlation Coefficient (0.8)

#### 19 #### -----------------------------------------------------------------------------

ggplot(CorMatrix2.Data, aes(x=log_LoansPerCapita, y=log_Avg.Loan.Amount)) + 
  geom_point(color = 'orange',
             fill = 'pink') +
  geom_smooth(method=lm, color = 'hotpink', se=FALSE) +
  labs(title = "Strongest Correlation Scatterplot",
       y = 'log(Loan Amount)',
       x = 'log(Loans Per Capita)') +
  theme_classic()


df19 <- df16 %>% mutate(UELevel = log(as.numeric(UELevel))) %>%
  dplyr::rename('log_UELevel' = 'UELevel') %>%
  mutate(ECSB660 = log(ECSB660)) %>%
  dplyr::rename('log_ECSB660' = 'ECSB660') %>%
  mutate(Commute.mins = log(Commute.mins)) %>%
  dplyr::rename('log_Commute.mins' = 'Commute.mins') %>%
  mutate(AL_Count = log(AL_Count)) %>%
  dplyr::rename('log_AL_Count' = 'AL_Count') %>%
  mutate(Loan.Count = log(Loan.Count)) %>%
  dplyr::rename('log_Loan.Count' = 'Loan.Count') %>%
  mutate(Population.Thou = log(Population.Thou)) %>%
  dplyr::rename('log_Population.Thou' = 'Population.Thou') %>%
  mutate(LoansPerCapita = log(LoansPerCapita)) %>%
  dplyr::rename('log_LoansPerCapita' = 'LoansPerCapita') %>%
  mutate(Inc.Ineq.Ratio = log(Inc.Ineq.Ratio)) %>%
  dplyr::rename('log_Inc.Ineq.Ratio' = 'Inc.Ineq.Ratio') %>%
  mutate(HSGraduates = log(HSGraduates)) %>%
  dplyr::rename('log_HSGraduates' = 'HSGraduates') %>%
  mutate(SNAP.Participants = log(SNAP.Participants)) %>%
  dplyr::rename('log_SNAP.Participants' = 'SNAP.Participants') 

LoanAmount.Data2 <- ConventionalLAR %>% select(MA_County,activity_year,loan_amount)

LoanAmount.Data2 <- LoanAmount.Data2 %>% group_by(MA_County,activity_year) %>%
  dplyr::summarise(Avg.Loan.Amount = mean(loan_amount))  

LoanAmount.Data2 <- LoanAmount.Data2[-c(29,30,31),]

LoanAmount.Data2 <- LoanAmount.Data2 %>%
  dplyr::rename('date'='activity_year')

LoanAmount.Data2$date <- as.character(LoanAmount.Data2$date)

df19 <- left_join(df19, 
                  LoanAmount.Data2, 
                  by = NULL,
                  copy = FALSE, 
                  suffix = c(".df19", ".LoanAmount.Data2"), 
                  keep = FALSE, 
                  na_matches = "na")

df19 <- df19 %>% 
  relocate(Avg.Loan.Amount, .after = log_LoansPerCapita) %>%
  mutate(Avg.Loan.Amount = log(Avg.Loan.Amount)) %>%
  dplyr::rename('log_Avg.Loan.Amount' = 'Avg.Loan.Amount')

CorMatrix19.2.Data <- df19 %>% 
  select(-c(MA_County,date,PHA,Crime,Grad.Rate))

CorMatrix19.2 <- round(cor(CorMatrix19.2.Data[ ,colnames(CorMatrix19.2.Data) != 'log_Avg.Loan.Amount'],  
                       CorMatrix19.2.Data$log_Avg.Loan.Amount, use='pairwise.complete.obs'),
                 digits = 2L)
CorMatrix19.2


ggplot(df19, aes(x=log_LoansPerCapita, 
                 y=log_Avg.Loan.Amount, 
                 shape = date, color = date)) + 
  geom_point() +
  geom_smooth(method=lm, se = FALSE) +
  scale_color_manual(values=c('navajowhite3','burlywood4'))+
  guides(color = guide_legend('Year'),
         shape = guide_legend('Year')) +
  labs(title = "Strongest Correlation Scatterplot",
       y = 'log(Loan Amount)',
       x = 'log(Loans Per Capita)')+
  theme_classic()

#### 20 #### -----------------------------------------------------------------------------

df19$Grad.Rate <- as.factor(df19$Grad.Rate)

ggplot(df19, aes(y=log_ECSB660, 
                 x=Grad.Rate, 
                 fill=Grad.Rate)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("navajowhite2", "lemonchiffon3", "burlywood4")) +
  guides(fill = guide_legend('Graduation Rate')) +
  labs(title = 'Box Plot of Subprime Credit Score by Graduation Rate',
       x= 'Graduation Rate',
       y= 'log(Equifax Credit Score Below 660)') +
  geom_jitter(shape=16, position=position_jitter(0.08))



