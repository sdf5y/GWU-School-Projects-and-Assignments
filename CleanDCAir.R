setwd("C:/Users/18045/Documents/R/Statistic Class/Tony_R_project")
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
#install.packages('data.table')
library(data.table)

#load data------

med_income_df <- data.frame(read.csv("Wards_Incomes.csv",
                                     na.strings=c("","NA")))

t_cond_ward <- data.frame(read.csv("TreeWardCond.csv",
                                   na.strings=c("","NA")))

tree_df <- data.frame(read.csv("Urban_Forestry_Street_Trees.csv",
                               na.strings=c("","NA")))

ward_pop_df <- data.frame(read.csv("Ward_pop_2022.csv",
                               na.strings=c("","NA")))

tree_df_cond <- data.frame(tree_df[1:8], tree_df$CONDITION)

t_cond_ward['Order'] <- c(5,1,3,2,4,6,7)
t_cond_ward <- arrange(t_cond_ward, Order)

#Renaming columns, and creating dataframes of useful columns-----------

tree_df_cond <- data.frame(tree_df[1:8], tree_df$CONDITION)
colnames(med_income_df)[8] <- 'Median Income'
colnames(med_income_df)[10] <- 'Total Households'
t_cond_ward <- t_cond_ward[,-c(11, 1)]
colnames(ward_pop_df)[17] <- "Total Population"

#Tree function for each ward-------

ward_data <- function (temp_ward,j){
    temp_ward <- na.omit(tree_df_cond %>%
      filter(WARD == j) %>%
      select(tree_df.CONDITION) %>%
      count(tree_df.CONDITION))
  return(temp_ward)
}

#median income function for each ward-------

ward_income <- function (temp_income,i){
  temp_income <- na.omit(med_income_df %>%
                         filter(FID_Wards2020 == i) %>%
                         select(B19049_001))
  return(temp_income)
}

summary(ward_income(med_income_df,1))
summary(ward_income(med_income_df,7))

#each ward dfs of tree conditions------
w1 <- ward_data(tree_df_cond,1) 
w2 <- ward_data(tree_df_cond,2)
w3 <- ward_data(tree_df_cond,3)
w4 <- ward_data(tree_df_cond,4)
w5 <- ward_data(tree_df_cond,5)
w6 <- ward_data(tree_df_cond,6)
w7 <- ward_data(tree_df_cond,7)
w8 <- ward_data(tree_df_cond,8)

#Re-order each ward's tree conditions-----------------

orda <- function(tempoda){
  tempoda['Order'] <- c(5,1,3,2,4)
  tempoda <- arrange(tempoda, Order)
  tempoda <- tempoda[,-3]
  return(tempoda)
}

w1 <- orda(w1)
w2 <- orda(w2)
w3 <- orda(w3)
w4 <- orda(w4)
w5 <- orda(w5)
w6 <- orda(w6)
w7 <- orda(w7)
w8 <- orda(w8)

#Calculate percapita trees----

wardtree_df <- function(temp_ward_tree, temp_ward_medinc, i){
  temp_ward_tree <- wardtree_df[i] / med_inc_capita_df$`Ward Population`[i]
  return(temp_ward)
}
    
#Calculate percapita median income-------

wardpops_df <- function(temp_ward,j){
  temp_ward <- ward_pop_df %>%
    arrange(WARD) %>%  
    filter(WARD == j) %>%  
    select('Total Population')
  return(temp_ward)
}

#list of ward populations----
for (i in 1:8) {
  if (i == 1){
    li <- data.frame(wardpops_df(ward_pop_df,i))
    i <- i+1
  }else if (1 < i & i <= 8){
    li <- append(li, wardpops_df(ward_pop_df,i))
    i <- i+1
  }
}
    
#Calculation of percapita income----
li <- data.frame(li)
li_df <- data.frame(transpose(li))

med_inc_capita_df <- data.frame(transpose(t_cond_ward[7,-1]))
Wards_df <- data.frame(c('Ward 1','Ward 2','Ward 3','Ward 4','Ward 5','Ward 6','Ward 7','Ward 8'))
med_inc_capita_df <- data.frame(Wards_df, med_inc_capita_df, li_df, transpose(t_cond_ward[1,-1]))

colnames(med_inc_capita_df) <- c('Wards', 'Median Income', 'Ward Population', 'Excellent Tree')

med_inc_capita_df['Ward Median Income Per Capita'] <- data.frame(med_inc_capita_df$`Median Income`/ med_inc_capita_df$`Ward Population`)
med_inc_capita_df['Ward Excellent Tree Per Capita']  <- data.frame(med_inc_capita_df$`Excellent Tree` / med_inc_capita_df$`Ward Population`)

#write.csv(med_inc_capita_df, file = 'med_inc_capita.csv')

#Plotting------------

#tree conditions in wards---
Counts <- c(w1$n, w2$n, w3$n, w4$n, w5$n, w6$n, w7$n, w8$n)

Wards <- c(rep('Ward 1', 5), rep('Ward 2', 5), rep('Ward 3', 5), rep('Ward 4', 5), rep('Ward 5', 5), rep('Ward 6', 5), rep('Ward 7', 5), rep('Ward 8', 5))
Tree_Condition <- rep(c("Excellent", "Good", "Fair", "Poor", "Dead"), 8)
datat <- data.frame(Wards, Tree_Condition, Counts)

datat <- datat %>% 
  mutate(Ward_Group = as.factor(Wards))

datat %>%
  ggplot(aes(x = interaction(Wards, Counts, lex.order = TRUE), 
             y = Counts, 
             fill = Tree_Condition)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "Tree Count for Each Ward", 
       x = "Wards", 
       y = "Tree Count") +
  scale_x_discrete(breaks= c("Ward 1", "Ward 2", "Ward 3", "Ward 4", "Ward 5", "Ward 6", "Ward 7", "Ward 8"))

#percapita tree condition wards---

ggplot(med_inc_capita_df, aes(fill= Wards,
                              y=`Ward Excellent Tree Per Capita`,
                              x=Wards)) +
  geom_bar(position= 'dodge',
           stat= 'identity')+
  scale_fill_discrete(breaks=c('Ward 1','Ward 2','Ward 3','Ward 4','Ward 5','Ward 6','Ward 7','Ward 8'))

# Z-Score ----

z_scores_df <- ((med_inc_capita_df$`Ward Excellent Tree Per Capita`)- mean(med_inc_capita_df$`Ward Excellent Tree Per Capita`)) / sd(med_inc_capita_df$`Ward Excellent Tree Per Capita`) 

plot(z_scores_df, type = 'o', col='red')
barplot(z_scores_df, 
        names.arg=c(1:8),
        xlab = "Wards",
        ylab = "Z-Scores",
        col = c("red", "black", "green", "yellow", "blue", "orange", "purple", "grey"),
        main = "Ward Excellent Tree Condition Z-Scores")

#  https://www.dchealthmatters.org/demographicdata/index/view?id=2419&localeTypeId=27
#Perform Chi-Squared Test------

test <- transpose(t_cond_ward[-c(7,6),-c(1)])
test <- test[,-c(6,7)]
chisq.test(test)

sum(test$V1+test$V2+test$V3+test$V4+test$V5)
as.factor(tree_df_cond$WARD)
as.factor(tree_df_cond$tree_df.CONDITION)
chisq.test(tree_df_cond$WARD, tree_df_cond$tree_df.CONDITION)
chisq.test(med_inc_capita_df$`Ward Median Income Per Capita`, med_inc_capita_df$`Excellent Tree`)
tabll <- table(med_inc_capita_df)
