
# Linear Regression Models with Interactions
# This script was written to produce multiple linear regression model including 
# some with interactions and model validation. Written by Sharn Singh. 

options(scipen = 999)

##----------------------------
##       Load Packages       -
##----------------------------
library(readxl)
library(readr)
library(tidyverse)
library(data.table)
library(survival)
library(jtools)
library(car)
library(lme4)


##----------------------------
##        Import Data        -
##----------------------------
data <-  read_excel("~working_data.xlsx")
head(data)
summary(data)


##----------------------------
##     Data Manipulation     -
##----------------------------
data_front <- subset(data, data$Cohort<6)
data_back <- subset(data, data$Cohort>1)
data_middle <- subset(data, data$Cohort<6&data$Cohort>1)
my_data$Aceb0 <- ifelse(my_data$Acescore>0,1,0)
my_data$Aceb1 <- ifelse(my_data$Acescore>1,1,0)
my_data$Aceb2 <- ifelse(my_data$Acescore>2,1,0)


# Linear Functions
univariate_A <- function(data) {
  uni <- lm(data$BMI ~ data$Acescore, data=data)
  return(summary(uni))
}

univariate_C <- function(data) {
  uni <- lm(data$BMI ~ data$Cohort, data=data)
  return(summary(uni))
}

univariate_G <- function(data) {
  uni <- lm(data$BMI ~ data$Gender, data=data)
  return(summary(uni))
}

interaction_2 <- function(data){
  int <- lm(data$BMI ~ factor(data$Cohort)*data$Acescore, data=data)
  return(summary(int))
}

interaction_3 <- function(data){
  int <- lm(data$BMI ~ factor(data$Cohort)*data$Acescore*factor(data$Gender), data=data)
  return(summary(int))
}

ref_test <- function(num){
  ref <- lm(my_data$BMI ~ relevel(factor(my_data$Cohort), ref = num)*my_data$Acescore, data=my_data)
  summary(ref)
}


##### Analysis #####


#### Total Data ####
# Univariate 
univariate_A(my_data)
univariate_C(my_data)
univariate_G(my_data)

# Interaction 
interaction_2(my_data)

# Three Way Interaction
interaction_3(my_data)


#### Data without 6th cohort ####
# Univariate 
univariate_A(data_front)
univariate_C(data_front)
univariate_G(data_front)

# Interaction 
interaction_2(data_front)

# Three Way Interaction
interaction_3(data_front)

#### Data without 1st cohort ####
# Univariate 
univariate_A(data_back)
univariate_C(data_back)
univariate_G(data_back)

# Interaction 
interaction_2(data_back)

# Three Way Interaction
interaction_3(data_back)


#### Data without 1st and 6th cohort ####
# Univariate 
univariate_A(data_middle)
univariate_C(data_middle)
univariate_G(data_middle)

# Interaction 
interaction_2(data_middle)

# Three Way Interaction
interaction_3(data_middle)

# Plots
p1 <- ggplot(my_data, aes(my_data$Cohort, my_data$BMI)) + geom_boxplot(aes(colour = factor(my_data$Acescore)))
p1

# Testing total data with different reference groups 
ref_test("1")
ref_test("2")
ref_test("3")
ref_test('4')
ref_test("5")
ref_test("6")

### Type 3 ###
options(contrasts = c('contr.sum','contr.poly'))
model <- lm(my_data$BMI ~ factor(my_data$Cohort)*my_data$Acescore, data=my_data)
drop1(model, .~., test = "F")

# Cohort distribution among Ace score 
summary(ace <- lm(my_data$Acescore ~ factor(my_data$Cohort), data=my_data))
summary(pos <- glm(my_data$Acescore ~ factor(my_data$Cohort), family = 'poisson', data=my_data))

# Quadratic Model
my_data$Age2 <- my_data$Age^2

summary(cont <- lm(my_data$BMI ~ my_data$Age + my_data$Age2, data=my_data))
summ(cont)

# Binary Models 
summary(ace <- lm(my_data$BMI ~ my_data$Aceb0 + my_data$Cohort, data=my_data))

summary(ace <- lm(my_data$BMI ~ my_data$Aceb1 + my_data$Age, data=my_data))

summary(ace <- lm(my_data$BMI ~ my_data$Aceb2 +my_data$Cohort + my_data$Gender, data=my_data))
