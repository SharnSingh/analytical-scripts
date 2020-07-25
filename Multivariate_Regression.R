
# Multivariate Regression 
# This script was written to produce data visualization and multiple multivariate regression models. 
# It also includes correlation calculations and data imputation. Written by Sharn Singh

options(scipen=999)


##----------------------------
##       Load Packages       -
##----------------------------
library(tidyverse)
library(readxl)
library(mice)
library(VIM)
library(data.table)
library(nlme)


##----------------------------
##        Import Data        -
##----------------------------

data <- read_excel("~/Update_data.xlsx")
summary(data)



##----------------------------
##       Data Analysis       -
##----------------------------

# Scatterplot for Quality of Life
ggplot(data=data, aes (x=data$Time, y=data$QOL, group=data$ID, color=data$ID)) +  
  geom_line() + labs(title = 'Quality of Life Score Over 18 Months' , x='Months' , y='Quality of Life Score') +
  theme_bw() +  theme(legend.position = 'none') +
  theme(plot.title = element_text(size = 20, face = 'bold',  hjust = 0.5), text= element_text(size=20)) +
  scale_x_continuous(breaks=c(0,6,12,18), labels=c("0", "6", "12",'18')) +
  coord_cartesian(xlim = c(0, 18))


# Scatterplot for Self Esteem
ggplot(data=data, aes (x=data$Time, y=data$SE, group = data$ID, color = data$ID)) + 
  geom_line() + labs(title = 'Rosenberg Self Esteem Score Over 18 Months' , x='Months' , y='Rosenberg Self Esteem Score') +
  theme_bw() +  theme(legend.position = 'none') +
  theme(plot.title = element_text(size = 20, face = 'bold',  hjust = 0.5), text= element_text(size=20)) +
  scale_x_continuous(breaks=c(0,6,12,18), labels=c("0", "6", "12",'18')) +
  coord_cartesian(xlim = c(0, 18))


# Scatterplot for Stigma for Adults
ggplot(data=data, aes (x=data$Time, y=data$AP, group = data$ID, color = data$ID)) + 
  geom_line() + labs(title = 'Austin Stigma Score Over 18 Months for Adults' , x='Months' , y='Austin Stigma Score') +
  theme_bw() +  theme(legend.position = 'none') +
  theme(plot.title = element_text(size = 20, face = 'bold',  hjust = 0.5), text= element_text(size=20)) +
  scale_x_continuous(breaks=c(0,6,12,18), labels=c("0", "6", "12",'18')) +
  coord_cartesian(xlim = c(0, 18))


# Scatterplot for Stigma for Children
ggplot(data=data, aes (x=data$Time, y=data$`AustinChild Mean`, group = data$ID, color = data$ID)) + 
  geom_line() + labs(title = 'Austin Stigma Score Over 18 Months for Children' , x='Months' , y='Austin Stigma Score') +
  theme_bw() +  theme(legend.position = 'none') +
  theme(plot.title = element_text(size = 20, face = 'bold',  hjust = 0.5), text= element_text(size=20)) +
  scale_x_continuous(breaks=c(0,6,12,18), labels=c("0", "6", "12",'18')) +
  coord_cartesian(xlim = c(0, 18))



# Correlations

corrByTime <- function(x) {
  T0 <- subset(data, data$Time == x)
  cor.test(T0$AP, T0$AC, method = 'spearman')
}
corrByTime(0)
corrByTime(6)
corrByTime(12)
corrByTime(18)
corrByTime(24)

# Subset for age greater than or equal to 6
tempdat <- subset(data, data$Age >= 6)

corrByTime2 <- function(x) {
  T0 <- subset(tempdat, tempdat$Time == x)
  cor.test(T0$AP, T0$AC, method = 'spearman')
}
corrByTime2(0)
corrByTime2(6)
corrByTime2(12)
corrByTime2(18)
corrByTime2(24)



# Checking Missing Data 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,1,pMiss)

aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), 
     cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))



##### Linear Models #####


data <- subset(data, data$Time < 24)

# Quality of Life 

# Basic Model
summary(lme(QOL ~ factor(Time) + Liverpool + Age + factor(Gender), 
  data=data, random = ~1 | ID, na.action=na.omit))

tempData2 <- mice(data,m=20, print=FALSE, seed=245435)
modelFit2 <- with(tempData2,lme(QOL ~ factor(Time) + Liverpool + Age + factor(Gender), 
                                random = ~1 | ID, na.action=na.omit, method = "ML"))
summary(pool(modelFit2))



# Self Esteem 
tempData <- mice(data, m=20)
tempData
modelFit2 <- with(tempData,lme(SE ~ Time + Liverpool + Age + factor(Gender), 
                                random = ~1 | ID,na.action=na.omit, method = "ML"))
summary(pool(modelFit2))



# Parent Stigma 
tempData <- mice(data, m=20)
tempData
modelFit2 <- with(tempData,lme(AP ~ Time + Liverpool + Age + factor(Gender), 
                               random = ~1 | ID,na.action=na.omit, method = "ML"))
summary(pool(modelFit2))


# Child Stigma 
tempData <- mice(data, m=20)
tempData
modelFit2 <- with(tempData,lme(AC ~ Time + Liverpool + Age + factor(Gender), 
                                random = ~1 | ID,na.action=na.omit, method = "ML"))
summary(pool(modelFit2))



