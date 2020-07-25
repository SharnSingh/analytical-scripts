
# Linear and Logistic Regression
# This script was written to produce multiple linear and logisitic regression models based on 
# cost analysis hospital data. It also produces some descriptive statistics. Written by Sharn Singh.

options(scipen=999)

##----------------------------
##       Load Packages       -
##----------------------------
library(readxl)
library(data.table)
library (psych)
library(gmodels)


##----------------------------
##        Import Data        -
##----------------------------
data <- read_excel("~/Copy_data.xlsx")
View(data)


##----------------------------
##     Data Manipulation     -
##----------------------------
setnames(data, 'Avg Dir Cost', 'avg_cost')
setnames(data, 'TOTAL STONE BURDEN',"stone_burden")
setnames(data, 'Age of operation', 'age')
setnames(data, 'Indication for surgery (1=stone, 2=tumor, 3=obstruction, 4=other)', 'indic_surg')
setnames(data, 'surgeon type', 'surg_type')
setnames(data, 'Total Supply Cost Avg', 'equip_cost')
setnames(data, 'op time (minutes)', 'op_timem')
setnames(data, 'Sex (Male=1, Female=2)', 'sex')
setnames(data, 'Change in scrub tech', 'change_scrub')
setnames(data, 'Change in circulator', 'change_circ')
setnames(data, 'BMI', 'bmi')

data$indic_surg <- ifelse(data$indic_surg>1,1,0)
data$logcost <- log(data$avg_cost)


##----------------------------
##       Data Analysis       -
##----------------------------

# Continous Analysis
# Patient char
linear_patients <- lm((logcost) ~ age + stone_burden + factor(indic_surg) +  bmi , data=data)
summary(linear_patients)
AIC(linear_patients)
exp(cbind(OR = coef(linear_patients), confint(linear_patients)))


# Hospital cahr
linear_hospital <- lm(logcost ~ factor(surg_type) + equip_cost+ change_scrub , data=data)
summary(linear_hospital)
AIC(linear_hospital)

####Univariate operation time model####
linear_hospital <- lm(logcost ~ op_timem + equip_cost+ change_scrub , data=data)
summary(linear_hospital)

####Univariate circulator model#####
linear_hospital <- lm(logcost ~ change_circ + equip_cost+ change_scrub , data=data)
summary(linear_hospital)

# Logistic Analysis
median(data$avg_cost)
data$avg_cost
data$cont_cost <- ifelse(data$avg_cost >= 2953.286, 1,0)
data$cont_cost

# Patient char
logistic_patients <- glm(cont_cost ~ age + stone_burden + factor(indic_surg) + bmi, data=data, family="binomial")
summary(logistic_patients)
AIC(logistic_patients)
exp(cbind(OR = coef(logistic_patients), confint.default(logistic_patients)))

# Hospital char
logistic_hospital <- glm(cont_cost ~ factor(surg_type) + equip_cost + change_scrub, data=data, family="binomial")
summary(logistic_hospital)
AIC(logistic_hospital)

logistic_hospital <- glm(cont_cost ~ op_timem + equip_cost + change_scrub, data=data, family="binomial")
summary(logistic_hospital)

logistic_hospital <- glm(cont_cost ~ change_circ + equip_cost + change_scrub, data=data, family="binomial")
summary(logistic_hospital)



#######Descriptive Statistics######
describe (data$age)
describe (data$bmi)
mean (data$sex=1)
table(data$sex)
mean(data$sex == 2)
sd(data$sex == 1)
aggregate(data$sex, by=data$sex, FUN=mean)

mean(subset(data, data$sex == 2))
summary(data$sex)
CrossTable(data$sex, format = "SAS")
CrossTable(data$indic_surg, format = "SAS")
CrossTable(data$change_scrub, format = "SAS")
CrossTable(data$change_circ, format = "SAS")
CrossTable(data$surg_type, format = "SAS")
CrossTable(data$stone_burden, data$indic_surg)
