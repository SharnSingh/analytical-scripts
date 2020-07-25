
# Survival Analysis Models
# This script was written to calculate Survival Analysis models and Kaplen Meier curves.
# Written by Sharn Singh

options(scipen = 999)

##----------------------------
##       Load Packages       -
##----------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(survival)
library(survminer)
library(rms)

##----------------------------
##        Import Data        -
##----------------------------
course <- read_excel("~/course.xlsx")
ct_checkup <- read_excel("~/ct_checkup.xlsx")
treated_lesion <- read_excel("~/treated_lesion.xlsx")
patients <- read_excel("~/patients.xlsx")

##----------------------------
##     Data Manipulation     -
##----------------------------

#######  Joining the tables ####### 
data1 <- treated_lesion %>% left_join(ct_checkup, by="lid")
data2 <- patients %>% left_join(course, by = "PID")
my_data <- data2 %>% left_join(data1, by = "CID")

# Turn BED10_calc into binary 
bed10 <- ifelse (my_data$BED10_calc>=100, 1, 0)
my_data$bed10 <- as.factor(bed10)

# Turn GTV Max Diameter into binary variable 
gtvMaxDiam <- ifelse (my_data$gtv_max_diameter>=3,1,0)
my_data$gtvMaxDiam <- as.factor(gtvMaxDiam)


##----------------------------
##       Data Analysis       -
##----------------------------

# Survival Time 
df<-data.frame(x1=my_data$fu_date,x2=my_data$course_end)
df[]<-lapply(df,ymd)
end2FuDate <- as.numeric(df$x1-df$x2, units="days")
end2FuDate

surv <- ifelse (my_data$control_at_time == "failure" | my_data$control_at_time == "Failure", 0, 1)
surv
my_data$surv <- as.factor(surv)

## Survival Analysis  
# Creating a survival object. control at time  = failure 
my_data$SurvObj <- with(my_data, Surv(end2FuDate, surv == 0))

# Normal
res.cox <- coxph(Surv(end2FuDate, surv == 0) ~ bed10 + gtvMaxDiam + age_at_tx, data =  my_data)
summary(res.cox)
res.zph <- cox.zph(res.cox)
res.zph
plot(res.zph)

# Clustered by patient
res.cox1 <- coxph(Surv(end2FuDate, surv == 0) ~ strata(bed10) + gtvMaxDiam + age_at_tx + cluster(PID), data =  my_data)
summary(res.cox1)
res.zph1 <- cox.zph(res.cox1)
res.zph1

plot(survfit(res.cox1), xlab="Time", ylab="Estimated Survival Function", main=expression("Kaplan Meier Graph by BED"[10]*" Dosage"), col = c("blue", "red"))
Bt1 <- expression("BED"[10]*" >= "*"100")
Bt2 <- expression("BED"[10]*" < " *"100")
legend('center', c(Bt1,Bt2), col=c('red','blue'), lty =1)

res.cox2 <- coxph(Surv(end2FuDate, surv == 0) ~ bed10 + gtvMaxDiam + age_at_tx + cluster(PID), data =  my_data)
summary(res.cox1)

plot(survfit(res.cox2), xlab="Time", ylab="Estimated Local Control", main="Kaplan Meier Graph of Local Control", conf.int = FALSE)


