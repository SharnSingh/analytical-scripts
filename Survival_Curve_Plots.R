
# Survival Curve Plots
# This script was written to produce survival curve plots for publication. 
# Written by Sharn Singh

options(scipen=999)

##----------------------------
##       Load Packages       -
##----------------------------
library(readxl)
library(readr)
library(tidyverse)
library(data.table)
library(xlsx)
library(Hmisc)
library(irr)
library(survival)
library(survminer)


##----------------------------
##        Import Data        -
##----------------------------

updated_dat <- read_excel("~/updated_data.xlsx")



##----------------------------
##     Data Manipulation     -
##----------------------------
updated_dat <- subset(updated_dat, select = c(PATIENT_ID, AGE, SEX, RACEGROUP, AYARECODE, SURV_MON_ACT, VITSTAT, DRTYPE))
summary(updated_dat)
updated_dat$death <- ifelse (updated_dat$VITSTAT == 0, 1, 0)

CNS_dat <- subset(updated_dat, 7<= AYARECODE & AYARECODE<=16 | AYARECODE == 27)
CNS_dat <- subset(CNS_dat, DRTYPE==1 | DRTYPE==0 )
CNS_dat$death <- ifelse (CNS_dat$VITSTAT == 0, 1, 0)




##----------------------------
##       Data Analysis       -
##----------------------------

SurvObj <- Surv(time = updated_dat$SURV_MON_ACT, event = updated_dat$surv )
fit1 <- survfit(SurvObj ~ 1, data=updated_dat)
ggsurvplot(fit1, data=updated_dat)


### Overall ### 
fit=survfit(Surv(SURV_MON_ACT, death) ~ 1, data = updated_dat)
p <- ggsurvplot(fit, data = updated_dat,  xlab="Time (Months)",ylab=" Survival Probability",
  legend = 'none',
  title = 'Overall Survival',
  xlim = c(0,170)

 # conf.int = TRUE,
 # Add risk table
 # risk.table = TRUE,
 # tables.height = 0.2,
 # tables.theme = theme_cleantable(),

 palette = c("#2E9FDF")
 #ggtheme = theme_bw() # Change ggplot2 theme
)

p$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold"))


### CNS ### 
fit=survfit(Surv(SURV_MON_ACT, death) ~ CNS_dat$DRTYPE, data = CNS_dat)
p1 <- ggsurvplot(fit, data = CNS_dat,  xlab="Time (Months)",ylab=" Survival Probability",
  legend.title = "Doctor Type",
  legend.labs = c("Adult", "Peds"),
  title = 'CNS Malginancy Survival by doctor type',
   # pval=TRUE,

 # conf.int = TRUE,
 # Add risk table
 # risk.table = TRUE,
 # tables.height = 0.2,
 # tables.theme = theme_cleantable(),

 # palette = c("#2E9FDF"),
 #ggtheme = theme_bw() # Change ggplot2 theme
 legend = c(0.2, 0.2),
 xlim = c(0, 170)
)
p1$plot


