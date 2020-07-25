
# Survival Analysis 
# This script was written to produce survival analysis models across multiple subsets of data.
# Also includes bootstrapping and multiple survival plots. Written by Sharn Singh

options(scipen=999)


##----------------------------
##       Load Packages       -
##----------------------------
library(readr)
library(tidyverse)
library(data.table)
library(survival)
library(survminer)
library(Hmisc)
library(boot)


##----------------------------
##        Import Data        -
##----------------------------
surv_dat <- read_csv("~/Survival_and_Recurrence_Data.csv")
summary(surv_dat)

tx_dat <- read_csv("~/PreTx_data.csv")
summary(tx_dat)



##----------------------------
##     Data Manipulation     -
##----------------------------
tot_dat <- merge(x=tx_dat, y=surv_dat, by = "Count", all= TRUE)
tot_dat$HEP_B <- ifelse(is.na(tot_dat$HEP_B),0,1)
tot_dat$HEP_C <- ifelse(is.na(tot_dat$HEP_C),0,1)
tot_dat$ALD <- ifelse(is.na(tot_dat$ALD),0,1)
tot_dat$NASH <- ifelse(is.na(tot_dat$NASH),0,1)
tot_dat$Cirrhosis <- ifelse(is.na(tot_dat$Cirrhosis),0,1)
tot_dat$Milan_criteria <- ifelse(is.na(tot_dat$Milan_criteria),0,1)
tot_dat$SF_criteria <- ifelse(is.na(tot_dat$SF_criteria),0,1)
tot_dat$Death <- ifelse(tot_dat$Survival=="D",1,0)
tot_dat$PFSD <- ifelse(tot_dat$PFS=="N",1,0)
tot_dat$LCD <- ifelse(tot_dat$Liver_Control=="N",1,0)
tot_dat$LTCD <- ifelse(tot_dat$Local_Tumor_Control=="N",1,0)
tot_dat$SD <- ifelse(tot_dat$Systemic=="N",1,0)
tot_dat$COS <- ifelse(tot_dat$Liver_Transplant=="Y" ,tot_dat$Transplant_Mon_After_TX,tot_dat$Survival_Mon)
tot_dat$COS <- as.numeric(as.character(tot_dat$COS))


write.table(tot_dat, file="~/Pretx_update_Data.csv",sep=",",row.names=F)

ttest <- function(num){
  temp <- t.test(num ~ Treat , data=tot_dat)
  return(temp)
}

ttest <-  function(num) {
  temp
}
chsqtst <- function(num) {
  temp <- chisq.test(tot_dat$num, tot_dat$Treat, is.na = TRUE)
  return(temp)
}


##----------------------------
##       Data Analysis       -
##----------------------------

# Exploratory DA 
chsqtst(tot_dat$HEP_B)
chisq.test(tot_dat$HEP_B, tot_dat$Treat)
ttest(tot_dat$CPT_Score)


# Overall survival
summary(coxph(Surv(tot_dat$Survival_Mon, tot_dat$Death) ~   tot_dat$Tumor_1_Size + tot_dat$Treat +
                tot_dat$Tumor_Biopsy  + tot_dat$ALD + factor(tot_dat$No_Tumors), tot_dat))

fit=survfit(Surv(tot_dat$Survival_Mon, tot_dat$Death) ~ tot_dat$Treat, data = tot_dat)
print(fit)
summary(fit)
o <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Overall Survival",
           legend.title = "",
           legend='none',
           title = 'Kaplan Meier Curve for Overall Survival',
           # conf.int = TRUE,
           # Add risk table
           # risk.table = TRUE,
           # tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           
          # palette = c("#2E9FDF")
           #ggtheme = theme_bw() # Change ggplot2 theme
)

o$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
#+ 
 # scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))


# Progression Free Survival
print(coxph(Surv(tot_dat$PFS_Mon, tot_dat$PFSD) ~  tot_dat$Tumor_1_Size + tot_dat$Treat +
                tot_dat$Tumor_Biopsy  + tot_dat$ALD + factor(tot_dat$No_Tumors), tot_dat))

fit=survfit(Surv(tot_dat$PFS_Mon, tot_dat$PFSD) ~ tot_dat$Treat, data = tot_dat)
print(fit)
summary(fit)
quantile(fit, c(.025,.975), na.rm=TRUE)

# Subset data for the progression free survival 
pfsdat <- data.frame(col1=tot_dat$PFSD, col2=tot_dat$PFS_Mon)

# Function for calculating survival based off of dual column data 
surv1 <- function (data, indices) {
  dt <- data[indices,]
  temp <- survfit(Surv(data[,2], data[,1]) ~ 1)
  return(temp$median) # Returns the median survival time based on the survival analysis
}

# Bootstrapping the subset data based on the surv1 function 
set.seed(1234)
bootobject <- boot(pfsdat, surv1, R=500)
head(bootobject)

pf <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Progression Free Survival",
           legend.title = "",
           legend = 'top',
           title = 'Kaplan Meier Curve for Progression Free Survival',
           # conf.int = TRUE,
           # Add risk table
           # risk.table = TRUE,
           # tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           
          #palette = c("#2E9FDF")
           #ggtheme = theme_bw() # Change ggplot2 theme
)
pf$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
 scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))


# Liver Control
summary(coxph(Surv(tot_dat$Liver_Mon, tot_dat$LCD) ~   tot_dat$Tumor_1_Size +  tot_dat$Treat +
        tot_dat$Tumor_Biopsy  + tot_dat$ALD + factor(tot_dat$No_Tumors), tot_dat))

fit=survfit(Surv(tot_dat$Liver_Mon, tot_dat$LCD) ~  tot_dat$Treat, data = tot_dat)
print(fit)
summary(fit)
surv_median(fit)
L <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Liver Control Probability",
           legend.title = "",
           legend = 'top',
           title = 'Kaplan Meier Curve for Liver Control',
           # conf.int = TRUE,
           # Add risk table
           # risk.table = TRUE,
           # tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           
           # palette = c("#2E9FDF")
           #ggtheme = theme_bw() # Change ggplot2 theme
)

L$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))

# Local Tumor Control
summary(coxph(Surv(tot_dat$Local_Mon, tot_dat$LTCD) ~   tot_dat$Tumor_1_Size + tot_dat$Treat +
        tot_dat$Tumor_Biopsy  + tot_dat$ALD,  tot_dat))

fit=survfit(Surv(tot_dat$Local_Mon, tot_dat$LTCD) ~ tot_dat$Treat, data = tot_dat)
print(fit)
summary(fit)
p <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Local Tumor Control Probability",
           legend.title = "", 
           legend = 'top',
           title = 'Kaplan Meier Curve for Local Tumor Control',
           # conf.int = TRUE,
           # Add risk table
           # risk.table = TRUE,
           # tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           
           #palette = c("#2E9FDF")
           #ggtheme = theme_bw() # Change ggplot2 theme
)

p$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))

# Systemic Control
summary(coxph(Surv(tot_dat$Systemic_Mon, tot_dat$SD) ~   tot_dat$Tumor_1_Size +  tot_dat$Treat +
                tot_dat$Tumor_Biopsy  + tot_dat$ALD ,  tot_dat))

fit=survfit(Surv(tot_dat$Systemic_Mon, tot_dat$SD) ~ tot_dat$Treat, data = tot_dat)
print(fit)
summary(fit)

p <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Systemic Control Probability",
                legend.title = "", 
                legend = 'top',
                title = 'Kaplan Meier Curve for Systemic Control',
                #conf.int = TRUE,
                # Add risk table
                # risk.table = TRUE,
                # tables.height = 0.2,
                # tables.theme = theme_cleantable(),
                
                #palette = c("#2E9FDF")
                #ggtheme = theme_bw() # Change ggplot2 theme
)

p$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
  scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))

# Liver Transplant Censored Overall Survival
summary(coxph(Surv(tot_dat$COS, tot_dat$Death) ~   tot_dat$Tumor_1_Size + tot_dat$Treat +
                tot_dat$Tumor_Biopsy  + tot_dat$ALD + factor(tot_dat$No_Tumors) ,  tot_dat))

fit=survfit(Surv(tot_dat$COS, tot_dat$Death) ~ tot_dat$Treat, data = tot_dat)
print (fit)
p <- ggsurvplot(fit, data = tot_dat,  xlab="Time (Months)",ylab="Overall Survival",
                legend.title = "", 
                legend = 'none',
                title = 'Kaplan Meier Curve for Overall Survival',
                 conf.int = TRUE,
                # Add risk table
                # risk.table = TRUE,
                # tables.height = 0.2,
                # tables.theme = theme_cleantable(),
                
                palette = c("#2E9FDF")
                #ggtheme = theme_bw() # Change ggplot2 theme
)

p$plot + theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
#+ 
# scale_color_manual(labels = c("Proton", "Tace"), values = c("cyan", "gold"))
