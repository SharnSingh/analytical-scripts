
# MetaRegression Models
# This script was written to produce multiple meta-regression models inlcuding forest plots.
# Wirtten by Sharn Singh

options(scipen = 999)

##----------------------------
##       Load Packages       -
##----------------------------
library(metafor)
library(RCurl)
library(bitops)
library(Formula)
library(readxl)


##----------------------------
##        Import Data        -
##----------------------------
AAL <- read_excel("~/Adjustable ACL Loops Cleaned Copy3.xlsx")


##----------------------------
##     Data Manipulation     -
##----------------------------

# Recategorize Loading
Loading <- ifelse (AAL$`Loading Cycles`>=2000, 1, 0)
AAL$Loading <- as.factor(Loading)

# Animal subset 
Animal <- subset(AAL, AAL$`Animal or medical` > 0)
Animal

# Medical subset
Medical <- subset(AAL, AAL$`Animal or medical` < 1)
Medical

# Analysis 1: IDC Unknotted
Analysis1 <- Animal[,c(1:3, 5:8, 35)]

# Analysis 2: IDC knotted
Analysis2 <- Animal[,c(1:3, 5:6, 9:10, 35)]

# Analysis 3: TDC Unknotted
Analysis3 <- Animal[,c(1:3, 15:18, 35)]

# Analysis 4: TDC knotted
Analysis4 <- Animal[,c(1:3, 15:16, 19:20, 35)]

# Analysis 5: FECL Unknotted
Analysis5 <- Animal[,c(1:3, 25:28, 35)]

# Analysis 6: FECL knotted
Analysis6 <- Animal[,c(1:3, 25:26, 29:30, 35)]

# Analysis 7: IDC Unknotted 
Analysis7 <- Medical[,c(1:3, 5:8, 35)]

# Analysis 8: IDC knotted
Analysis8 <- Medical[,c(1:3, 5:6, 9:10, 35)]

# Analysis 9: TDC Unknotted
Analysis9 <- Medical[,c(1:3, 15:18, 35)]

# Analysis 10: TDC knotted
Analysis10 <- Medical[,c(1:3, 15:16, 19:20, 35)]

# Analysis 11: FECL Unknotted
Analysis11 <- Medical[,c(1:3, 25:28, 35)]

# Analysis 12: FECL knotted
Analysis12 <- Medical[,c(1:3, 25:26, 29:30, 35)]



##----------------------------
##       Data Analysis       -
##----------------------------

# Analysis 1
#Calc effect estimate
Analysis1 <- escalc(n1i = Number, n2i = Number, m1i = IDCM, m2i = IDEUM, sd1i = IDCSD, sd2i = IDEUSD, data = Analysis1, measure = "SMD", append = TRUE)
Analysis1

#Calculate the random-effects model
Analysis1_model <- rma(yi, vi, data=Analysis1)
summary(Analysis1_model)
funnel(Analysis1_model)
forest(Analysis1_model)

# Analysis 2
#Calc effect estimate
Analysis2 <- escalc(n1i = Number, n2i = Number, m1i = IDCM, m2i = IDEKM, sd1i = IDCSD, sd2i = IDEKSD, data = Analysis2, measure = "SMD", append = TRUE)
Analysis2

#Calculate the random-effects model
Analysis2_model <- rma(yi, vi,  data=Analysis2)
summary(Analysis2_model)
funnel(Analysis2_model)
forest(Analysis2_model)

# Analysis 3
#Calc effect estimate
Analysis3 <- escalc(n1i = Number, n2i = Number, m1i = TDCM, m2i = TDEUM, sd1i = TDCSD, sd2i = TDEUSD, data = Analysis3, measure = "SMD", append = TRUE)
Analysis3

#Calculate the random-effects model
Analysis3_model <- rma(yi, vi,  data=Analysis3)
summary(Analysis3_model)
funnel(Analysis3_model)
forest(Analysis3_model)


# Analysis 4
#Calc effect estimate
Analysis4 <- escalc(n1i = Number, n2i = Number, m1i = TDCM, m2i = TDEKM, sd1i = TDCSD, sd2i = TDEKSD,  data = Analysis4, measure = "SMD", append = TRUE)
Analysis4

#Calculate the random-effects model
Analysis4_model <- rma(yi, vi, data=Analysis4)
summary(Analysis4_model)
funnel(Analysis4_model)
forest(Analysis4_model)


# Analysis 5
#Calc effect estimate
Analysis5 <- escalc(n1i = Number, n2i = Number, m1i = FECLM, m2i = FLTUM, sd1i = FECLSD, sd2i = FLTUSD, data = Analysis5, measure = "SMD", append = TRUE)
Analysis5

#Calculate the random-effects model
Analysis5_model <- rma(yi, vi, data=Analysis5)
summary(Analysis5_model)
funnel(Analysis5_model)
forest(Analysis5_model)

# Analysis 6
#Calc effect estimate
Analysis6 <- escalc(n1i = Number, n2i = Number, m1i = FECLM, m2i = FLTKM, sd1i = FECLSD, sd2i = FLTKSD, data = Analysis6, measure = "SMD", append = TRUE)
Analysis6

#Calculate the random-effects model
Analysis6_model <- rma(yi, vi,  data=Analysis6)
summary(Analysis6_model)
funnel(Analysis6_model)
forest(Analysis6_model)

# Analysis 7
#Calc effect estimate
Analysis7 <- escalc(n1i = Number, n2i = Number, m1i = IDCM, m2i = IDEUM, sd1i = IDCSD, sd2i = IDEUSD, data = Analysis7, measure = "SMD", append = TRUE)
Analysis7

#Calculate the random-effects model
Analysis7_model <- rma(yi, vi, data=Analysis7)
summary(Analysis7_model)
funnel(Analysis7_model)
forest(Analysis7_model)

# Analysis 8
#Calc effect estimate
Analysis8 <- escalc(n1i = Number, n2i = Number, m1i = IDCM, m2i = IDEKM, sd1i = IDCSD, sd2i = IDEKSD, data = Analysis8, measure = "SMD", append = TRUE)
Analysis8

#Calculate the random-effects model
Analysis8_model <- rma(yi, vi,  data=Analysis8)
summary(Analysis8_model)
funnel(Analysis8_model)
forest(Analysis8_model)

# Analysis 9
#Calc effect estimate
Analysis9 <- escalc(n1i = Number, n2i = Number, m1i = TDCM, m2i = TDEUM, sd1i = TDCSD, sd2i = TDEUSD, data = Analysis9, measure = "SMD", append = TRUE)
Analysis9

#Calculate the random-effects model
Analysis9_model <- rma(yi, vi, data=Analysis9)
summary(Analysis9_model)
funnel(Analysis9_model)
forest(Analysis9_model)

# Analysis 10
#Calc effect estimate
Analysis10 <- escalc(n1i = Number, n2i = Number, m1i = TDCM, m2i = TDEKM, sd1i = TDCSD, sd2i = TDEKSD, data = Analysis10, measure = "SMD", append = TRUE)
Analysis10

#Calculate the random-effects model
Analysis10_model <- rma(yi, vi, mods = ~ Loading, data=Analysis10)
summary(Analysis10_model)
funnel(Analysis10_model)
forest(Analysis10_model)

# Analysis 11
#Calc effect estimate
Analysis11 <- escalc(n1i = Number, n2i = Number, m1i = FECLM, m2i = FLTUM, sd1i = FECLSD, sd2i = FLTUSD, data = Analysis11, measure = "SMD", append = TRUE)
Analysis11

#Calculate the random-effects model
Analysis11_model <- rma(yi, vi, data=Analysis11)
summary(Analysis11_model)
funnel(Analysis11_model)
forest(Analysis11_model)

# Analysis 12
#Calc effect estimate
Analysis12 <- escalc(n1i = Number, n2i = Number, m1i = FECLM, m2i = FLTKM, sd1i = FECLSD, sd2i = FLTKSD, data = Analysis12, measure = "SMD", append = TRUE)
Analysis12

#Calculate the random-effects model
Analysis12_model <- rma(yi, vi, data=Analysis12)
summary(Analysis12_model)
funnel(Analysis12_model)
forest(Analysis12_model)

