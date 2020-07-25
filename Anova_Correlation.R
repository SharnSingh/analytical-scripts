
# ANOVA_correlation
# This script was for ANOVA analytics with correlation calculation. Repeated 
# measures data was used with some transformation. Written by Sharn Singh 


options(scipen = 999)

##----------------------------
##       Load Packages       -
##----------------------------
library(readxl)
library(tidyr)
library(data.table)
library(lsmeans)
library(multcompView)
library(car)
library(ggpubr)
library(cowplot)
library(nlme)


##----------------------------
##        Import Data        -
##----------------------------
data <- read_excel("~/WorkingData.xlsx", 
                          sheet = "StateDur Total Time")


##----------------------------
##     Data Manipulation     -
##----------------------------
# Wide to Long
data_long <- gather(my_data, behavior, time, Swimming:Eating, factor_key = TRUE)
data_long
table (data_long$Identity,data_long$behavior)

# Renaming
setnames(data_long, 'Boat Intensity Zone', 'BI')

# Time transformation
hist(data_long$time)
hist(log(data_long$time + 1))
data_long$stime <- sqrt(data_long$time + 1)
hist(1/(data_long$time + 1))
hist(1/sqrt(data_long$time + 1))
hist((data_long$time + 1)^2)
hist(1/(data_long$time + 1)^2)


##----------------------------
##       Data Analysis       -
##----------------------------
# Repeated Measures 

# Single model
turtle.aov <- aov(stime ~ behavior , data=data_long)
summary(turtle.aov)
summary(lme(stime ~ behavior, data=data_long, random = ~ 1 | Identity))

# Dual model
turtle1.aov <- aov(time ~ behavior + BI + Error(Identity), data=data_long)
summary(turtle1.aov)
summary(lme(stime ~ behavior + BI, data=data_long, random = ~ 1 | Identity))

# Zone Model
turtle2.aov <- aov(time ~ behavior + Zone, data=data_long)
summary(turtle2.aov)
summary(lme(stime ~ behavior + Zone, data=data_long, random = ~ 1 | Identity))

# Interaction model
turtle2.aov <- aov(time ~ behavior + Zone + behavior*Zone, data=data_long)
summary(turtle2.aov)
turtle2.aov <- aov(time ~ behavior + BI + behavior*BI, data=data_long)
summary(turtle2.aov)


# Correlation
data_long$b1<- ifelse(data_long$behavior=='Swimming', 1, 
                   ifelse(data_long$behavior=="Surfacing*Surfaced", 2,
                          ifelse(data_long$behavior=="Investigating/foraging",3,4)))

# Pearson Correlation
cor.test(my_data$BI, my_data$Swimming, method = 'pearson', use = 'complete.obs')
cor.test(my_data$BI, my_data$Eating, method = 'pearson', use = 'complete.obs')
cor.test(my_data$BI, my_data$`Surfacing*Surfaced`, method = 'pearson', use = 'complete.obs')
cor.test(my_data$BI, my_data$`Investigating/foraging`, method = 'pearson', use = 'complete.obs')

iplot <- ggplot(my_data. aes(my_data$BI, my_data$Swimming)) + geom_point()

# Plot Behaviors by Zone
splot <- ggplot(data_long, aes(data_long$behavior, data_long$Zone)) + geom_point()
splot
bplot <- splot + theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.title = element_text(colour="black", size=10, 
                                    face="bold")) +
  xlab("Turtle Sighings") + 
  ylab("Boat Intensity") +
  labs(title = "Turtle Sightings vs. Boat Intensity by Year")

bplot + guides(color=guide_legend("Year")) + theme(  plot.title =element_text( size=15, hjust=0.7), 
                                                     axis.title=element_text(size=14))

