
# ANOVA with Interactions
# This script was written to run multiple ANOVA models including some with interactions. Correlations and plots 
# were also developed depending on the results. Written by Sharn Singh

options(scipen = 999)

##----------------------------
##       Load Packages       -
##----------------------------
library(readxl)
library(tidyverse)
library(data.table)
library(lsmeans)
library(multcompView)
library(car)
library(ggpubr)
library(cowplot)
library(tableone)


##----------------------------
##        Import Data        -
##----------------------------
data <- read_excel("~/WorkingData.xlsx") 
View(data)


##----------------------------
##     Data Manipulation     -
##----------------------------
setnames(data, 'Boat.Intensity', 'BI')
setnames (data, 'Zone', 'Z')



##----------------------------
##       Data Analysis       -
##----------------------------

# Exploratory Analysis 
plot(data$Sighting ~ data$Year + data$Sector + data$Zone, data=data)
ggplot(data, aes(data$Sector, data$Sighting, colour = factor(data$Year))) + geom_point()


########## Univaraite ANOVA ########## 

## Turtle Outcome 
# By Zone 
tur1 <- lm(data$Sighting ~ factor(data$Zone), data=data)
anova(tur1)
tur1means <-lsmeans(tur1, "Zone")
cld(tur1means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(tur1),col= "blue")
kruskal.test(data$Sighting ~ factor(data$Zone), data=data)

# By Time
hist(data$Time)
tur2 <- lm(data$Sighting ~ factor(data$Time), data=data)
anova(tur2)
tur2means <-lsmeans(tur2, "Time")
cld(tur2means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(tur2),col= "pink")
data$logt <- log(data$Time)
kruskal.test(data$Sighting ~ factor(data$Time), data=data)

# By Sector 
hist(data$Sector)
tur3 <- lm(data$Sighting ~ factor(data$Sector), data=data)
anova(tur3)
tur3means <-lsmeans(tur3, "Sector")
cld(tur3means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(tur3),col= "pink")
tur33 <- kruskal.test(data$Sighting ~ factor(data$Sector), data=data)
tur33

# By Year 
hist(data$Year)
tur4 <- lm(data$Sighting ~ factor(data$Year), data=data)
anova(tur4)
tur4means <-lsmeans(tur4, "Year")
cld(tur4means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(tur4),col= "pink")
tur44 <- kruskal.test(data$Sighting ~ factor(data$Year), data=data)
tur44


## Boat Outcome

# By Zone
boat1 <- lm(data$BI ~ factor(data$Zone), data=data)
anova(boat1)
boat1means <-lsmeans(boat1, "Zone")
cld(boat1means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(boat1),col= "pink")
kruskal.test(data$BI ~ factor(data$Zone), data=data)

# By Time
hist(data$Time)
boat2 <- lm(data$BI ~ factor(data$Time), data=data)
anova(boat2)
boat2means <-lsmeans(boat2, "Time")
cld(boat2means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(boat2),col= "pink")
kruskal.test(data$BI ~ factor(data$Time), data=data)

# By Sector 
hist(data$Sector)
boat3 <- lm(data$BI ~ factor(data$Sector), data=data)
anova(boat3)
boat3means <-lsmeans(boat3, "Sector")
cld(boat3means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(boat3),col= "pink")
kruskal.test(data$BI ~ factor(data$Sector), data=data)

# By Year 
hist(data$Year)
boat4 <- lm(data$BI ~ factor(data$Year), data=data)
anova(boat4)
boat4means <-lsmeans(boat4, "Year")
cld(boat4means, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(boat4),col= "pink")
kruskal.test(data$BI ~ factor(data$Year), data=data)



########## Two Way ANOVA ########## 
# Turtle
turtle1 <- lm(Sighting ~ relevel (factor(Year), ref="2017") + factor(Z) + factor(Time), data=data)
summary(turtle1)

turtle1MY <- lsmeans(turtle1, "Year")
turtle1MZ <- lsmeans(turtle1, "Zone")
turtle1MT <- lsmeans(turtle1, "Time")
cld(turtle1MY, alpha=0.05, Letters=letters, adjust='tukey')
cld(turtle1MZ, alpha=0.05, Letters=letters, adjust='tukey')
cld(turtle1MT, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(turtle1),col= "pink")

# Boat 
boat1 <- lm(BI ~ relevel(factor(Year),ref="2017") + factor(Z), data=data)
summary(boat1)

boat1MY <- lsmeans(boat1, "Year")
boat1MZ <- lsmeans(boat1, "Zone")
boat11MT <- lsmeans(boat1, "Time")
cld(boat1MY, alpha=0.05, Letters=letters, adjust='tukey')
cld(boat1MZ, alpha=0.05, Letters=letters, adjust='tukey')
cld(boat11MT, alpha=0.05, Letters=letters, adjust='tukey')
hist(residuals(boat1),col= "pink")



########## Two Way ANOVA w/ Interactions ########## 
# Turtle; 
turtle1 <- lm(data$Sighting ~ data$Year + data$Zone + data$Time + data$Year*data$Zone + data$Time*data$Zone + data$Time*data$Year + data$Time*data$Year*data$Zone, data=data)
summary(turtle1)
turtle1MY <- lsmeans(turtle1, 'data$Year|data$Zone', adjust='tukey')
turtle1MZ <- lsmeans(turtle1, "Zone")
turtle11MT <- lsmeans(turtle1, "Time")
cld(turtle1MY, alpha=0.05, Letters=letters, adjust='tukey')
cld(turtle1MZ, alpha=0.05, Letters=letters, adjust='tukey')
cld(turtle11MT, alpha=0.05, Letters=letters, adjust='tukey')


turtle1 <- lm(data$Sighting ~ data$Year + data$Zone + data$Time + data$Time*data$Zone + data$Time*data$Year, data=data)
summary(turtle1)

# Boat;
boat1 <- lm(data$BI~ data$Year + data$Zone + data$Time + data$Year*data$Zone + data$Time*data$Zone + data$Time*data$Year + data$Time*data$Year*data$Zone, data=data)
summary(boat1)

boat1 <- lm(data$BI~ data$Year + data$Zone + data$Time + data$Year*data$Zone, data=data)
summary(boat1)


########## Correlation Plot ##########

# Pearson Correlation
BTcorr <- cor.test(data$BI, data$Sighting, method = 's')
BTcorr

Year2016corr <- cor.test (data$BI, data$Sightings, subset=(Year=="2016"))


######Subset the data by year to run correlations###
mydata2016 <- subset (data, data$Year=="2016")

mydata2017 <- subset (data, data$Year=="2017")

mydata2018 <- subset (data, data$Year=="2018")


######Year Correlation#####

corr2016 <- cor.test(mydata2016$BI, mydata2016$Sighting, method = 's')
corr2016

corr2017 <- cor.test(mydata2017$BI, mydata2017$Sighting, method = 's')
corr2017

corr2018 <- cor.test(mydata2018$BI, mydata2018$Sighting, method = 's')
corr2018


# Correlation Scatterplot 
splot <- ggplot(data, aes(data$Sighting, data$BI, colour = factor(data$Year))) + geom_point()
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

pairs(~data$Sighting+data$BI+data$Year)

p2 <- ggplot(data, aes(data$Sighting, data$BI, colour = factor(data$Year))) +
  geom_point() + facet_wrap(~ data$Year, ncol = 2, scales = "free") +
  guides(colour = "none") +
  theme() +
  xlab("Turtle Sightings") + 
  ylab("Boat Intensity") 
p2




