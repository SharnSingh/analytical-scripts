
# Foresplot Script
# This script was written to output a forestplot based on cardiology data. 
# Written by Sharn Singh

options(scipen=999)


##----------------------------
##       Load Packages       -
##----------------------------
#install.packages("forestplot")

library(tidyverse)
library(readxl)
library(forestplot)


##----------------------------
##        Import Data        -
##----------------------------
mortdat <- read_excel("~/Outcomes_data.xlsx", 
           sheet = "Mortality")

mort50 <- subset(mortdat, mortdat$EF_Level == ">=50")

gfdat <- read_excel("~/Outcomes_data.xlsx", 
                      sheet = "Graft Failure")


##----------------------------
##       Data Analysis       -
##----------------------------

meta <- structure(list(
  HR = c( 1.006, 1.014, 1.164, 1.109, 1.1, 0.999, 0.935, 1.011),
  lower = c(1.003, 1.011, 1.125, 0.997, 0.999, 0.994, 0.857, 1.003),
  upper = c(1.009, 1.017, 1.204, 1.233, 1.211, 1.005, 1.019, 1.019),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -11L),
  class = "data.frame"
  )
)

tabletext<-cbind(
  c("Variable", "Recepeint Age", "Donor Age", 
    "Ichemia Time", "Ethinicity: Black", "Ethinicity: Other ", "Ejection Fraction", 
    "Female Gender", "BMI"),
  c("HR", "1.006", "1.014", "1.164", 
    "1.109", "1.1", "0.999", "0.935", 
    "1.011"),
  c("LCI", "1.003", "1.011", "1.125", 
    "0.997", "0.999", "0.994", "0.857", 
    "1.003"),
  c("UCI", "1.009", "1.017", "1.204", 
    "1.233", "1.211", "1.005", "1.019", 
    "1.019"),
  c("P-Value", "0.0001", "<0.0001", "<0.0001", 
    "0.0565", "0.052", "0.7386", "0.1275", 
    "0.0053"))

forestplot(labeltext = tabletext, 
           meta,new_page = TRUE,
           #is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,2.5), 
           xlog=TRUE, 
           #col=fpColors(box="royalblue",line="darkblue", summary="royalblue")
           )

forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,mort50$HR), 
           lower=c(NA,mort50$LCI), upper=c(NA,mort50$UCI),
           title="Hazard Ratio",
           # xlab="     <---PCI Better---    ---Medical Therapy Better--->",
          # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                          # "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           #"15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                          # "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           #"31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1),
                          ticks=gpar(cex=1),
                          xlab=gpar(cex = 1.25),
                          title=gpar(cex = 2)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.1, colgap=unit(9,"mm"),
           lwd.ci=1, ci.vertices=TRUE, ci.vertices.height = 0.1)


