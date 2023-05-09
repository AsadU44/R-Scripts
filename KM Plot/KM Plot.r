library(survival)
library(survminer)
library(dplyr)
library(tidyverse)

setwd('E:/R-Programming-Practices/Data Visualization/KM Plot')
data<- data.frame(read.csv('PSMC2.csv'))

#Lets get the Survival period in the Month format
data$Time<- data$Days/30

#Let's make two groups based on average expression
x=median(unlist(data$EXPRESSION))
data$Group<- ifelse(data$EXPRESSION >=x, "2", "1")

#Attach data
attach(data)

#Fit model and prepare basic KM plot
kmcurve<- survfit(Surv(Time, Event)~Group, data=data)
ggsurvplot(kmcurve, data=data)
ggsurvplot(kmcurve, data=data, censor.shape="|", censor.size = 4)


#Customize plot
ggsurvplot(
  kmcurve,
  data = data,
  size = 1.5,  xlim=c(0, 60),break.x.by = 12,xlab="Time (Months)",
  censor.shape="|", censor.size = 3, # change line size
  palette =
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value which will perform log-rank T test
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs =
    c("Low Expression", "High Expression"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_light()      # Change ggplot2 theme
)



