library(survival)
library(survminer)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)

setwd('E:/R-Programming-Practices/Data Visualization/KM Plot')

data<- read.csv('Lung_surv.csv')

#Fit model and prepare basic KM plot
fit<- survfit(Surv(Time, Status)~Group, data=data) 
fit

#Plot KM plot
ggsurvplot(fit, data=data, pval = T)

#Summary
#With the summary function we can compare the survival probability of wo groups
#at any given time. The time variable in this dataset in months and we'll calculate
#the survial probabilit in one year.

summary(fit, times = 365)

#Alternatively
tbl_survfit(fit, times = 365, label_header = "**1-year survival (95% CI)**")

#Producing the median survival table for two groups
tbl_survfit(fit, probs = 0.5, label_header = "**Median survival (95% CI)**")

#Performing log-rank t test between two groups
survdiff(formula = Surv(Time, Status) ~ Group, data = data)

#Fit cox-regression model
cox<-coxph(Surv(Time, Status) ~ Group, data = data)

tbl_regression(cox, exp = TRUE) 

#Checking survival trend among multiple groups
fit2<- survfit(Surv(Time, Status)~Group+Sex, data=data) 
fit2
ggsurvplot(fit2, data = data)

#Cox model on multiple groups
cox2<-coxph(Surv(Time, Status) ~ Group+Sex, data = data)

tbl_regression(cox2, exp = TRUE)


#Customize plot
KM_plot<-ggsurvplot(
                fit,
                data = data,
                size = 1,  
                xlim=c(0, 1000),
                break.x.by = 200,
                xlab="Time (Days)",
                censor.shape="|", censor.size = 1.5, # change plot line size
                palette =c("blue", "red"),# custom color palettes
                conf.int = F,          # Add confidence interval
                pval = F,              # Add p-value which will perform log-rank T test
                risk.table = TRUE,        # Add risk table below
                risk.table.col = "strata",# Risk table color by groups
                #legend.labs = c("Low Expression", "High Expression"),    # Change legend labels
                risk.table.height = 0.25, # Useful to change when you have multiple groups
                ggtheme = theme_bw()     # Change ggplot2 theme
                )


#Add HR and p value
KM_plot$plot <- KM_plot$plot +
  ggplot2::annotate(
    "text",
    x = Inf, y = Inf,
    vjust = 1, hjust = 1,
    label = "HR = 1.16 \n p=0.43",
    size = 4
  )

KM_plot

