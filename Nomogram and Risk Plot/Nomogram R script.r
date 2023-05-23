library(rms)
library(dplyr)
library(tidyverse)
library(survival)
library(ggforest)
library(condsurv)
library(survminer)
library(ggsurvfit)
library(ggrisk)
library(ggfore)
library(gtsummary)
library(lubridate)
library(tidycmprsk)

#Nomogram from linera regression
data<- read.csv('Lung_surv.csv')

#Tidy
lin_reg<- data
lin_reg$Expr_TP53<- ifelse(lin_reg$TP53>=median(lin_reg$TP53),'High','Low')
lin_reg$Expr_CCNE1<- ifelse(lin_reg$CCNE1>=median(lin_reg$CCNE1),'High','Low')
lin_reg<- lin_reg %>% select(!c(4,10:14))

#Prepare data
ddist<- datadist(lin_reg)
options(datadist='ddist')

#Fit linear model and
fit<- lrm(Event~., data = lin_reg)

#Prepare nomogram data
nom<- nomogram(fit, fun = function(x)1/(1+exp(-x)),
              #fun.at=c(0.001,0.01,0.05,0.1,0.2,0.3,0.4,0.95,0.99), 
              funlabel = 'Risk of Event')
#Plot nomogram
plot(nom)

#Nomogram of survival
data(package='survival')
surv_data<- cancer

ddist<- datadist(surv_data)
options(datadist='ddist')

surv_fit <- cph(Surv(time, status) ~ age + sex + ph.ecog + pat.karno +wt.loss,
         x=T, y=T, surv=T, data=surv_data, time.inc=36)

surv <- Survival(surv_fit)

surv_nom <- nomogram(surv_fit, fun=list(function(x) surv(36, x), function(x) 
            surv(60, x),function(x) surv(120, x)), lp=F, funlabel=c("3-year 
            survival", "5-year survival", "10-year survival"), maxscale=10, 
               fun.at=c(0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.6, 0.5))
plot(surv_nom)

# Forest plot
colnames(lin_reg)[9]<- 'TP53'
colnames(lin_reg)[10]<- 'CCNE1'

fit<- coxph(Surv(Time, Event)~., data = lin_reg)
tbl_regression(fit)
ggforest(fit)

#Risk Score Plot
risk_data<- data[,c(10:14,1,2)]
risk_data$Time<-risk_data$Time/365

risk_fit<- coxph(Surv(Time, Event)~., data = risk_data)

ggrisk(risk_fit, cutoff.value = 'median', color.A=c(low='#3B4992',high='#DC0000'),
       color.B=c(code.0='#3B4992',code.1='#DC0000'),
       color.C=c(low='#3B4992',median='white',high='#DC0000'))


