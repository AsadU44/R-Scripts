library(pROC)



setwd('E:/R-Programming-Practices/Data Visualization/ROC Curve')

data<- data.frame(read.csv('FOXM1 and SOX9.csv'))

rocobj1 <- plot.roc(data$Sample, data$FOXM1, levels=c("Normal", "BRCA"), alpha=0.05, #Alpha specifies CI 
                    ci=T, print.auc=T, percent=F, grid=T, main="ROC of FOXM1 in BRCA", 
                     col= 'blue')

rocobj2 <- plot.roc(data$Sample, data$SOX9, levels=c("Normal", "BRCA"), alpha=0.05, #Alpha specifies CI 
                    ci=T, print.auc=T, percent=F, grid=T, main="ROC of SOX9 in BRCA", 
                    col= 'red')

#Check differences between two ROCs
diff<- roc.test(rocobj1, rocobj2)

#Combine two ROCs
roc1<- plot.roc(data$Sample, data$FOXM1, levels=c("Normal", "BRCA"), alpha=0.05, #Alpha specifies CI 
                 ci=T, print.auc=F, percent=T, grid=T, main="ROC of FOXM1 and SOX9 in BRCA", 
                 col= 'blue')

Combined_roc<- plot.roc(data$Sample, data$SOX9, levels=c("Normal", "BRCA"), alpha=0.05,
                   percent=roc1$percent, #Specify percent of previous ROC
                   add=T,  col= 'red')

legend("bottomright", legend=c("FOXM1", "SOX9"), col=c("blue", "red"), lwd=2)