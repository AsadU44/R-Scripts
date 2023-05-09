library(ggplot2)
library(ggpubr)
library(ggExtra)

setwd("E:/R-Programming-Practices/Data Visualization/Scatter Plot")

data<- data.frame(read.csv("FOXM1 and SOX9.csv", header = T))

#Check the ranges of the variables and select color
range(data$FOXM1)
rangerange(data$SOX9)
col=c('#063970')

#Plot a basic scatterplot
sc_plot<- ggplot(data, aes(x=FOXM1, y=SOX9))+
     geom_point(color=col, size=2)+ labs(x= "FOXM1 Normalized Expression", y="SOX9 Normalized Expression")+ 
    geom_smooth(method = lm, se=F)+stat_cor(method = "pearson", label.x = 0, label.y = 1000)+theme_minimal()+ #Can change to spearman
     theme(panel.border = element_rect(color = 'black', fill = NA, size = 1))



#Without regression line
#Set color
data2<- data
data2$Color<- ifelse(data2$Sample=="BRCA", "#063970",'#C70039')  

sc_plot2<- ggplot(data, aes(x=FOXM1, y=SOX9, shape=Sample))+
  geom_point(color=data2$Color, size=2) +theme_minimal()+ #Can change to spearman
  theme(panel.border = element_rect(color = 'black', fill = NA, size = 1))+ 
 labs(x= "FOXM1 Normalized Expression", y="SOX9 Normalized Expression")

sc_plot2

#Add marginal histogram
sc_plot3<- sc_plot2+theme(legend.position = 'bottom')
sc_plot3<- ggMarginal(sc_plot3, type = 'histogram', groupColour = TRUE, groupFill = TRUE)
sc_plot3
