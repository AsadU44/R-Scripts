library(ggplot2)
library(dplyr)
library(tidyverse)
library(tibble)
library(stringr)
library(RColorBrewer)

setwd('E:/R-Programming-Practices/Data Visualization/Bubble Plot')
data<- read.csv('KEGG.csv')

## Bubble plot
Plot1<- ggplot(data)+ geom_jitter(aes(x=Count, y=Term, color=Adjusted.P.value, 
          size=Count))+scale_size_continuous(range=c(4,10)) + #theme_minimal()+ 
          scale_color_gradient(high = 'red', low = 'darkblue')+ theme(panel.border = 
           element_rect(color='black', fill=NA), axis.text.x = element_text(size 
           = 7, face = 'bold'), axis.text.y = element_text(size=7, face = 'bold'),
           panel.background = element_rect(fill = 'white'), panel.grid.minor = 
          element_line(color='grey'), panel.grid.major = element_line(color='grey'))
          
Plot1
## Use facet 
Plot1<- Plot1+facet_grid(rows = 'Class',scales = 'free',   space = 'free')
Plot1

## Barplot
Plot2<- ggplot(data, aes(x=Count, y=Term, fill=Adjusted.P.value))+
       geom_bar(stat = 'identity')+#+theme_minimal()
      scale_fill_gradient(high = 
      'red', low = 'limegreen')+theme(panel.border =element_rect(color='black', 
      fill=NA),axis.text.x = element_text(size= 7, face = 'bold'),axis.text.y 
       = element_text(size=7, face = 'bold'), panel.background = element_rect(fill = 
      'white'), panel.grid.minor = element_line(color='grey'), panel.grid.major = 
        element_line(color='grey')) 
Plot2
## Use facet 
Plot2<- Plot2+facet_grid(rows = 'Class',scales = 'free',   space = 'free')
Plot2

## Use of facet wrap
data2<- read.csv('normalized_expression.csv')
data2<- data2[1:6,]
rownames(data2)<-data2[,1] 
data2<-data2[,-1]
data2<- as.data.frame(t(data2)) 
data2<-rownames_to_column(data2)
colnames(data2)[1]<- 'Sample'
data2$Sample<- gsub('*\\.[0-9]','', as.character(data2$Sample))
data2<- data2 %>% pivot_longer(!Sample,names_to = "Gene", values_to='Expression')

Plot3<-ggplot(data2, aes(x=Sample,y=Expression, fill=Sample)) + 
      geom_boxplot()+ facet_wrap(vars(Gene), ncol = 3)+ scale_fill_discrete(breaks
      =c('Healthy','Cancer'))+scale_x_discrete(limits=c('Healthy','Cancer'))+
      theme(axis.text.x =element_text(size= 7, face = 'bold'),axis.text.y  = 
      element_text(size=7,face = 'bold'), strip.text=element_text(size=10, 
      face='bold'),legend.text = element_text(size=7, face = 'bold'))+
      scale_fill_manual(values=c("#377EB8", '#E41A1C')) 
      
Plot3

## Divergent plot
data3<- read.csv('Metascape_data.csv')
colnames(data3)[3]<- 'Variable'

Plot4<-ggplot(data3, aes(x=Value, y=Description, fill=Variable))+geom_bar(stat='identity', 
       width=0.5)+ coord_fixed(ratio = 1.5)+theme_bw()+ theme(axis.text.x 
      =element_text(size= 7, face = 'bold'),axis.text.y  =element_text(size=7,
      face = 'bold'),legend.text=element_text(size=7, face = 'bold'))+
      scale_fill_manual(values=c("#8DD3C7", '#081D58')) 
Plot4
   



 
                              
