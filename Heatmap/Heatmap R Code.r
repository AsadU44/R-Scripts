library(ComplexHeatmap)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(circlize)
library(magick)

setwd('E:/R-Programming-Practices/Data Visualization/Heatmap')


## Heatmap from gene expression data (like output from DESeq2)
#Read count data
norm.counts<- read.csv('Normalized read counts.csv', row.names = 1)
z.data<- t(apply(norm.counts, 1, scale)) #Convert to z scores
colnames(z.data)<-rep(c('Healthy', 'COVID-19'), c(9,36)) #Change column names
z.data<- data.frame(z.data)

## Read DEGs matrix and stratify
sig.genes<- read.csv('Significant_DEGs.csv', row.names = 1)
sig.genes<- sig.genes[order(sig.genes$padj),]
sig.genes_final<- filter(sig.genes, sig.genes$padj<0.001 & abs(sig.genes$log2FoldChange)>=5)
data.final<- z.data[row.names(sig.genes_final),]
data.final<- na.omit(data.final)

Heatmap(data.final, cluster_rows = T, cluster_columns = T,
                  column_labels = colnames(data.final), row_labels = 
                  sig.genes[rownames(data.final),]$symbol, name = 'Z-score')

## Change color
col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
col_fun(seq(-2, 4))

Heatmap(data.final, cluster_rows = T, cluster_columns = T,
        column_labels = colnames(data.final), row_labels = 
          sig.genes[rownames(data.final),]$symbol, name = 'Z-score', col= col_fun)

## Heatmap from random data
data2<- read.csv('Voom_CPM.csv', row.names = 1)
symbol<- mapIds(org.Hs.eg.db, keys = rownames(data2), 
                keytype = 'ENSEMBL', column = 'SYMBOL')
symbol<- as.data.frame(symbol)

data2.z<- t(apply(data2,1,scale))
colnames(data2.z)<- colnames(data2)
data2.z<- data2.z[1:40,]

#Generate heatmap
Heatmap(data2.z, row_labels = symbol[rownames(data2.z),], name = 'Z-score')

#Split heatmap based on clustering
Heatmap(data2.z, row_labels = symbol[rownames(data2.z),], name = 'Z-score',
        column_km = 2)

#Reorder row and column names (Dendogram will not work)
Heatmap(data2.z, row_labels = symbol[rownames(data2.z),], name = 'Z-score',
   row_order = sort(rownames(data2.z)), column_order = sort(colnames(data2.z)),
              column_title = "DEGs in CML Patients vs Healthy Individuals")

# Heatmap from one/two column data
data3<- read.csv('Expression Atlas.csv')

plot<- ggplot(data3, aes(x=Genes, y= Disease, fill=log2FC))+geom_tile()+
        scale_fill_gradient2(high = 'red', mid='white', low = 'blue') + 
        theme(axis.text.x = element_text(angle = 90, size = 8, face = 
        'bold', hjust = 1), axis.text.y = element_text(size = 8, face = 
        'bold'), panel.background = element_rect(colour = 'black'),
         panel.grid.major = element_line(color = 'white')) 
plot


