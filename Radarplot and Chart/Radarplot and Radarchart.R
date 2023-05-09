library(ggplot2)
library(fmsb)
library(tidyverse)
library(ggpubr)
library(waffle)

#Import dataset
data <- data.frame(read.csv("D:/PSMC.csv", row.names = 1))

#Set minimum and maximum range
max_min <- data.frame(
  Protein_Partners = c(140, 0), miRNA_Partners = c(140, 0), TF_Targets = c(140, 0))

# Bind the variable ranges to the data
rownames(max_min) <- c("Max", "Min")
df <- rbind(max_min, data)

#Generic Radar Chart
radarchart(df)

#Select color
col=c("#00AFBB", "#E7B800", "#FC4E07")


#Customize
radarchart( df, axistype=2, 
            
            #custom polygon
            pcol=col, pfcol=scales::alpha(col, 0.5), plwd=3, plty=1 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,75, 150), cglwd=1.5,
            
            #custom labels
            vlcex=1)


#Add legend
legend(x=0.85, y=1, legend = c("PSMC1", "PSMC2", "PSMC3"), 
       bty = "n", pch=20 , col=col , text.col = "black", 
       cex=0.9, pt.cex=1.6)



#Alternative way of representation is dotchart

#assign first row name and col data to row data
df2 <- t(data) %>%
  as.data.frame() %>%
  rownames_to_column("Partners")
df2

#Lets rename the row names
new.names<- c('Protein_Partners' = 'Protein Partners', 'miRNA_Partners' = 'miRNA Partners', 
              'TF_Targets' = 'TF Targets')

df2$Partners<- new.names[df2$Partners]

#Preparing data
df2 <- df2 %>%
  select(Partners, PSMC1, PSMC2, PSMC3) %>%
  pivot_longer(
    cols = c(PSMC1, PSMC2, PSMC3),
    names_to = "PSMCs",
    values_to = "Value"
  )
head(df2)

ggdotchart(df2, x = "Partners", y = "Value", 
           group = "PSMCs", color = "PSMCs", palette = "jco",
           add = "segment", position = position_dodge(0.1),
           sorting = "descending", facet.by = "PSMCs",
           rotate = TRUE, legend = "none", dot.size = 5)

#Waffle plots
par(mfrow = c(2, 2))
# Create Vector
x <- c(PSMC1 = 15, PSMC2 = 25, PSMC3= 20)
x2<-  c(PSMC1 = 30, PSMC2 = 12, PSMC3= 18)

# Waffle chart
W1<- waffle(x, rows = 10, 
       colors =c("#00AFBB", "#E7B800", "#FC4E07"),
       legend_pos = "bottom", title = "Protein Partners")
W2<- waffle(x2, rows = 10, 
            legend_pos = "bottom", title = "miRNA Partners")



q()