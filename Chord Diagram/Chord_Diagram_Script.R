library(circlize)
library(viridis)
library(RColorBrewer)
library(ggplot2)
library(colorspace)



setwd('D:/CancerData/IPF/Step 2')
data<- read.csv('Protein Class.csv')

data<- data[1:67,]
data$Plasma<- ifelse(data$Plasma=='yes', 1, 0)
rownames(data)<- data[,1]
data<- as.matrix(data[,-1])





#Adjust the text size
par(cex = 0.7, mar = c(0, 0, 0, 0))

#Setting grid color
grid.col <- setNames(viridis(length(unlist(dimnames(data)))), 
              union(rownames(data), colnames(data)))
jpeg('Protein_class.jpg', width = 24, height = 24, units = 'cm', res=600)
chordDiagram(data, annotationTrack = 'grid', preAllocateTracks = 1, 
           grid.col = grid.col, transparency = 0)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  #circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

dev.off()

