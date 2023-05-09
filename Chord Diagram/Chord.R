library(circlize)

data <- read.csv("D:/PSMC.csv", row.names = 1)


#convert the table to a martix
data <- as.matrix(data)

#create a chord diagram
chordDiagram(data)

#Customize color
colors <- c(Col1 = "#2596be", Col2 = "#e28743",
            Row1 = "#21130d",Row2 = "#abdbe3", Row3 = "#063970",
            Row4 = "db3a34", Row5 = "eef4ed")

#Increase gap
circos.par(gap.after = c(rep(5, nrow(data)-1), 15, rep(5, ncol(data)-1), 15))
chordDiagram(data)

#Remoove track
chordDiagram(data, grid.col = colors,  annotationTrack =  c("name", "grid"))
circos.clear() 


