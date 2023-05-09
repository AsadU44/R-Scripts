install.packages("waterfalls")
library(waterfalls)
library(ggplot2)
library(dplyr)



#Creating diverging barplot
dat<- Data
color <- ifelse(dat$logFC < 0, "#66BBBB", "#DD4444")


plot<-  ggplot(dat, aes(x= reorder(Gene, logFC), y=logFC))+
               geom_bar(stat = 'identity',show.legend = FALSE,
                        fill = color,     
                     color = "white") #background


plot<- plot+ xlab("Gene") + ylab("logFc") +
          scale_y_continuous(breaks= seq(-6, 6, by = 2),
            limits = c(min(dat$logFC) - 0.6,
            max(dat$logFC) + 0.6)) +coord_flip() +
            theme_minimal() + 
            theme(panel.grid.major.y = element_blank()) # Remove horizontal grid



#Creating waterfall plot

dat2<- select(Data, Gene, Partners)

waterfall(dat2)