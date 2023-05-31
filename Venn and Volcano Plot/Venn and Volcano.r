library(ggvenn)
library(ggplot2)
library(org.Hs.eg.db)
library(AnnotationDbi)
library(ggrepel)
library(colorspace)

COVID<- read.csv('COVID-19.csv')
AML<- read.csv('AML.csv')
CML<- read.csv('CML.csv')
CLL<- read.csv('CLL.csv')

venn<- list(
           COVID= COVID$symbol,
           AML= AML$Gene.symbol,
           CML= CML$symbol,
           CLL= CLL$symbol
           )

#Basic venn diagram
ggvenn(venn)

#Subset to 3 column
ggvenn(venn, c("COVID", 'AML', 'CLL'))

#Customize
ggvenn(venn, 
       fill_color = c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA"), #fill color
       fill_alpha = 0.5, #Fill transparency
       stroke_color = 'black',
       stroke_size = 0.5,
       text_color = 'black',
       text_size = 6,
       set_name_color = c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA"),
       set_name_size = 7,
       show_percentage = F
       )


#Manual venn diagram
DEG<- read.csv('DEGs.csv')

#Prepare data
colnames(DEG)[3]<- 'log2FC'
DEG$Label<- ifelse(DEG$log2FC>=1,'Upregulated', 
                   ifelse(DEG$log2FC<=-1, 'Down regulated','Not Significant'))
DEG$Genes<- mapIds(org.Hs.eg.db, keys = DEG$X, keytype = 'ENSEMBL', column = 'SYMBOL')
DEG<- DEG[order(DEG$pvalue),]


# Prepare plot
vol<-ggplot(data = DEG, aes(x = log2FC, y = -log10(pvalue), col = Label, label = Label)) +
      geom_vline(xintercept = c(-1, 1), col = "#7C878E", linetype = 'dashed') +
      geom_hline(yintercept = -log10(0.05), col = "#7C878E", linetype = 'dashed') + 
      geom_point(size = 1)+ theme_bw()+ coord_cartesian(ylim = c(0, 100), 
      xlim = c(-4, 4)) + labs(color='Regulation Pattern', x='log2 Fold Change', y='-log10(pvalue)')+
     scale_color_manual(values=c('#5C88DA', '#7C878E', '#CC0C00'))
vol

# Add gene names
vol2<-  vol+ geom_text_repel(data = head(DEG, 20),
       aes(label=Genes))
vol2

#Alternative method
cols <- densCols(DEG$log2FC, -log10(DEG$pvalue))
plot(DEG$log2FC, -log10(DEG$pvalue), col=cols, panel.first=grid(),
     main="Volcano plot", xlab="log2 Fold Change", ylab="-log10(p-value)",
     pch=20, cex=0.6)
abline(v=0)
abline(v=c(-1,1), col="brown")
abline(h=-log10(0.05), col="brown")
gn.selected <- abs(DEG$log2FC) > 2.5 & DEG$pvalue< 0.05
text(DEG$log2FC[gn.selected],
     -log10(DEG$pvalue)[gn.selected],
     lab=DEG$Genes[gn.selected ], cex=0.6)



  