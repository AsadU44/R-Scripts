library(dplyr)
library(tidyverse)
library(g3viz)

setwd('E:/R-Programming-Practices/Data Visualization/Mutation Plot')

###We need to keep the CSV file in the extdata folder of the package
mutation.csv <- system.file("extdata", "snv_sample.csv", package = "g3viz")

#Prepare maf object
mut.dat <- readMAF(mutation.csv,
                        gene.symbol.col = "Hugo_Symbol",
                        variant.class.col = "Variant_Classification",
                        protein.change.col = "amino_acid_change",
                        sep = ",")  # column-separator of csv file

#Creating plotting options
chart.options <- g3Lollipop.theme(theme.name = "cbioportal",
                                  title.text = "BRCA1 Gene Mutation Summary")
g3Lollipop(mut.dat,
           gene.symbol = "BRCA1",
           protein.change.col = "amino_acid_change",
           btn.style = "blue", # blue-style chart download buttons
           plot.options = chart.options,
           output.filename = "customized_plot")