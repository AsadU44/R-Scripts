install.packages('ggparliament')
library(ggparliament)
library(tidyverse)
library(ggplot2)

Genes<- c('PSMC1', 'PSMC2', 'PSMC3','PSMC4', 'PSMC5')
Protein_Partners<- c(41, 57, 30, 25, 112)
miRNA_Partners<- c(75, 19, 56, 90, 22)
Colors<- c('#D50000', '#2862B3', '#0C2C84', '#FAB512', '#EA484A')

data<- data.frame(Genes, Protein_Partners, miRNA_Partners, Colors)

write.csv(data, file = 'Parliament Plot Data.csv')

# Create the data frame to be used
Plot_semicircle <- parliament_data(election_data = data,
                                 type = "semicircle", # Parliament type
                                 parl_rows = 10,      # Number of rows of the parliament
                                 party_seats = data$Protein_Partners) # Seats per party
#Semicircle plot
Plot<- ggplot(Plot_semicircle, aes(x = x, y = y, colour = Genes)) +
  geom_parliament_seats(size = 10) + 
  theme_ggparliament() +
  labs(title = "Protein Partners of PSMCs") +
  scale_colour_manual(values = Plot_semicircle$Colors, 
                      limits = Plot_semicircle$Genes) 

Plot



#Circle plot
Plot_circle <- parliament_data(election_data = data,
                                   type = "circle", # Parliament type
                                   parl_rows = 5,      # Number of rows of the parliament
                                   party_seats = data$Protein_Partners) # Seats per party

Plot<- ggplot(Plot_circle, aes(x = x, y = y, colour = Genes)) +
  geom_parliament_seats(size = 10) + 
  theme_ggparliament() +
  labs(title = "Protein Partners of PSMCs") +
  scale_colour_manual(values = Plot_circle$Colors, 
                      limits = Plot_circle$Genes) 
Plot



#Classroom Plot
Plot_classroom <- parliament_data(election_data = data,
                               type = "classroom", # Parliament type
                               parl_rows = 8,      # Number of rows of the parliament
                               party_seats = data$Protein_Partners) # Seats per party
#Semicircle plot
Plot<- ggplot(Plot_classroom, aes(x = x, y = y, colour = Genes)) +
  geom_parliament_seats(size = 10) + 
  theme_ggparliament() +
  labs(title = "Protein Partners of PSMCs") +
  scale_colour_manual(values = Plot_classroom$Colors, 
                      limits = Plot_classroom$Genes) 
Plot

#Adding parliament bar
Plot<- Plot+ geom_parliament_bar(colour = Colors, party = Genes, label = TRUE)
Plot

ggsave(filename = 'Rplot10.jpg', height = 16, width = 20, bg= 'white')