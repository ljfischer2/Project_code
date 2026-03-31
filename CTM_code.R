
library(dplyr)
library(tidyverse)
#install.packages("ggpattern")
library(ggpattern)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject")
ctm_data <- read.csv("CTM_data_analysis.csv")
fish_colors <- c(Ito = 'darkgrey', Masu = 'white')




Masu_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Masu"]
Ito_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Ito"]
t.test(Masu_ctm, Ito_ctm)

#

ctmnochar <- ctm_data[-c(ctm_data$Species == "Char"),]
ctmnochar <- subset(ctm_data, ctm_data$Species == "Masu" | 
  ctm_data$Species == "Ito" | ctm_data$Species == "Rainbow"|
  ctm_data$Species == "Brown")

#if (ctmnochar$Species == "Masu") {
#  ctmnochar$value <- 1}




boxplot(ctmnochar$Temp_LOE ~ ctmnochar$Species)
ggplot(data = ctm_data) + 
  geom_point(mapping = aes(x = Trial, y = Temp_LOE,
                           color = Species, size = 2))


mean(ctmnochar$Temp_LOE[ctmnochar$Species == 'Ito'])
sd(ctmnochar$Temp_LOE[ctmnochar$Species == 'Ito'])
mean(ctmnochar$Temp_LOE[ctmnochar$Species == 'Masu'])
sd(ctmnochar$Temp_LOE[ctmnochar$Species == 'Masu'])


  
# A really basic boxplot.




# to reorder the species
ctmnochar$Species <- factor(ctmnochar$Species, levels = c("Ito", "Masu", 'Rainbow', "Brown"))


ggplot(ctmnochar, aes(x=Species, y=Temp_LOE, fill = Species)) + 
  geom_boxplot() + 
  labs(#title = 'Critical Thermal Maxima',
       x = 'Species',
       y = 'Temperature(Celsius)') +
  theme_minimal() +
  theme(text = element_text(size = 20),
        #axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) + 
  theme(legend.position = "none") 
  #scale_fill_brewer(palette = 'Dark2')
  #xlab("Species") + ylab("Temperature(Celsius)") +
  #title('Critical Thermal Maxima')

#####################################################################



library(dplyr)
library(ggplot2)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject")
ctm_data <- read.csv("CTM_data_analysis.csv")
?t.test
Masu_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Masu"]
Ito_ctm <- ctm_data$Temp_LOE[ctm_data$Species == "Ito"]
t.test(Masu_ctm, Ito_ctm)



ctmnochar <- ctm_data[-c(ctm_data$Species == "Char"),]
ctmnochar <- subset(ctm_data, ctm_data$Species == "Masu" | ctm_data$Species == "Ito" )


?boxplot
boxplot(ctmnochar$Temp_LOE ~ ctmnochar$Species)
ggplot(data = ctm_data) + 
  geom_point(mapping = aes(x = Trial, y = Temp_LOE,
                           color = Species, size = 2))
  
# A really basic boxplot.

ggplot(ctmnochar, aes(x=Species, y=Temp_LOE, fill = Species)) + 
  geom_boxplot(linewidth = 1) + 
  labs(x = 'Species',
       y = 'Temperature(°C)') +
#  scale_fill_brewer(palette = 'Dark2') + 
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)
  ) + 
  scale_fill_manual(values = fish_colors)


  xlab("Species") + ylab("Temperature(Celsius)") +
    title('Critical Thermal Maxima') +
              
?plot
?geom_point


  
  geom_boxplot_pattern(
    fill = 'white',
    color = 'black',
    pattern_fill = 'black',
    pattern_density = 0.1,
    pattern_spacing = 0.05
  ) 