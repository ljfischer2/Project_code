
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

Masu_ctm <- ctm_data$Temp.LOE[ctm_data$Species == "Masu"]
Ito_ctm <- ctm_data$Temp.LOE[ctm_data$Species == "Ito"]
t.test(Masu_ctm, Ito_ctm)



ctmnochar <- ctm_data[-c(ctm_data$Species == "Char"),]
ctmnochar <- subset(ctm_data, ctm_data$Species == "Masu" | ctm_data$Species == "Ito" )


summary(Masu_ctm)
sd(Masu_ctm)

sd(Ito_ctm)


boxplot(ctmnochar$Temp_LOE ~ ctmnochar$Species)
ggplot(data = ctm_data) + 
  geom_point(mapping = aes(x = Trial, y = Temp_LOE,
                           color = Species, size = 2))
  


ggplot(ctmnochar, aes(x=Species, y=Temp.LOE, fill = Species)) + 
  geom_boxplot(linewidth = 1) + 
  labs(x = 'Species',
       y = 'Temperature(°C)') +
#  scale_fill_brewer(palette = 'Dark2') + 
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14)
  ) + 
  scale_fill_manual(values = c('yellow', 'red'))


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
  

ctm_table <- ctmnochar %>%
  select(Species, Fork.Length.mm, Weight.g, Trial, Temp.LOE)

names(ctm_table) <- c('Species', 'Fork Length (mm)', 'Weight (g)',
                      'Trial', 'Temperature of LOE')  
  
#write.csv(ctm_table, 'ctm_table.csv')  
####################################### Final Code? ########################### 

library(dplyr)
library(ggplot2)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")
ctm_data <- read.csv("CTM_Data_File.csv")




ggplot(ctm_data, aes(x=Species, y=Temp.LOE, fill = Species)) + 
  geom_boxplot(linewidth = 1) + 
  labs(x = 'Species',
       y = 'Temperature(°C)') +
  #  scale_fill_brewer(palette = 'Dark2') + 
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 14)
  ) + 
  scale_fill_manual(values = c('goldenrod', 'red'))


EL_rain <- data.frame(Species = 'Rainbow (Eagle Lake)',
                      avg =  29.6,
                      sd = 0.57)
Sha_rain <- data.frame(Species = 'Rainbow',
                       avg = 29.3,
                      sd = 0.25)
cutt <- data.frame(Species = 'Cutthroat',
                   avg = 27,
              sd = 0.8)
wd_brown <- data.frame(Species = 'Brown (Wild)',
                       avg = 29,
                       sd = 0.23)
bell_brown <- data.frame(Species = 'Brown (Bellefonte)',
                         avg = 27.4,
                         sd = 0.42)

masu_df <- ctm_data %>%
  filter(!Species %in% 'Ito') %>%
  group_by(Species) %>%
  summarize(avg = mean(Temp.LOE),
            sd = sd(Temp.LOE))

ito_df <- ctm_data %>%
  filter(!Species %in% 'Masu') %>%
  group_by(Species) %>%
  summarize(avg = mean(Temp.LOE),
            sd = sd(Temp.LOE))


# Combine all data
all_data <- bind_rows(
  masu_df %>% mutate(group = "Masu(18°C)"),
  ito_df %>% mutate(group = "Ito(18°C)"),
  EL_rain %>% mutate(group = "Rainbow (Eagle Lake)(19°C)"),
  bell_brown %>% mutate(group = "Brown (Bellefonte)(12°C)"),
  wd_brown %>% mutate(group = "Brown (Wild)(12°C)"),
)

all_data$Species <- factor(all_data$Species, 
                           levels = c("Ito", "Masu", "Rainbow (Eagle Lake)",
                                      "Brown (Wild)", "Brown (Bellefonte)"))
all_data$group <- factor(all_data$group, 
                           levels = c("Ito(18°C)", "Masu(18°C)", "Rainbow (Eagle Lake)(19°C)",
                                      "Brown (Wild)(12°C)", "Brown (Bellefonte)(12°C)"))
# Simpler plot
ggplot(all_data, aes(x = group, y = avg, color = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd), linewidth = 1.3) +
  geom_text(aes(label = paste0(round(avg, 2), " (", round(sd,1), ")")), 
            y = 26.3, size = 4) +
  theme_minimal() + 
  ylim(26, 30.5) +
  labs(y = 'Average LOE(°C)',
       x = 'Species') +
  scale_color_manual(
    values = c(
      'Brown (Wild)(12°C)' = 'black',
      'Brown (Bellefonte)(12°C)' = 'black',
      'Rainbow (Eagle Lake)(19°C)' = 'black',
      'Ito(18°C)' = 'black',
      'Masu(18°C)' = 'black')) +
  theme(legend.position = 'none',
  axis.text = element_text(size = 10, color = 'black'))





ggplot(masu_df, aes(x = Species, y = avg, color = Species)) +
  geom_point(size = 3) +
  #geom_line() +
  geom_errorbar(aes(ymin = avg - sd,
                    ymax =  avg + sd),
                linewidth = 1.3) +
  
  geom_point(data = ito_df, size = 3) +
  geom_errorbar(data = ito_df, aes(ymin = avg - sd,
                    ymax =  avg + sd),
                linewidth = 1.3) +
  
  geom_point(data = EL_rain, size = 3) +
  geom_errorbar(data = EL_rain, aes(ymin = avg - sd,
                                   ymax =  avg + sd),
                linewidth = 1.3) +

geom_point(data = bell_brown, size = 3) +
  geom_errorbar(data = bell_brown, aes(ymin = avg - sd,
                                    ymax =  avg + sd),
                linewidth = 1.3) +

geom_point(data = wd_brown, size = 3) +
  geom_errorbar(data = wd_brown, aes(ymin = avg - sd,
                                    ymax =  avg + sd),
                linewidth = 1.3) +
    theme_minimal() + 
    scale_color_manual(
      values = c(
        'Brown (Wild)' = 'black',
        'Brown' = 'black',
        'Rainbow' = 'black',
        'Ito' = 'black',
        'Masu' = 'black'))
