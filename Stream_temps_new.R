
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)  # for date handling
library(patchwork)

#Ask Michio For list of stream temp Locations, if possible


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Stream Temps")


#### Locations and descriptions of temp sites
locations <- read.csv('eDNA_temp_sites.csv')
#locations$Description.EN

##########################Higurezawa############################################
hig <- read.csv("Higurezawa.csv")

#hig$Time <- as.Date(hig$Time)
hig$date <- as.Date(hig$Time, format = '%m/%d/%y')
hig$juldate <- as.integer(format(hig$date, "%j"))
hig$Temperature <- as.numeric(hig$Temperature)


#?as.Date

hig_avg <- hig %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))
hig_avg <- hig_avg[-1,]


hig_plot <- ggplot(hig_avg, aes(x = juldate, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = juldate, y = max_temp), linetype = 'dashed') +
  labs(title = "Higurezawa",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()
hig_plot

###############################Karibetsu US#####################################
KarUS <- read.csv("KaribetsuUS.csv", header = T)
KarUS$date <- as.Date(KarUS$Time, format = '%m/%d/%y')
KarUS$juldate <- as.integer(format(KarUS$date, "%j"))
KarUS$Temperature <- as.numeric(KarUS$Temperature)

KarUS_avg <- KarUS %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))



KarUS_plot <- ggplot(KarUS_avg, aes(x = juldate, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = juldate, y = max_temp), linetype = 'dashed') +
  labs(title = "Karibetsu Upper",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()
KarUS_plot
############################OOM Oomagari#################################################
OOM <- read.csv("OOM.csv")
OOM$date <- as.Date(OOM$Time, format = '%m/%d/%y')
OOM$juldate <- as.integer(format(OOM$date, "%j"))
OOM$Temperature <- as.numeric(OOM$Temperature)




OOM_avg <- OOM %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))



OOM_plot <- ggplot(OOM_avg, aes(x = juldate, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = juldate, y = max_temp), linetype = 'dashed') +
  labs(title = "Oomagari River",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()
OOM_plot


#####################Karibetsu Weir#############################################
Karw <- read.csv("KarWeir_2023.csv", header = T)
Karw$date <- as.Date(Karw$Time, format = '%m/%d/%y')
Karw$juldate <- as.integer(format(Karw$date, "%j"))
Karw$Temperature <- as.numeric(Karw$Temperature)



Karw_avg <- Karw %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))




KarW_plot <- ggplot(Karw_avg, aes(x = juldate, y = avg_temp)) +
  geom_line(aes(color = 'Average')) +
  geom_line(aes(y = max_temp, color = 'Max'), linetype = 'dashed') +
#  geom_hline(aes(yintercept = 29.48),
#             linewidth = 0.9, linetype = 'solid') +
#  geom_hline(aes(yintercept = 29.02),
#             linewidth = 0.9, linetype = 'dotted') +
  labs(title = "Karibetsu Weir",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    text = element_text(size = 14),
    legend.position = c(0.8, 0.2),
    legend.title = element_blank()) + 
  scale_color_manual(values = c("Average" = "black", "Max" = "black"))
#                                'Ito' = 'solid', 'Masu' = 'dotted'))
KarW_plot





################Mokeuni River ###############

#

mor <- read.csv("MOR_2023-2024.csv", header = T)
mor$date <- as.Date(mor$Time, format = '%m/%d/%y')
mor$juldate <- as.integer(format(mor$date, "%j"))
mor$Temperature <- as.numeric(mor$Temperature)

mor_avg <- mor %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))

mor_plot <- ggplot(mor_avg, aes(x = juldate, y = avg_temp)) +
  geom_line(aes(color = '2023')) +
  geom_line(aes(y = max_temp, color = 'Max'), linetype = 'dashed') +
  geom_hline(aes(yintercept = 25),
             linewidth = 0.9, linetype = 'solid') +
  labs(title = "Mokeuni River",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    text = element_text(size = 14),
    legend.position = c(0.7, 0.2),
    legend.title = element_blank()) +
  scale_color_manual(values = c("2023" = "black", "Max" = "black"))
mor_plot

################ Furukawa Bridge #########

Fur <- read.csv("FUR_2023_10min.csv", header = T)
Fur$date <- as.Date(Fur$Time, format = '%m/%d/%y')
Fur$juldate <- as.integer(format(Fur$date, "%j"))
Fur$Temperature <- as.numeric(Fur$Temperature)

Fur_avg <- Fur %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))



Fur_plot <- ggplot(Fur_avg, aes(x = juldate, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = juldate, y = max_temp), linetype = 'dashed') +
  labs(title = "Furukawa",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal()
Fur_plot

################ Sansen River ######################

#

San <- read.csv("SANR_2023-2024.csv", header = T)
San$date <- as.Date(San$Time, format = '%m/%d/%y')
San$juldate <- as.integer(format(San$date, "%j"))
San$Temperature <- as.numeric(San$Temperature)

San_avg <- San %>%
  group_by(juldate) %>%
  summarize(avg_temp1 = mean(Temperature),
            avg_temp2 = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp1 = max(Temperature),
            max_temp2 = max(Temperature))

San_avg$avg_temp1[187:296] <- NA 
San_avg$avg_temp2[1:186] <- NA

San_avg$max_temp1[187:296] <- NA 
San_avg$max_temp2[1:186] <- NA





San_plot <- ggplot(San_avg, aes(x = juldate, y = avg_temp)) +
  geom_line(aes(y = avg_temp1, color = '2023')) +
  geom_line(aes(y = max_temp1, color = 'Max'), linetype = 'dashed') +
  geom_line(aes(y = avg_temp2, color = '2023')) +
  geom_line(aes(y = max_temp2, color = 'Max'), linetype = 'dashed') +
  labs(title = "Sansen River",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank()) +
  scale_color_manual(values = c("2023" = "black", "Max" = "black"))
San_plot
################### Shobu River ##################### 

#

Shor <- read.csv("SHOR_2023-2024.csv", header = T)
Shor$date <- as.Date(Shor$Time, format = '%m/%d/%y')
Shor$juldate <- as.integer(format(Shor$date, "%j"))
Shor$Temperature <- as.numeric(Shor$Temperature)

Shor_avg <- Shor %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp = max(Temperature))



Shor_plot <- ggplot(Shor_avg, aes(x = juldate, y = avg_temp)) +
  geom_line(aes(color = '2023')) +
  geom_line(aes(y = max_temp, color = 'Max'), linetype = 'dashed') +
  labs(title = "Shobu River",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank()) +
  scale_color_manual(values = c("2023" = "black", "Max" = "black"))
Shor_plot

############## Sarufutsu Upper #####################


#

SRU <- read.csv("SR3_2023-2024.csv", header = T)
SRU$date <- as.Date(SRU$Time, format = '%m/%d/%y')
SRU$juldate <- as.integer(format(SRU$date, "%j"))
SRU$Temperature <- as.numeric(SRU$Temperature)

SRU_avg <- SRU %>%
  group_by(juldate) %>%
  summarize(avg_temp1 = mean(Temperature),
            avg_temp2 = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2),
            max_temp1 = max(Temperature),
            max_temp2 = max(Temperature))

SRU_avg$avg_temp1[187:296] <- NA 
SRU_avg$avg_temp2[1:186] <- NA

SRU_avg$max_temp1[187:296] <- NA 
SRU_avg$max_temp2[1:186] <- NA

SRU_plot <- ggplot(SRU_avg, aes(x = juldate, y = avg_temp)) +
  geom_path(aes(y = avg_temp1, color = '2023')) +
  geom_path(aes(y = avg_temp2, color = '2023')) +
  geom_path(aes(y = max_temp1, color = 'Max'), linetype = 'dashed') +
  geom_path(aes(y = max_temp2, color = 'Max'), linetype = 'dashed') +
  labs(title = "Sarufutsu River Upper",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank()) +
  scale_color_manual(values = c("2023" = "black", "Max" = "black"))
SRU_plot





Fur_plot + hig_plot + KarUS_plot + KarW_plot + mor_plot + 
  OOM_plot + San_plot + Shor_plot + SRU_plot















