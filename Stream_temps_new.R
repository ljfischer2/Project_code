
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)  # for date handling


#Ask Michio For list of stream temp Locations, if possible


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Stream Temps")


#### Locations and descriptions of temp sites
locations <- read.csv('eDNA_temp_sites.csv')
locations$Description.EN

##########################Higurezawa############################################
hig <- read.csv("Higurezawa.csv")

#hig$Time <- as.Date(hig$Time)
hig$date <- as.Date(hig$Time, format = '%m/%d/%y')
hig$Temperature <- as.numeric(hig$Temperature)


#?as.Date

hig_avg <- hig %>%
  group_by(date) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 1.5))



hig_plot <- ggplot(hig_avg, aes(x = date, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = date, y = fut_avg_temp), linetype = 'dashed') +
  labs(title = "Higurezawa Average Daily Temperature",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()


###############################Karibetsu US#####################################
KarUS <- read.csv("KaribetsuUS.csv", header = T)
KarUS$date <- as.Date(KarUS$Time, format = '%m/%d/%y')
KarUS$Temperature <- as.numeric(KarUS$Temperature)

KarUS_avg <- KarUS %>%
  group_by(date) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 1.5))



KarUS_plot <- ggplot(KarUS_avg, aes(x = date, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = date, y = fut_avg_temp), linetype = 'dashed') +
  labs(title = "Karibetsu Average Daily Temperature",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()

############################OOM Oomagari#################################################
OOM <- read.csv("OOM.csv")
OOM$date <- as.Date(OOM$Time, format = '%m/%d/%y')
OOM$Temperature <- as.numeric(OOM$Temperature)




OOM_avg <- OOM %>%
  group_by(date) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 1.5))



OOM_plot <- ggplot(OOM_avg, aes(x = date, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = date, y = fut_avg_temp), linetype = 'dashed') +
  labs(title = "OOM Average Daily Temperature",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()



#####################Karibetsu Weir#############################################
Karw <- read.csv("KarWeir_2023.csv", header = T)
Karw$date <- as.Date(Karw$Time, format = '%m/%d/%y')
Karw$juldate <- as.integer(format(Karw$date, "%j"))
Karw$Temperature <- as.numeric(Karw$Temperature)

Karw_avg <- Karw %>%
  group_by(juldate) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 2))



KarW_plot <- ggplot(Karw_avg, aes(x = juldate, y = avg_temp)) +
  geom_line(aes(color = '2023')) +
  geom_line(aes(y = fut_avg_temp, color = 'Future'), linetype = 'dashed') +
  labs(#title = "Karibetsu Average Daily Temperature",
       x = "Julian Day",
       y = "Temperature (°C)") + 
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.2),
    legend.title = element_blank()) +
  scale_color_manual(values = c("2023" = "black", "Future" = "black"))

################Mokeuni River ###############
mor <- read.csv("MOR_2023-2024.csv", header = T)
mor$date <- as.Date(mor$Time, format = '%m/%d/%y')
mor$Temperature <- as.numeric(mor$Temperature)

mor_avg <- mor %>%
  group_by(date) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 1.5))



mor_plot <- ggplot(mor_avg, aes(x = date, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = date, y = fut_avg_temp), linetype = 'dashed') +
  labs(title = "Mokeuni Average Daily Temperature",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()



################ Furukawa Bridge #########


mor <- read.csv("MOR_2023-2024.csv", header = T)
mor$date <- as.Date(mor$Time, format = '%m/%d/%y')
mor$Temperature <- as.numeric(mor$Temperature)

mor_avg <- mor %>%
  group_by(date) %>%
  summarize(avg_temp = mean(Temperature),
            fut_avg_temp = (mean(Temperature) + 1.5))



mor_plot <- ggplot(mor_avg, aes(x = date, y = avg_temp)) +
  geom_line() +
  geom_line(aes(x = date, y = fut_avg_temp), linetype = 'dashed') +
  labs(title = "Mokeuni Average Daily Temperature",
       x = "Date",
       y = "Temperature (°C)") + 
  theme_minimal()












library(ggplot2)
library(dplyr)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")

################################################################################
#Higurezawa
################################################################################


hig <- read.csv("Higurezawa.csv")

hig$Time <- as.POSIXct(hig$Time, format = "%m/%d/%Y %H:%M:%S")
hig$date <- as.Date(hig$Time)
hig$timeint <- as.POSIXlt(hig$Time)
hig$timeint <- format(hig$time, "%H:%M:%S")

hig_daily_avg <- hig %>%
  group_by(date) %>%
  summarise(avg_temperature = mean(Temperature, na.rm = TRUE))

ggplot(hig_daily_avg, aes(x = date, y = avg_temperature)) +
  geom_line() +
  labs(title = "Average Daily Temperature", x = "Date", y = "Temperature (°C)")
################################################################################

################################################################################
#Karibetsu US
################################################################################
KarUS <- read.csv("KaribetsuUS.csv")

KarUS$Time <- as.POSIXct(KarUS$Time, format = "%m/%d/%Y %H:%M:%S")
KarUS$date <- as.Date(KarUS$Time)
KarUS$timeint <- as.POSIXlt(KarUS$Time)
KarUS$timeint <- format(KarUS$time, "%H:%M:%S")

KarUSdaily_avg <- KarUS %>%
  group_by(date) %>%
  summarise(avg_temperature = mean(Temperature, na.rm = TRUE))

ggplot(KarUSdaily_avg, aes(x = date, y = avg_temperature)) +
  geom_line() +
  labs(title = "Average Daily Temperature", x = "Date", y = "Temperature (°C)")

################################################################################

################################################################################
#OOM
################################################################################
OOM <- read.csv("OOM.csv")

OOM$Time <- as.POSIXct(OOM$Time, format = "%m/%d/%Y %H:%M:%S")
OOM$date <- as.Date(OOM$Time)
OOM$timeint <- as.POSIXlt(OOM$Time)
OOM$timeint <- format(OOM$time, "%H:%M:%S")

OOM_daily_avg <- OOM %>%
  group_by(date) %>%
  summarise(avg_temperature = mean(Temperature, na.rm = TRUE))

ggplot(OOM_daily_avg, aes(x = date, y = avg_temperature)) +
  geom_line() +
  labs(title = "Average Daily Temperature", x = "Date", y = "Temperature (°C)")

################################################################################

################################################################################
#OOM
>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
################################################################################