<<<<<<< HEAD
library(ggplot2)
library(dplyr)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Stream Temps")


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
=======
library(ggplot2)
library(dplyr)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo/Stream Temps")


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