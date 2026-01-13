<<<<<<< HEAD
rm(list = ls())

library(dplyr)
library(respR) # load the package
library(tidyverse)
# 1. Check data for errors, selecting cols 1 and 15:
#urch <- inspect(urchins.rd, time = 1, oxygen = 15) 
# 2. Automatically determine most linear regions:
#rate <- auto_rate(urch)
# 3. Convert
#out <- convert_rate(rate,                #example of a dataset
 ##                  time.unit = "min", 
   #                 output.unit = "mg/h/kg", 
    #                volume = 0.6, 
     #               mass = 0.4)
#print(out)

## Alternatively, use pipes:
#urchins.rd %>%        # using the urchins dataset,
#  select(1, 15) %>%   # select columns 1 and 15
#  inspect()     %>%   # inspect the data, then
#  auto_rate()   %>%   # automatically determine most linear segment
##  print()       %>%   # a quick preview
#  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units



#################################################################################################
#Importing Data


library(dplyr)
library(respR) # load the package
library(tidyverse)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/FirestingCSV/Trial 1/Trial1_All")

T1_All <- read.table("trial1_all.txt", sep = "\t")  #Trial 1 Channel 1 practice file
str(T1CH1)
head(T1CH1)

inspect(T1CH1, time = 2, oxygen = 4)

class(T1CH1$V2)

#T1_All$V4 <- as.numeric(T1_All$V4)

#inspect(T1CH1, time = 2, oxygen = 4)

#T1CH1$V2 <- as.numeric(T1CH1$V2)

#format_time(T1CH1, time = 1, format = "HMS")

#T1CH1 <- format_time(T1CH1, time = 2, format = "HMS")
#?format_time

#tinytex::install_tinytex()


start_time <- seq(1, 27000, 1800)#Start times, measure times, end times consistent across all trials
measure_time <- seq(1500, 26700, 1800)
end_time <- seq(1800, 27000, 1800)

##################################################################################################

#Only taking Respirometry values

T1CH1_Parsed <- select(T1_All, 2,4) #selects only the time and oxy column
inspect(T1CH1_Parsed)


T1CH1_Parsed <- T1CH1_Parsed[-c(28785:143921), ]#removes blank columns

#head(T1CH1_Parsed, n = 5)

#format_time(T1CH1_Parsed, time = 1, format = 'HMS')

T1CH1_Parsed <- T1CH1_Parsed[-c(1), ]

T1CH1_Parsed <- format_time(T1CH1_Parsed, time = 1, format = 'HMS')

T1CH1_Parsed$V4 <- as.numeric(T1CH1_Parsed$V4)


inspect(T1CH1_Parsed, time = 3, oxygen = 2)
#?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

#T1CH1_Parsed %>%   
#  select(3, 2) %>%
#  auto_rate()   %>%   # automatically determine most linear segment
#  print()       %>%   # a quick preview
#  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units


T1CH1_Parsed %>%
  select(3, 2)  %>%
  calc_rate.int(starts = start_time,
                wait = 1550,
                measure = end_time, 
                by = "time", width = 0.1)   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
#summary()



#################################################################################################

T1CH1_Temp<- select(T1CH1, 2,4, 12) #including temperature because histogram.

T1CH1_Temp <- T1CH1_Temp[-c(28785:143921), ]#removes blank columns
T1CH1_Temp <- T1CH1_Temp[-c(1), ]

T1CH1_Temp$V12 <- as.numeric(T1CH1_Temp$V12)

hist(T1CH1_Temp$V12, breaks = 10)
hist(T1CH1_Temp$V12, breaks = 15)
hist(T1CH1_Temp$V12, breaks=seq(15, 25, 100))
?hist

#ggplot histogram
?fortify
ggplot(data = T1CH1_Temp) +
  geom_histogram(binwidth = .5, mapping = aes(x = V12)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")



#How to neaten this?
T1CH1_NF<- T1CH1_Temp[c(1500:1800, 3000:3300, 4500:4800,6000:6300, 7500:7800,
                        9000:9300,10500:10800,12000:12300,13500:13800,15000:15300,
                        16500:16800,18000:18300,19500:19800,21000:21300,22500:22800,
                        24000:24300,25500:25800,27000:27300), ]
ggplot(data = T1CH1_NF) + 
  geom_histogram(binwidth = 0.5, mapping = aes(x = V12))

start_time <- seq(1, 108000, 1800)
measure_time <- seq(1500, 106800, 1800)
end_time <- seq(1800, 108000, 1800)
?seq

#######################################################################################################

#Only taking Respirometry values

T1CH2 <- select(T1_All, 20,22) #selects only the time and oxy column
inspect(T1CH2)

T1CH2$V22 <- as.numeric(T1CH2$V22)

T1CH2 <- T1CH2[-c(28785:143921), ]#removes blank columns

head(T1CH2, n = 5)

#format_time(T1CH2, time = 1, format = 'HMS')

T1CH2 <- T1CH2[-c(1), ]

T1CH2 <- format_time(T1CH2, time = 1, format = 'HMS')




inspect(T1CH2, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH2 %>%
  select(3, 2)  %>%
  calc_rate.int(starts = start_time,
                wait = 1550,
                measure = end_time, 
                by = "time", width = 0.1)   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
  summary()

################measureNULL################measure = ####################################################################################################

#Only taking Respirometry values

T1CH3 <- select(T1_All, 38,40) #selects only the time and oxy column
inspect(T1CH1_Parsed)

T1CH3$V40 <- as.numeric(T1CH3$V40)

T1CH3 <- T1CH3[-c(28785:143921), ]#removes blank columns

head(T1CH1_Parsed, n = 5)

#format_time(T1CH2, time = 1, format = 'HMS')

T1CH3 <- T1CH3[-c(1), ]

T1CH3 <- format_time(T1CH3, time = 1, format = 'HMS')




inspect(T1CH2, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH3 %>%   
  select(3, 2) %>%
  auto_rate()   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units

####################################################################################################

#Only taking Respirometry values

T1CH4 <- select(T1_All, 56,58) #selects only the time and oxy column
inspect(T1CH4)

T1CH4$V58 <- as.numeric(T1CH4$V58)

T1CH4 <- T1CH4[-c(28785:143921), ]#removes blank columns

head(T1CH4, n = 5)

#format_time(T1CH4, time = 1, format = 'HMS')

T1CH4 <- T1CH4[-c(1), ]

T1CH4 <- format_time(T1CH4, time = 1, format = 'HMS')




inspect(T1CH4, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH4 %>%   
  select(3, 2) %>%
  auto_rate()   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
=======
rm(list = ls())

library(dplyr)
library(respR) # load the package
library(tidyverse)
# 1. Check data for errors, selecting cols 1 and 15:
#urch <- inspect(urchins.rd, time = 1, oxygen = 15) 
# 2. Automatically determine most linear regions:
#rate <- auto_rate(urch)
# 3. Convert
#out <- convert_rate(rate,                #example of a dataset
 ##                  time.unit = "min", 
   #                 output.unit = "mg/h/kg", 
    #                volume = 0.6, 
     #               mass = 0.4)
#print(out)

## Alternatively, use pipes:
#urchins.rd %>%        # using the urchins dataset,
#  select(1, 15) %>%   # select columns 1 and 15
#  inspect()     %>%   # inspect the data, then
#  auto_rate()   %>%   # automatically determine most linear segment
##  print()       %>%   # a quick preview
#  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units



#################################################################################################
#Importing Data


library(dplyr)
library(respR) # load the package
library(tidyverse)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/FirestingCSV/Trial 1/Trial1_All")

T1_All <- read.table("trial1_all.txt", sep = "\t")  #Trial 1 Channel 1 practice file
str(T1CH1)
head(T1CH1)

inspect(T1CH1, time = 2, oxygen = 4)

class(T1CH1$V2)

#T1_All$V4 <- as.numeric(T1_All$V4)

#inspect(T1CH1, time = 2, oxygen = 4)

#T1CH1$V2 <- as.numeric(T1CH1$V2)

#format_time(T1CH1, time = 1, format = "HMS")

#T1CH1 <- format_time(T1CH1, time = 2, format = "HMS")
#?format_time

#tinytex::install_tinytex()


start_time <- seq(1, 27000, 1800)#Start times, measure times, end times consistent across all trials
measure_time <- seq(1500, 26700, 1800)
end_time <- seq(1800, 27000, 1800)

##################################################################################################

#Only taking Respirometry values

T1CH1_Parsed <- select(T1_All, 2,4) #selects only the time and oxy column
inspect(T1CH1_Parsed)


T1CH1_Parsed <- T1CH1_Parsed[-c(28785:143921), ]#removes blank columns

#head(T1CH1_Parsed, n = 5)

#format_time(T1CH1_Parsed, time = 1, format = 'HMS')

T1CH1_Parsed <- T1CH1_Parsed[-c(1), ]

T1CH1_Parsed <- format_time(T1CH1_Parsed, time = 1, format = 'HMS')

T1CH1_Parsed$V4 <- as.numeric(T1CH1_Parsed$V4)


inspect(T1CH1_Parsed, time = 3, oxygen = 2)
#?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

#T1CH1_Parsed %>%   
#  select(3, 2) %>%
#  auto_rate()   %>%   # automatically determine most linear segment
#  print()       %>%   # a quick preview
#  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units


T1CH1_Parsed %>%
  select(3, 2)  %>%
  calc_rate.int(starts = start_time,
                wait = 1550,
                measure = end_time, 
                by = "time", width = 0.1)   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
#summary()



#################################################################################################

T1CH1_Temp<- select(T1CH1, 2,4, 12) #including temperature because histogram.

T1CH1_Temp <- T1CH1_Temp[-c(28785:143921), ]#removes blank columns
T1CH1_Temp <- T1CH1_Temp[-c(1), ]

T1CH1_Temp$V12 <- as.numeric(T1CH1_Temp$V12)

hist(T1CH1_Temp$V12, breaks = 10)
hist(T1CH1_Temp$V12, breaks = 15)
hist(T1CH1_Temp$V12, breaks=seq(15, 25, 100))
?hist

#ggplot histogram
?fortify
ggplot(data = T1CH1_Temp) +
  geom_histogram(binwidth = .5, mapping = aes(x = V12)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")



#How to neaten this?
T1CH1_NF<- T1CH1_Temp[c(1500:1800, 3000:3300, 4500:4800,6000:6300, 7500:7800,
                        9000:9300,10500:10800,12000:12300,13500:13800,15000:15300,
                        16500:16800,18000:18300,19500:19800,21000:21300,22500:22800,
                        24000:24300,25500:25800,27000:27300), ]
ggplot(data = T1CH1_NF) + 
  geom_histogram(binwidth = 0.5, mapping = aes(x = V12))

start_time <- seq(1, 108000, 1800)
measure_time <- seq(1500, 106800, 1800)
end_time <- seq(1800, 108000, 1800)
?seq

#######################################################################################################

#Only taking Respirometry values

T1CH2 <- select(T1_All, 20,22) #selects only the time and oxy column
inspect(T1CH2)

T1CH2$V22 <- as.numeric(T1CH2$V22)

T1CH2 <- T1CH2[-c(28785:143921), ]#removes blank columns

head(T1CH2, n = 5)

#format_time(T1CH2, time = 1, format = 'HMS')

T1CH2 <- T1CH2[-c(1), ]

T1CH2 <- format_time(T1CH2, time = 1, format = 'HMS')




inspect(T1CH2, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH2 %>%
  select(3, 2)  %>%
  calc_rate.int(starts = start_time,
                wait = 1550,
                measure = end_time, 
                by = "time", width = 0.1)   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
  summary()

################measureNULL################measure = ####################################################################################################

#Only taking Respirometry values

T1CH3 <- select(T1_All, 38,40) #selects only the time and oxy column
inspect(T1CH1_Parsed)

T1CH3$V40 <- as.numeric(T1CH3$V40)

T1CH3 <- T1CH3[-c(28785:143921), ]#removes blank columns

head(T1CH1_Parsed, n = 5)

#format_time(T1CH2, time = 1, format = 'HMS')

T1CH3 <- T1CH3[-c(1), ]

T1CH3 <- format_time(T1CH3, time = 1, format = 'HMS')




inspect(T1CH2, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH3 %>%   
  select(3, 2) %>%
  auto_rate()   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units

####################################################################################################

#Only taking Respirometry values

T1CH4 <- select(T1_All, 56,58) #selects only the time and oxy column
inspect(T1CH4)

T1CH4$V58 <- as.numeric(T1CH4$V58)

T1CH4 <- T1CH4[-c(28785:143921), ]#removes blank columns

head(T1CH4, n = 5)

#format_time(T1CH4, time = 1, format = 'HMS')

T1CH4 <- T1CH4[-c(1), ]

T1CH4 <- format_time(T1CH4, time = 1, format = 'HMS')




inspect(T1CH4, time = 3, oxygen = 2)
?inspect


#flush/wait ends after 1500s = 1500.  Experiment runs 1500 - 1800. 
#Repeats every 1800 with no gaps

T1CH4 %>%   
  select(3, 2) %>%
  auto_rate()   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
