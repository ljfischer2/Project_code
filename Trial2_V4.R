<<<<<<< HEAD
#################################################################################################
#Trial 2
#################################################################################################W#
rm(list = ls())
library(respR)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data2<-read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
data2 <- data2[-c(1:21600), ]
chamber2.1<-inspect(data2, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber2.2<-inspect(data2, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber2.3<-inspect(data2, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber2.4<-inspect(data2, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber2.5<-inspect(data2, time = 1, oxygen = 11)#
chamber2.6<-inspect(data2, time = 1, oxygen = 12)#
chamber2.7<-inspect(data2, time = 1, oxygen = 13)#
chamber2.8<-inspect(data2, time = 1, oxygen = 14)#

#################################################################################################
#Fish O2 Consumption
#################################################################################################
#?calc_rate
Masu2 <- calc_rate.int(chamber2.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito4 <- calc_rate.int(chamber2.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu3 <- calc_rate.int(chamber2.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu4 <-calc_rate.int(chamber2.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito5 <- calc_rate.int(chamber2.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito6 <- calc_rate.int(chamber2.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito7 <- calc_rate.int(chamber2.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Char1 <- calc_rate.int(chamber2.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data2) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Masu2 <- convert_rate(Masu2,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0056)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito4 <- convert_rate(Ito4,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0068)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu3 <- convert_rate(Masu3,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0025)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu4 <- convert_rate(Masu4,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.003)            # mass of the specimen in kg (fish wet mass: kg)



ggplot(data = data2) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")

convert_Ito5 <- convert_rate(Ito5,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0148)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito6 <- convert_rate(Ito6,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0107)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito7 <- convert_rate(Ito7,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0412)            # mass of the specimen in kg (fish wet mass: kg)

convert_Char1 <- convert_rate(Char1,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0473)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Masu2)
summary(convert_Ito4)
summary(convert_Masu3)
summary(convert_Masu4)
summary(convert_Ito5)
summary(convert_Ito6)
summary(convert_Ito7)
summary(convert_Char1)

#########################################################################################################################
#Attempts to bin based on temp.
#####################################################

#What if we select the temp at the start of every trial?
total_rows <- nrow(data2)
num_sets <- total_rows %/% 450
Rep_TempA <- data2$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB <- data2$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

#hist(Rep_TempA, breaks = 11)
#hist(Rep_TempB, breaks = 14)

#plot(convert_Ito4, type = "rate")
#plot(convert_Ito4, type = "overlap")

#######################################################
#Masu 2
######################################################
#### llx start ###
dfMasu2 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric(),
                      trial = numeric(),
                      rep = numeric())


Rep_TempA_bin <- round(Rep_TempA)
#view(Rep_TempA_bin)
#hist(Rep_TempA_bin, breaks = 11)


min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRMasu2_T <- convert_Masu2 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu2_T$rate.output)) SMRMasu2_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu2_T$rate.output) && length(SMRMasu2_T$rate.output) >= o) {
      SMRMasu2_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu2 <- rbind(dfMasu2, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}



dfMasu2$FishID <- "Masu2"
dfMasu2$Species <- "Masu"
View(dfMasu2)
### llx end ###

posrate <- abs(dfMasu2$rate_output)
plot(posrate)
ggplot(data = dfMasu2, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Masu 2") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfMasu2$rate_output)



##################################################################################
#Ito4
##################################################################################
dfIto4 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric(),
                     trial = numeric(),
                     rep = numeric())


Rep_TempA_bin <- round(Rep_TempA)
#view(Rep_TempA_bin)
#hist(Rep_TempA_bin, breaks = 11)


min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRIto4_T <- convert_Ito4 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRIto4_T$rate.output)) SMRIto4_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto4_T$rate.output) && length(SMRIto4_T$rate.output) >= o) {
      SMRIto4_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto4 <- rbind(dfIto4, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}

dfIto4$FishID <- "Ito4"
dfIto4$Species <- "Ito"
View(dfIto4)
#posrateIto4 <- abs(dfIto4$rate_output)
#plot(posrateIto4)

################################################################################
#Masu3
################################################################################

dfMasu3 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric(),
                      trial = numeric(),
                      rep = numeric())


Rep_TempA_bin <- round(Rep_TempA)
#view(Rep_TempA_bin)
#hist(Rep_TempA_bin, breaks = 11)


min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRMasu3_T <- convert_Masu3 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu3_T$rate.output)) SMRMasu3_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu3_T$rate.output) && length(SMRMasu3_T$rate.output) >= o) {
      SMRMasu3_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu3 <- rbind(dfMasu3, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}



dfMasu3$FishID <- "Masu3"
dfMasu3$Species <- "Masu"
View(dfMasu3)
#posrateMasu3 <- abs(dfMasu3$rate_output)
#plot(posrateMasu3)
################################################################################
#Masu4
################################################################################

dfMasu4 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric(),
                      trial = numeric(),
                      rep = numeric())


Rep_TempA_bin <- round(Rep_TempA)
#view(Rep_TempA_bin)
#hist(Rep_TempA_bin, breaks = 11)


min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRMasu4_T <- convert_Masu4 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu4_T$rate.output)) SMRMasu4_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu4_T$rate.output) && length(SMRMasu4_T$rate.output) >= o) {
      SMRMasu2_T$rate.output[o]
    } else {
      NA
    }
   
    dfMasu4 <- rbind(dfMasu4, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}



dfMasu4$FishID <- "Masu4"
dfMasu4$Species <- "Masu"
View(dfMasu4)
#posrateMasu4 <- abs(dfMasu4$rate_output)
#plot(posrateMasu4)

################################################################################
#FirestingB
################################################################################
Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Ito5
################################################################################

dfIto5 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric(),
                      trial = numeric(),
                      rep = numeric())


Rep_TempB_bin <- round(Rep_TempB)
#view(Rep_TempB_bin)
#hist(Rep_TempB_bin, breaks = 11)


min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRIto5_T <- convert_Ito5 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto5_T$rate.output)) SMRIto5_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto5_T$rate.output) && length(SMRIto5_T$rate.output) >= o) {
      SMRIto5_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto5 <- rbind(dfIto5, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}

dfIto5$FishID <- "Ito5"
dfIto5$Species <- "Ito"
View(dfIto5)

posrateIto5 <- abs(dfIto5$rate_output)
plot(posrateIto5)



################################################################################
#Ito6
################################################################################
dfIto6 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric(),
                     trial = numeric(),
                     rep = numeric())


Rep_TempB_bin <- round(Rep_TempB)
#view(Rep_TempB_bin)
#hist(Rep_TempB_bin, breaks = 11)


min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRIto6_T <- convert_Ito6 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto6_T$rate.output)) SMRIto6_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto6_T$rate.output) && length(SMRIto6_T$rate.output) >= o) {
      SMRIto6_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto6 <- rbind(dfIto6, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}

dfIto6$FishID <- "Ito6"
dfIto6$Species <- "Ito"
View(dfIto6)

posrateIto6 <- abs(dfIto6$rate_output)
plot(posrateIto6)
################################################################################
#Ito7
################################################################################
dfIto7 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric(),
                     trial = numeric(),
                     rep = numeric())


Rep_TempB_bin <- round(Rep_TempB)
#view(Rep_TempB_bin)
#hist(Rep_TempB_bin, breaks = 11)


min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  
  if (length(n) == 0) next  # Skip if no match due to NA or no values
  
  for (o in 1:length(n)) {
    SMRIto7_T <- convert_Ito7 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto7_T$rate.output)) SMRIto7_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto7_T$rate.output) && length(SMRIto7_T$rate.output) >= o) {
      SMRIto7_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto7 <- rbind(dfIto7, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 2,
      rep = rep_val
    ))
  }
}

dfIto7$FishID <- "Ito7"
dfIto7$Species <- "Ito"
View(dfIto7)

posrateIto7 <- abs(dfIto7$rate_output)
plot(posrateIto7)
=======
#################################################################################################
#Trial 2
#################################################################################################
rm(list = ls())
library(respR)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data2<-read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
data2 <- data2[-c(1:21600), ]
chamber2.1<-inspect(data2, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber2.2<-inspect(data2, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber2.3<-inspect(data2, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber2.4<-inspect(data2, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber2.5<-inspect(data2, time = 1, oxygen = 11)#
chamber2.6<-inspect(data2, time = 1, oxygen = 12)#
chamber2.7<-inspect(data2, time = 1, oxygen = 13)#
chamber2.8<-inspect(data2, time = 1, oxygen = 14)#

#################################################################################################
#Fish O2 Consumption
#################################################################################################
#?calc_rate
Masu2 <- calc_rate.int(chamber2.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito4 <- calc_rate.int(chamber2.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu3 <- calc_rate.int(chamber2.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu4 <-calc_rate.int(chamber2.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito5 <- calc_rate.int(chamber2.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito6 <- calc_rate.int(chamber2.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito7 <- calc_rate.int(chamber2.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Char1 <- calc_rate.int(chamber2.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data2) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Masu2 <- convert_rate(Masu2,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0056)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito4 <- convert_rate(Ito4,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0068)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu3 <- convert_rate(Masu3,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0025)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu4 <- convert_rate(Masu4,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.003)            # mass of the specimen in kg (fish wet mass: kg)



ggplot(data = data2) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")

convert_Ito5 <- convert_rate(Ito5,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0148)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito6 <- convert_rate(Ito6,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0107)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito7 <- convert_rate(Ito7,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0412)            # mass of the specimen in kg (fish wet mass: kg)

convert_Char1 <- convert_rate(Char1,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0473)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Masu2)
summary(convert_Ito4)
summary(convert_Masu3)
summary(convert_Masu4)
summary(convert_Ito5)
summary(convert_Ito6)
summary(convert_Ito7)
summary(convert_Char1)

#########################################################################################################################
#Attempts to bin based on temp.
#####################################################

#What if we select the temp at the start of every trial?
total_rows <- nrow(data2)
num_sets <- total_rows %/% 450
Rep_TempA <- data2$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB <- data2$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA, breaks = 11)
hist(Rep_TempB, breaks = 14)

plot(convert_Ito4, type = "rate")
plot(convert_Ito4, type = "overlap")

#######################################################
#Masu 2
######################################################
#### llx start ###
dfMasu2 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu2_T <- convert_Masu2 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu2 <- rbind(dfMasu2, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu2_T$rate.output[o]
    ))
  }
}

View(dfMasu2)
### llx end ###

posrate <- abs(dfMasu2$rate_output)
plot(posrate)
ggplot(data = dfMasu2, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Masu 2") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfMasu2$rate_output)



##################################################################################
#Ito4
##################################################################################
dfIto4 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto4_T <- convert_Ito4 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto4 <- rbind(dfIto4, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto4_T$rate.output[o]
    ))
  }
}

View(dfIto4)
#posrateIto4 <- abs(dfIto4$rate_output)
#plot(posrateIto4)

################################################################################
#Masu3
################################################################################

dfMasu3 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu3_T <- convert_Masu3 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu3 <- rbind(dfMasu3, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu3_T$rate.output[o]
    ))
  }
}

View(dfMasu3)
#posrateMasu3 <- abs(dfMasu3$rate_output)
#plot(posrateMasu3)
################################################################################
#Masu4
################################################################################

dfMasu4 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu4_T <- convert_Masu4 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu4 <- rbind(dfMasu4, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu4_T$rate.output[o]
    ))
  }
}

View(dfMasu4)
#posrateMasu4 <- abs(dfMasu4$rate_output)
#plot(posrateMasu4)

################################################################################
#FirestingB
################################################################################
Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Ito5
################################################################################

dfIto5 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto5_T <- convert_Ito5 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto5 <- rbind(dfIto5, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempB[n][o],
      rate_output = SMRIto5_T$rate.output[o]
    ))
  }
}

View(dfIto5)
posrateIto5 <- abs(dfIto5$rate_output)
plot(posrateIto5)
ggplot(data = dfIto5, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Ito 5") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfIto5$rate_output)


################################################################################
#Ito6
################################################################################
dfIto6 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto6_T <- convert_Ito6 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto6 <- rbind(dfIto6, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempB[n][o],
      rate_output = SMRIto6_T$rate.output[o]
    ))
  }
}

View(dfIto6)
posrateIto6 <- abs(dfIto6$rate_output)
plot(posrateIto6)
################################################################################
#Ito7
################################################################################
dfIto7 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto7_T <- convert_Ito7 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto7 <- rbind(dfIto7, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempB[n][o],
      rate_output = SMRIto7_T$rate.output[o]
    ))
  }
}

View(dfIto7)
posrateIto7 <- abs(dfIto7$rate_output)
plot(posrateIto7)
>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
