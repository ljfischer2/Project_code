<<<<<<< HEAD
rm(list = ls())
library(respR)
library(tidyverse)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data4<-read.csv("Trial_4_Comp.csv",header=T)
data4 <- data4[-c(42751:43215), ]
chamber4.1<-inspect(data4, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber4.2<-inspect(data4, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber4.3<-inspect(data4, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber4.4<-inspect(data4, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber4.5<-inspect(data4, time = 1, oxygen = 11)#
chamber4.6<-inspect(data4, time = 1, oxygen = 12)#
chamber4.7<-inspect(data4, time = 1, oxygen = 13)#
chamber4.8<-inspect(data4, time = 1, oxygen = 14)#

#################################################################################################
#Fish O2 Consumption
#################################################################################################

Masu11 <- calc_rate.int(chamber4.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito13 <- calc_rate.int(chamber4.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu12 <- calc_rate.int(chamber4.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu13 <- calc_rate.int(chamber4.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Blank <- calc_rate.int(chamber4.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito12 <- calc_rate.int(chamber4.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu10 <- calc_rate.int(chamber4.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito11 <- calc_rate.int(chamber4.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data4) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Masu11 <- convert_rate(Masu11,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0022)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito13 <- convert_rate(Ito13,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0175)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu12 <- convert_rate(Masu12,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0028)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu13 <- convert_rate(Masu13,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0032)            # mass of the specimen in kg (fish wet mass: kg)



ggplot(data = data4) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")

convert_Blank <- convert_rate(Blank,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 1)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito12 <- convert_rate(Ito12,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0278)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu10 <- convert_rate(Masu10,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0042)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito11 <- convert_rate(Ito13,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0657)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Masu11)
summary(convert_Ito13)
summary(convert_Masu12)
summary(convert_Masu13)
summary(convert_Blank)
summary(convert_Ito12)
summary(convert_Masu10)
summary(convert_Ito11)

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
dfMasu11 <- data.frame(temp_bin = numeric(),
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
    SMRMasu11_T <- convert_Masu11 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu11_T$rate.output)) SMRMasu11_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu11_T$rate.output) && length(SMRMasu11_T$rate.output) >= o) {
      SMRMasu11_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu11 <- rbind(dfMasu11, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfMasu11$FishID <- "Masu11"
dfMasu11$Species <- "Masu"
View(dfMasu11)
### llx end ###

posrate <- abs(dfMasu11$rate_output)
plot(posrate)
#ggplot(data = dfMasu2, aes(x = temp, y = rate_output)) + 
#  geom_point() +
#  labs(x = "Temp",
#       y = 'Oxygen Consumption(g O2·g−1·day−1)',
#       title  = "Oxygen Consumption Rate of Masu 2") + 
#  scale_color_manual(values = c('#1b9e77'))

#mean(dfMasu2$rate_output)

################################################################################
#
################################################################################
dfIto13 <- data.frame(temp_bin = numeric(),
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
    SMRIto13_T <- convert_Ito13 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRIto13_T$rate.output)) SMRIto13_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto13_T$rate.output) && length(SMRIto13_T$rate.output) >= o) {
      SMRIto13_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto13 <- rbind(dfIto13, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfIto13$FishID <- "Ito13"
dfIto13$Species <- "Ito"
View(dfIto13)
### llx end ###

posrate <- abs(dfIto13$rate_output)
plot(posrate)

################################################################################
#
################################################################################
dfMasu12 <- data.frame(temp_bin = numeric(),
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
    SMRMasu12_T <- convert_Masu12 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu12_T$rate.output)) SMRMasu12_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu12_T$rate.output) && length(SMRMasu12_T$rate.output) >= o) {
      SMRMasu12_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu12 <- rbind(dfMasu12, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfMasu12$FishID <- "Masu12"
dfMasu12$Species <- "Masu"
View(dfMasu12)
### llx end ###

posrate <- abs(dfMasu12$rate_output)
plot(posrate)

################################################################################
#
################################################################################

dfMasu13 <- data.frame(temp_bin = numeric(),
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
    SMRMasu13_T <- convert_Masu13 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu13_T$rate.output)) SMRMasu13_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu13_T$rate.output) && length(SMRMasu13_T$rate.output) >= o) {
      SMRMasu13_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu13 <- rbind(dfMasu13, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfMasu13$FishID <- "Masu13"
dfMasu13$Species <- "Masu"
View(dfMasu13)
### llx end ###
#
posrate <- abs(dfMasu13$rate_output)
plot(posrate)

################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Blank
################################################################################

dfBlank <- data.frame(temp_bin = numeric(),
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
    SMRBlank_T <- convert_Blank |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRBlank_T$rate.output)) SMRBlank_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRBlank_T$rate.output) && length(SMRBlank_T$rate.output) >= o) {
      SMRBlank_T$rate.output[o]
    } else {
      NA
    }
    
    dfBlank <- rbind(dfBlank, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfBlank$FishID <- "Blank"
dfBlank$Species <- "Blank"
dfBlank$Method <- "Intermittent"
dfBlank$rate_output <- dfBlank$rate_output / 1000
View(dfBlank)
posrateBlank <- abs(dfBlank$rate_output)
plot(posrateBlank)

################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Ito12
################################################################################

dfIto12 <- data.frame(temp_bin = numeric(),
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
    SMRIto12_T <- convert_Ito12 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto12_T$rate.output)) SMRIto12_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto12_T$rate.output) && length(SMRIto12_T$rate.output) >= o) {
      SMRIto12_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto12 <- rbind(dfIto12, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfIto12$FishID <- "Ito12"
dfIto12$Species <- "Ito"
View(dfIto12)
posrateIto12 <- abs(dfIto12$rate_output)
plot(posrateIto12)


################################################################################
#Masu9
################################################################################

dfMasu10 <- data.frame(temp_bin = numeric(),
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
    SMRMasu10_T <- convert_Masu10 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu10_T$rate.output)) SMRMasu10_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu10_T$rate.output) && length(SMRMasu10_T$rate.output) >= o) {
      SMRMasu10_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu10 <- rbind(dfMasu10, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfMasu10$FishID <- "Masu10"
dfMasu10$Species <- "Masu"
View(dfMasu10)
posrateMasu10 <- abs(dfMasu10$rate_output)
plot(posrateMasu10)

################################################################################
#Ito13
################################################################################
dfIto11 <- data.frame(temp_bin = numeric(),
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
    SMRIto11_T <- convert_Ito11 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto11_T$rate.output)) SMRIto11_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto11_T$rate.output) && length(SMRIto11_T$rate.output) >= o) {
      SMRIto11_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto11 <- rbind(dfIto11, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 4,
      rep = rep_val
    ))
  }
}

dfIto11$FishID <- "Ito11"
dfIto11$Species <- "Ito"
View(dfIto11)
posrateIto11 <- abs(dfIto11$rate_output)
plot(posrateIto11)


=======
rm(list = ls())
library(respR)
library(tidyverse)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data4<-read.csv("Trial_4_Comp.csv",header=T)
data4 <- data4[-c(42751:43215), ]
chamber4.1<-inspect(data4, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber4.2<-inspect(data4, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber4.3<-inspect(data4, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber4.4<-inspect(data4, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber4.5<-inspect(data4, time = 1, oxygen = 11)#
chamber4.6<-inspect(data4, time = 1, oxygen = 12)#
chamber4.7<-inspect(data4, time = 1, oxygen = 13)#
chamber4.8<-inspect(data4, time = 1, oxygen = 14)#

#################################################################################################
#Fish O2 Consumption
#################################################################################################
?calc_rate
Masu11 <- calc_rate.int(chamber4.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito14 <- calc_rate.int(chamber4.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu12 <- calc_rate.int(chamber4.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu13 <- calc_rate.int(chamber4.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Blank <- calc_rate.int(chamber4.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito12 <- calc_rate.int(chamber4.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu10 <- calc_rate.int(chamber4.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito13 <- calc_rate.int(chamber4.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data4) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Masu11 <- convert_rate(Masu11,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0022)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito14 <- convert_rate(Ito14,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0175)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu12 <- convert_rate(Masu12,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0028)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu13 <- convert_rate(Masu13,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0032)            # mass of the specimen in kg (fish wet mass: kg)



ggplot(data = data4) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")

convert_Blank <- convert_rate(Blank,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito12 <- convert_rate(Ito12,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0278)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu10 <- convert_rate(Masu10,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0042)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito13 <- convert_rate(Ito13,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 2.880,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0657)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Masu11)
summary(convert_Ito14)
summary(convert_Masu12)
summary(convert_Masu13)
summary(convert_Blank)
summary(convert_Ito12)
summary(convert_Masu10)
summary(convert_Ito13)

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
dfMasu11 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu11_T <- convert_Masu11 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu11 <- rbind(dfMasu11, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu11_T$rate.output[o]
    ))
  }
}

View(dfMasu11)
### llx end ###

posrate <- abs(dfMasu11$rate_output)
plot(posrate)
#ggplot(data = dfMasu2, aes(x = temp, y = rate_output)) + 
#  geom_point() +
#  labs(x = "Temp",
#       y = 'Oxygen Consumption(g O2·g−1·day−1)',
#       title  = "Oxygen Consumption Rate of Masu 2") + 
#  scale_color_manual(values = c('#1b9e77'))

#mean(dfMasu2$rate_output)

################################################################################
#
################################################################################
dfIto14 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto14_T <- convert_Ito14 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto14 <- rbind(dfIto14, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto14_T$rate.output[o]
    ))
  }
}

View(dfIto14)
### llx end ###

posrate <- abs(dfIto14$rate_output)
plot(posrate)

################################################################################
#
################################################################################
dfMasu12 <- data.frame(temp_bin = numeric(),
                       rate_output = numeric(),
                       temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu12_T <- convert_Masu12 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu12 <- rbind(dfMasu12, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu12_T$rate.output[o]
    ))
  }
}

View(dfMasu12)
### llx end ###

posrate <- abs(dfMasu12$rate_output)
plot(posrate)

################################################################################
#
################################################################################

dfMasu13 <- data.frame(temp_bin = numeric(),
                       rate_output = numeric(),
                       temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu13_T <- convert_Masu13 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu13 <- rbind(dfMasu13, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu13_T$rate.output[o]
    ))
  }
}

View(dfMasu13)
### llx end ###

posrate <- abs(dfMasu13$rate_output)
plot(posrate)

################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Blank
################################################################################

dfBlank <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRBlank_T <- convert_Blank |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfBlank <- rbind(dfBlank, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRBlank_T$rate.output[o]
    ))
  }
}

View(dfBlank)
posrateBlank <- abs(dfBlank$rate_output)
plot(posrateBlank)

################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Ito12
################################################################################

dfIto12 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto12_T <- convert_Ito12 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto12 <- rbind(dfIto12, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRIto12_T$rate.output[o]
    ))
  }
}

View(dfIto12)
posrateIto12 <- abs(dfIto12$rate_output)
plot(posrateIto12)

################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Masu9
################################################################################

dfMasu10 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu10_T <- convert_Masu10 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu10 <- rbind(dfMasu10, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRMasu10_T$rate.output[o]
    ))
  }
}

View(dfMasu10)
posrateMasu10 <- abs(dfMasu10$rate_output)
plot(posrateMasu10)

################################################################################
#Ito13
################################################################################

dfIto13 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto13_T <- convert_Ito13 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto13 <- rbind(dfIto13, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRIto13_T$rate.output[o]
    ))
  }
}

View(dfIto13)
posrateIto13 <- abs(dfIto13$rate_output)
plot(posrateIto13)


>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
