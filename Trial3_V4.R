<<<<<<< HEAD
#################################################################################################
#Trial 3
#################################################################################################
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data3<-read.csv("Trial_3_Comp.csv",header=T)
################### Chamber 7 only because NaN values############
data3.7 <- data.frame(time = data3$Time,
                      temp = data3$TempB,
                      oxygen = data3$Ch7)
data3.7 <- data3.7[-c(42501:43226),]  #Remove unfinished trials
data3.7 <- data3.7[-c(1:21350), ] 
data3.7 <- data3.7[-c(4500:12149),]
data3.7$time <- seq(1,13500, by = 1)

#########################################################

data3 <- data3[-c(42501:43226),]  #Remove unfinished trials
data3 <- data3[-c(1:21350), ]      #removal of acclimation period
#data3 <- data3[-c(1:100),]
#Ch7 <- data3$Ch7
#data3.7 <- data.frame(time = ,
#                      temp = data3$TempB,
#                      oxygen = data3$Ch7)


#########################
#attempts to remove NaN values
data3.7 <- data.frame(time = data3$Time,
                      temp = data3$TempB,
                      oxygen = data3$Ch7)
data3.7 <- data3.7[-c(4499:12151),]
data3.7$time <- seq(1,13497, by = 1)

chamber3.1<-inspect(data3, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber3.2<-inspect(data3, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber3.3<-inspect(data3, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber3.4<-inspect(data3, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber3.5<-inspect(data3, time = 1, oxygen = 11)#
chamber3.6<-inspect(data3, time = 1, oxygen = 12)#
chamber3.7<-inspect(data3.7, time = 1, oxygen = 3)#
chamber3.8<-inspect(data3, time = 1, oxygen = 14)#





################################################################################
#
################################################################################

Ito8 <- calc_rate.int(chamber3.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito9 <- calc_rate.int(chamber3.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu5 <- calc_rate.int(chamber3.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu6 <-calc_rate.int(chamber3.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu9 <- calc_rate.int(chamber3.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu7 <- calc_rate.int(chamber3.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu8 <- calc_rate.int(chamber3.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito10 <- calc_rate.int(chamber3.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data3) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Ito8 <- convert_rate(Ito8,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0038)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito9 <- convert_rate(Ito9,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0198)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu5 <- convert_rate(Masu5,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0038)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu6 <- convert_rate(Masu6,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0034)            # mass of the specimen in kg (fish wet mass: kg)



#ggplot(data = data2) +
#  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
#  ggtitle("Trials Per Half Degree Change in Temperature") +
#  xlab("Temperature(Celsius)")

convert_Masu9 <- convert_rate(Masu9,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0029)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu7 <- convert_rate(Masu7,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0296)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu8 <- convert_rate(Masu8,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/d/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0219)            # mass of the specimen in kg (fish wet mass: kg)

#SMRMasu8_T <- convert_Masu8 |>
#  select_rate(method = "manual", -c(10)) |>
#  summary() |>
#  mean()

convert_Ito10 <- convert_rate(Ito10,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/d/g",  # desired output unit
                              volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0225)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Ito8)
summary(convert_Ito9)
summary(convert_Masu5)
summary(convert_Masu6)
summary(convert_Masu9)
summary(convert_Masu7)
summary(convert_Masu8)
summary(convert_Ito10)

#########################################################################################################################
#Attempts to bin based on temp.
#####################################################

#What if we select the temp at the start of every trial?
total_rows <- nrow(data3)
num_sets <- total_rows %/% 450
Rep_TempA <- data3$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB <- data3$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

#hist(Rep_TempA, breaks = 11)
#hist(Rep_TempB, breaks = 14)

#plot(convert_Ito4, type = "rate")
#plot(convert_Ito4, type = "overlap")

#######################################################
#Masu 2
######################################################
#### llx start ###
dfIto8 <- data.frame(temp_bin = numeric(),
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
    SMRIto8_T <- convert_Ito8 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRIto8_T$rate.output)) SMRIto8_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto8_T$rate.output) && length(SMRIto8_T$rate.output) >= o) {
      SMRIto8_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto8 <- rbind(dfIto8, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfIto8$FishID <- "Ito8"
dfIto8$Species <- "Ito"
View(dfIto8)
### llx end ###

posrate <- abs(dfIto8$rate_output)
plot(posrate)
ggplot(data = dfIto8, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Ito8") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfIto8$rate_output)

################################################################################
#
################################################################################
dfIto9 <- data.frame(temp_bin = numeric(),
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
    SMRIto9_T <- convert_Ito9 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRIto9_T$rate.output)) SMRIto9_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto9_T$rate.output) && length(SMRIto9_T$rate.output) >= o) {
      SMRIto9_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto9 <- rbind(dfIto9, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfIto9$FishID <- "Ito9"
dfIto9$Species <- "Ito"
View(dfIto9)
posrate <- abs(dfIto9$rate_output)
plot(posrate)

###############################################################################
#
###############################################################################
dfMasu5 <- data.frame(temp_bin = numeric(),
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
    SMRMasu5_T <- convert_Masu5 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu5_T$rate.output)) SMRMasu5_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu5_T$rate.output) && length(SMRMasu5_T$rate.output) >= o) {
      SMRMasu5_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu5 <- rbind(dfMasu5, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfMasu5$FishID <- "Masu5"
dfMasu5$Species <- "Masu"
View(dfMasu5)
posrate <- abs(dfMasu5$rate_output)
plot(posrate)


###############################################################################
#
###############################################################################
dfMasu6 <- data.frame(temp_bin = numeric(),
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
    SMRMasu6_T <- convert_Masu6 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu6_T$rate.output)) SMRMasu6_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu6_T$rate.output) && length(SMRMasu6_T$rate.output) >= o) {
      SMRMasu6_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu6 <- rbind(dfMasu6, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfMasu6$FishID <- "Masu6"
dfMasu6$Species <- "Masu"
View(dfMasu6)
posrate <- abs(dfMasu6$rate_output)
plot(posrate)
################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
view(Rep_TempB_bin)
hist(Rep_TempB_bin, breaks = 12)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Masu9
################################################################################

dfMasu9 <- data.frame(temp_bin = numeric(),
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
    SMRMasu9_T <- convert_Masu9 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu9_T$rate.output)) SMRMasu9_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu9_T$rate.output) && length(SMRMasu9_T$rate.output) >= o) {
      SMRMasu9_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu9 <- rbind(dfMasu9, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfMasu9$FishID <- "Masu9"
dfMasu9$Species <- "Masu"
View(dfMasu9)
posrateMasu9 <- abs(dfMasu9$rate_output)
plot(posrateMasu9)


################################################################################
#
################################################################################

dfMasu7 <- data.frame(temp_bin = numeric(),
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
    SMRMasu7_T <- convert_Masu7 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu7_T$rate.output)) SMRMasu7_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu7_T$rate.output) && length(SMRMasu7_T$rate.output) >= o) {
      SMRMasu7_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu7 <- rbind(dfMasu7, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfMasu7$FishID <- "Masu7"
dfMasu7$Species <- "Masu"
View(dfMasu7)
posrateMasu7 <- abs(dfMasu7$rate_output)
plot(posrateMasu7)

################################################################################
#
################################################################################

dfMasu8 <- data.frame(temp_bin = numeric(),
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
    SMRMasu8_T <- convert_Masu8 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu8_T$rate.output)) SMRMasu8_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu8_T$rate.output) && length(SMRMasu8_T$rate.output) >= o) {
      SMRMasu8_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu8 <- rbind(dfMasu8, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfMasu8$FishID <- "Masu8"
dfMasu8$Species <- "Masu"
View(dfMasu8)
posrateMasu8 <- abs(dfMasu8$rate_output)
plot(posrateMasu8)

################################################################################
#
################################################################################


dfIto10 <- data.frame(temp_bin = numeric(),
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
    SMRIto10_T <- convert_Ito10 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempB[n[o]])) Rep_TempB[n[o]] else NA
    rep_val <- if (!is.null(SMRIto10_T$rate.output)) SMRIto10_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRIto10_T$rate.output) && length(SMRIto10_T$rate.output) >= o) {
      SMRIto10_T$rate.output[o]
    } else {
      NA
    }
    
    dfIto10 <- rbind(dfIto10, data.frame(
      temp_bin = min_tempB_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 3,
      rep = rep_val
    ))
  }
}

dfIto10$FishID <- "Ito10"
dfIto10$Species <- "Ito"
View(dfIto10)
posrateIto10 <- abs(dfIto10$rate_output)
=======
#################################################################################################
#Trial 3
#################################################################################################
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data3<-read.csv("Trial_3_Comp.csv",header=T)
################### Chamber 7 only because NaN values############
data3.7 <- data.frame(time = data3$Time,
                      temp = data3$TempB,
                      oxygen = data3$Ch7)
data3.7 <- data3.7[-c(42501:43226),]  #Remove unfinished trials
data3.7 <- data3.7[-c(1:21350), ] 
data3.7 <- data3.7[-c(4500:12149),]
data3.7$time <- seq(1,13500, by = 1)

#########################################################

data3 <- data3[-c(42501:43226),]  #Remove unfinished trials
data3 <- data3[-c(1:21350), ]      #removal of acclimation period
#data3 <- data3[-c(1:100),]
#Ch7 <- data3$Ch7
#data3.7 <- data.frame(time = ,
#                      temp = data3$TempB,
#                      oxygen = data3$Ch7)


#########################
#attempts to remove NaN values
data3.7 <- data.frame(time = data3$Time,
                      temp = data3$TempB,
                      oxygen = data3$Ch7)
data3.7 <- data3.7[-c(4499:12151),]
data3.7$time <- seq(1,13497, by = 1)

chamber3.1<-inspect(data3, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber3.2<-inspect(data3, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber3.3<-inspect(data3, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber3.4<-inspect(data3, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

chamber3.5<-inspect(data3, time = 1, oxygen = 11)#
chamber3.6<-inspect(data3, time = 1, oxygen = 12)#
chamber3.7<-inspect(data3.7, time = 1, oxygen = 3)#
chamber3.8<-inspect(data3, time = 1, oxygen = 14)#





################################################################################
#
################################################################################

Ito8 <- calc_rate.int(chamber3.1,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Ito9 <- calc_rate.int(chamber3.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu5 <- calc_rate.int(chamber3.3,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

Masu6 <-calc_rate.int(chamber3.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu9 <- calc_rate.int(chamber3.5,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu7 <- calc_rate.int(chamber3.6,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Masu8 <- calc_rate.int(chamber3.7,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

Ito10 <- calc_rate.int(chamber3.8,
                       starts = 450,
                       wait = 375,
                       measure = 75,
                       by = "row")

#############################################################################################################
ggplot(data = data3) +
  geom_histogram(binwidth = .5, mapping = aes(x = TempA)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")


convert_Ito8 <- convert_rate(Ito8,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0038)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito9 <- convert_rate(Ito9,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0198)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu5 <- convert_rate(Masu5,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0038)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu6 <- convert_rate(Masu6,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0034)            # mass of the specimen in kg (fish wet mass: kg)



#ggplot(data = data2) +
#  geom_histogram(binwidth = .5, mapping = aes(x = TempB)) +
#  ggtitle("Trials Per Half Degree Change in Temperature") +
#  xlab("Temperature(Celsius)")

convert_Masu9 <- convert_rate(Masu9,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0029)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu7 <- convert_rate(Masu7,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0296)            # mass of the specimen in kg (fish wet mass: kg)

convert_Masu8 <- convert_rate(Masu8,
                             oxy.unit = "mg/L",       # oxygen units of the original raw data
                             time.unit = "secs",      # time units of the original raw data
                             output.unit = "mg/h/g",  # desired output unit
                             volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                             mass = 0.0219)            # mass of the specimen in kg (fish wet mass: kg)

#SMRMasu8_T <- convert_Masu8 |>
#  select_rate(method = "manual", -c(10)) |>
#  summary() |>
#  mean()

convert_Ito10 <- convert_rate(Ito10,
                              oxy.unit = "mg/L",       # oxygen units of the original raw data
                              time.unit = "secs",      # time units of the original raw data
                              output.unit = "mg/h/g",  # desired output unit
                              volume = 1.350,           # effective volume of the respirometer in L?@chamber volume + tube volume
                              mass = 0.0225)            # mass of the specimen in kg (fish wet mass: kg)

summary(convert_Ito8)
summary(convert_Ito9)
summary(convert_Masu5)
summary(convert_Masu6)
summary(convert_Masu9)
summary(convert_Masu7)
summary(convert_Masu8)
summary(convert_Ito10)

#########################################################################################################################
#Attempts to bin based on temp.
#####################################################

#What if we select the temp at the start of every trial?
total_rows <- nrow(data3)
num_sets <- total_rows %/% 450
Rep_TempA <- data3$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB <- data3$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA, breaks = 11)
hist(Rep_TempB, breaks = 14)

#plot(convert_Ito4, type = "rate")
#plot(convert_Ito4, type = "overlap")

#######################################################
#Masu 2
######################################################
#### llx start ###
dfIto8 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
view(Rep_TempA_bin)
hist(Rep_TempA_bin, breaks = 11)


min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)

for (i in 1:(max_tempA_bin - min_tempA_bin + 1)) {
  n <- which(Rep_TempA_bin == min_tempA_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto8_T <- convert_Ito8 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto8 <- rbind(dfIto8, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto8_T$rate.output[o]
    ))
  }
}

View(dfIto8)
### llx end ###

posrate <- abs(dfIto8$rate_output)
plot(posrate)
ggplot(data = dfIto8, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Ito8") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfIto8$rate_output)

################################################################################
#
################################################################################
dfIto9 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto9_T <- convert_Ito9 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto9 <- rbind(dfIto9, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRIto9_T$rate.output[o]
    ))
  }
}

View(dfIto9)
posrate <- abs(dfIto9$rate_output)
plot(posrate)

###############################################################################
#
###############################################################################
dfMasu5 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu5_T <- convert_Masu5 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu5 <- rbind(dfMasu5, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu5_T$rate.output[o]
    ))
  }
}

View(dfMasu5)
posrate <- abs(dfMasu5$rate_output)
plot(posrate)


###############################################################################
#
###############################################################################
dfMasu6 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())

Rep_TempA_bin <- round(Rep_TempA)
min_temp_bin <- min(Rep_TempA_bin)
max_temp_bin <- max(Rep_TempA_bin)

for (i in 1:(max_temp_bin - min_temp_bin + 1)) {
  n <- which(Rep_TempA_bin == min_temp_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu6_T <- convert_Masu6 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu6 <- rbind(dfMasu6, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempA[n][o],
      rate_output = SMRMasu6_T$rate.output[o]
    ))
  }
}

View(dfMasu6)
posrate <- abs(dfMasu6$rate_output)
plot(posrate)
################################################################################
#
################################################################################

Rep_TempB_bin <- round(Rep_TempB)
view(Rep_TempB_bin)
hist(Rep_TempB_bin, breaks = 12)
min_tempB_bin <- min(Rep_TempB_bin)
max_tempB_bin <- max(Rep_TempB_bin)

################################################################################
#Masu9
################################################################################

dfMasu9 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu9_T <- convert_Masu9 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu9 <- rbind(dfMasu9, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRMasu9_T$rate.output[o]
    ))
  }
}

View(dfMasu9)
posrateMasu9 <- abs(dfMasu9$rate_output)
plot(posrateMasu9)
#ggplot(data = dfIto5, aes(x = temp, y = rate_output)) + 
 # geom_point() +
  #labs(x = "Temp",
   #    y = 'Oxygen Consumption(g O2·g−1·day−1)',
    #   title  = "Oxygen Consumption Rate of Ito 5") + 
  #scale_color_manual(values = c('#1b9e77'))

#mean(dfIto5$rate_output)

################################################################################
#
################################################################################

dfMasu7 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu7_T <- convert_Masu7 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu7 <- rbind(dfMasu7, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRMasu7_T$rate.output[o]
    ))
  }
}

View(dfMasu7)
posrateMasu7 <- abs(dfMasu7$rate_output)
plot(posrateMasu7)

################################################################################
#
################################################################################

dfMasu8 <- data.frame(temp_bin = numeric(),
                      rate_output = numeric(),
                      temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRMasu8_T <- convert_Masu8 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfMasu8 <- rbind(dfMasu8, data.frame(
      tempB_bin = min_tempB_bin + i - 1,
      tempB = Rep_TempB[n][o],
      rate_output = SMRMasu8_T$rate.output[o]
    ))
  }
}

View(dfMasu8)
posrateMasu8 <- abs(dfMasu8$rate_output)
plot(posrateMasu8)

################################################################################
#
################################################################################


dfIto10 <- data.frame(temp_bin = numeric(),
                     rate_output = numeric(),
                     temp = numeric())


for (i in 1:(max_tempB_bin - min_tempB_bin + 1)) {
  n <- which(Rep_TempB_bin == min_tempB_bin + i - 1)
  for (o in 1:length(n)) {
    SMRIto10_T <- convert_Ito10 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Append new row
    dfIto10 <- rbind(dfIto10, data.frame(
      temp_bin = min_temp_bin + i - 1,
      temp = Rep_TempB[n][o],
      rate_output = SMRIto10_T$rate.output[o]
    ))
  }
}

View(dfIto10)
posrateIto10 <- abs(dfIto10$rate_output)
>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
plot(posrateIto10)