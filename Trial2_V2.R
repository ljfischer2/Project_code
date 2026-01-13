<<<<<<< HEAD
#################################################################################################
#Trial 2
#################################################################################################
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data2<-read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
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
?calc_rate
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
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
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
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0148)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito6 <- convert_rate(Ito6,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0107)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito7 <- convert_rate(Ito7,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0412)            # mass of the specimen in kg (fish wet mass: kg)

convert_Char1 <- convert_rate(Char1,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
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

############################################################################################################
#Breaks by Temp for Masu2
############################################################################################

#####################################################################

data2$bin_tempA = round(data2$TempA)
data2$bin_tempB = round(data2$TempB)

Rep_TempA


###############################################################
Rep_TempA_bin = round(Rep_TempA)
min_temp <- min(Rep_TempA_bin)
max_temp <- max(Rep_TempA_bin)

for (i in 1:(max_temp-min_temp+1)){
  break_i =  which(Rep_TempA >= (min_temp+i-2) & Rep_TempA < (min_temp+i-1))
  SMRMasu2_T =  convert_Masu2 |>
    select_rate(method = "manual", n = break_i) |>
    summary() |>
    mean()
}


break14 <- which(Rep_TempA <= 14)

Rep_TempA <= 14

SMRMasu2_T14 <- convert_Masu2 |>
  select_rate(method = "manual", n = which(data2$bin_tempA == 15) |>
  summary() |>
  mean()


break15 <- which(Rep_TempA >= 14 & Rep_TempA < 15)

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu2_T15 <- convert_Masu2 |>
  select_rate(method = "manual", n = break15) |>
  summary() |>
  mean()


SMRMasu2Val <- list()

for (i in 1:16) {
  breaks <- which(Rep_TempA >= (7 + i) & Rep_TempA < (8 + i))
  SMRMasu2 <- convert_Masu2 |>
    select_rate(method = "manual", n = breaks) |>
    summary() |>
    mean()
  SMRMasu2Val <- summary(SMRMasu2)
}

SMRMasu2Val <- select(SMRMasu2_T24)









break16 <- which(Rep_TempA >= 15 & Rep_TempA < 16)
Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu2_T16 <- convert_Masu2 |>
  select_rate(method = "manual", n = break16) |>
  summary() |>
  mean()




break17 <- which(Rep_TempA >= 16 & Rep_TempA < 17)
Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu2_T17 <- convert_Masu2 |>
  select_rate(method = "manual", n = break17) |>
  summary() |>
  mean()



break18 <- which(Rep_TempA >= 17 & Rep_TempA < 18)
Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu2_T18 <- convert_Masu2 |>
  select_rate(method = "manual", n = break18) |>
  summary() |>
  mean()



break19 <- which(Rep_TempA >= 18 & Rep_TempA < 19)
Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu2_T19 <- convert_Masu2 |>
  select_rate(method = "manual", n = break19) |>
  summary() |>
  mean()



break20 <- which(Rep_TempA >= 19 & Rep_TempA < 20)
Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu2_T20 <- convert_Masu2 |>
  select_rate(method = "manual", n = break20) |>
  summary() |>
  mean()


break21 <- which(Rep_TempA >= 20 & Rep_TempA < 21)
Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu2_T21 <- convert_Masu2 |>
  select_rate(method = "manual", n = break21) |>
  summary() |>
  mean()



break22 <- which(Rep_TempA >= 21 & Rep_TempA < 22)
Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu2_T22 <- convert_Masu2 |>
  select_rate(method = "manual", n = break22) |>
  summary() |>
  mean()


break23 <- which(Rep_TempA >= 22 & Rep_TempA < 23)
Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu2_T23 <- convert_Masu2 |>
  select_rate(method = "manual", n = break23) |>
  summary() |>
  mean()



break24 <- which(Rep_TempA >= 23 & Rep_TempA < 24)
Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu2_T24 <- convert_Masu2 |>
  select_rate(method = "manual", n = break24) |>
  summary() |>
  mean()

##################################################################################################
#Ito4
##################################################################################################

Rep_TempA <= 14

SMRIto4_T14 <- convert_Ito4 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRIto4_T15 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRIto4_T16 <- convert_Ito4 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRIto_4_T17 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRIto4_T18 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRIto4_T19 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRIto4_T20 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRIto4_T21 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRIto4_T22 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRIto4_T23 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24
    
SMRIto4_T24 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

#########################################################################################################
#Masu3
#########################################################################################################
Rep_TempA <= 14

SMRMasu3_T14 <- convert_Masu3 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu3_T15 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu3_T16 <- convert_Masu3 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu3_T17 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu3_T18 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu3_T19 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu3_T20 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu3_T21 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu3_T22 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu3_T23 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu3_T24 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

##########################################################################################################
#Masu4
##########################################################################################################


Rep_TempA <= 14

SMRMasu4_T14 <- convert_Masu4 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu4_T15 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu4_T16 <- convert_Masu4 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu4_T17 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu4_T18 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu4_T19 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu4_T20 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu4_T21 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu4_T22 <- convert_Masu |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu4_T23 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu4_T24 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

#########################################################################################################

# Channel B Temperatures

#Ito5

#########################################################################################################

Rep_TempB <= 18

SMRIto5_T18 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto5_T19 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto5_T20 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto5_T21 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto5_T22 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto5_T23 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto5_T24 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()


######################################################################################################
#Ito6
######################################################################################################

Rep_TempB <= 18

SMRIto6_T18 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto6_T19 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto6_T20 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto6_T21 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto6_T22 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto6_T23 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto6_T24 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

################################################################################
#Ito7
################################################################################

Rep_TempB <= 18

SMRIto7_T18 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto7_T19 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto7_T20 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto7_T21 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto7_T22 <- convert_Ito |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto7_T23 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto7_T24 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

###############################################################################
#Char1
###############################################################################


Rep_TempB <= 18

SMRChar1_T18 <- convert_Char1 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRChar1_T19 <- convert_Char1 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRChar1_T20 <- convert_Char1 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRChar1_T21 <- convert_Char1 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMChar1_T22 <- convert_Char1 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRChar1_T23 <- convert_Char1 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRChar1_T24 <- convert_Char1 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

############################################################################################################

#10th percentile
SMRMasu2_10 <- convert_Masu2 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu2_20 <- convert_Masu2  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu2)




#10th percentile
SMRIto4_10 <- convert_Ito4 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto4_20 <- convert_Ito4  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito4)




#10th percentile
SMRMasu3_10 <- convert_Masu3 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu3_20 <- convert_Masu3  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu3)




#10th percentile
SMRMasu4_10 <- convert_Masu4 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu4_20 <- convert_Masu4  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu4)




#10th percentile
SMRIto5_10 <- convert_Ito5 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto5_20 <- convert_Ito5  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito5)




#10th percentile
SMRIto6_10 <- convert_Ito6 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto6_20 <- convert_Ito6  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito6)




#10th percentile
SMRIto7_10 <- convert_Ito7 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto7_20 <- convert_Ito7  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito7)




#10th percentile
SMRChar1_10 <- convert_Char1 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRChar1_20 <- convert_Char1  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Char1)
=======
#################################################################################################
#Trial 2
#################################################################################################
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

data2<-read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
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
?calc_rate
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
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
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
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0148)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito6 <- convert_rate(Ito6,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0107)            # mass of the specimen in kg (fish wet mass: kg)

convert_Ito7 <- convert_rate(Ito7,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0412)            # mass of the specimen in kg (fish wet mass: kg)

convert_Char1 <- convert_rate(Char1,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
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

############################################################################################################
#Breaks by Temp for Masu2
############################################################################################

#####################################################################

data2$bin_tempA = round(data2$TempA)
data2$bin_tempB = round(data2$TempB)

Rep_TempA


###############################################################
Rep_TempA_bin = round(Rep_TempA)
min_temp <- min(Rep_TempA_bin)
max_temp <- max(Rep_TempA_bin)

for (i in 1:(max_temp-min_temp+1)){
  break_i =  which(Rep_TempA >= (min_temp+i-2) & Rep_TempA < (min_temp+i-1))
  SMRMasu2_T =  convert_Masu2 |>
    select_rate(method = "manual", n = break_i) |>
    summary() |>
    mean()
}


break14 <- which(Rep_TempA <= 14)

Rep_TempA <= 14

SMRMasu2_T14 <- convert_Masu2 |>
  select_rate(method = "manual", n = which(data2$bin_tempA == 15) |>
  summary() |>
  mean()


break15 <- which(Rep_TempA >= 14 & Rep_TempA < 15)

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu2_T15 <- convert_Masu2 |>
  select_rate(method = "manual", n = break15) |>
  summary() |>
  mean()


SMRMasu2Val <- list()

for (i in 1:16) {
  breaks <- which(Rep_TempA >= (7 + i) & Rep_TempA < (8 + i))
  SMRMasu2 <- convert_Masu2 |>
    select_rate(method = "manual", n = breaks) |>
    summary() |>
    mean()
  SMRMasu2Val <- summary(SMRMasu2)
}

SMRMasu2Val <- select(SMRMasu2_T24)









break16 <- which(Rep_TempA >= 15 & Rep_TempA < 16)
Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu2_T16 <- convert_Masu2 |>
  select_rate(method = "manual", n = break16) |>
  summary() |>
  mean()




break17 <- which(Rep_TempA >= 16 & Rep_TempA < 17)
Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu2_T17 <- convert_Masu2 |>
  select_rate(method = "manual", n = break17) |>
  summary() |>
  mean()



break18 <- which(Rep_TempA >= 17 & Rep_TempA < 18)
Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu2_T18 <- convert_Masu2 |>
  select_rate(method = "manual", n = break18) |>
  summary() |>
  mean()



break19 <- which(Rep_TempA >= 18 & Rep_TempA < 19)
Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu2_T19 <- convert_Masu2 |>
  select_rate(method = "manual", n = break19) |>
  summary() |>
  mean()



break20 <- which(Rep_TempA >= 19 & Rep_TempA < 20)
Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu2_T20 <- convert_Masu2 |>
  select_rate(method = "manual", n = break20) |>
  summary() |>
  mean()


break21 <- which(Rep_TempA >= 20 & Rep_TempA < 21)
Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu2_T21 <- convert_Masu2 |>
  select_rate(method = "manual", n = break21) |>
  summary() |>
  mean()



break22 <- which(Rep_TempA >= 21 & Rep_TempA < 22)
Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu2_T22 <- convert_Masu2 |>
  select_rate(method = "manual", n = break22) |>
  summary() |>
  mean()


break23 <- which(Rep_TempA >= 22 & Rep_TempA < 23)
Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu2_T23 <- convert_Masu2 |>
  select_rate(method = "manual", n = break23) |>
  summary() |>
  mean()



break24 <- which(Rep_TempA >= 23 & Rep_TempA < 24)
Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu2_T24 <- convert_Masu2 |>
  select_rate(method = "manual", n = break24) |>
  summary() |>
  mean()

##################################################################################################
#Ito4
##################################################################################################

Rep_TempA <= 14

SMRIto4_T14 <- convert_Ito4 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRIto4_T15 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRIto4_T16 <- convert_Ito4 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRIto_4_T17 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRIto4_T18 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRIto4_T19 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRIto4_T20 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRIto4_T21 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRIto4_T22 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRIto4_T23 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24
    
SMRIto4_T24 <- convert_Ito4 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

#########################################################################################################
#Masu3
#########################################################################################################
Rep_TempA <= 14

SMRMasu3_T14 <- convert_Masu3 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu3_T15 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu3_T16 <- convert_Masu3 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu3_T17 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu3_T18 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu3_T19 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu3_T20 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu3_T21 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu3_T22 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu3_T23 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu3_T24 <- convert_Masu3 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

##########################################################################################################
#Masu4
##########################################################################################################


Rep_TempA <= 14

SMRMasu4_T14 <- convert_Masu4 |>
  select_rate(method = "manual", n = 59) |>
  summary() |>
  mean()

Rep_TempA >= 14 & Rep_TempA<= 15

SMRMasu4_T15 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(58, 60,61)) |>
  summary() |>
  mean()




Rep_TempA >= 15 & Rep_TempA<= 16

SMRMasu4_T16 <- convert_Masu4 |>
  select_rate(method = "manual", n = 62) |>
  summary() |>
  mean()





Rep_TempA >= 16 & Rep_TempA<= 17

SMRMasu4_T17 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(63,64)) |>
  summary() |>
  mean()




Rep_TempA >= 17 & Rep_TempA<= 18

SMRMasu4_T18 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(65,66)) |>
  summary() |>
  mean()




Rep_TempA >= 18 & Rep_TempA<= 19

SMRMasu4_T19 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(3,5,6,56,67,68,69)) |>
  summary() |>
  mean()




Rep_TempA >= 19 & Rep_TempA <= 20

SMRMasu4_T20 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(1,2,7,8,45:48,70:72)) |>
  summary() |>
  mean()



Rep_TempA >= 20 & Rep_TempA<= 21

SMRMasu4_T21 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(9,10,14:18,44,49,50,73:76)) |>
  summary() |>
  mean()




Rep_TempA >= 21 & Rep_TempA<= 22

SMRMasu4_T22 <- convert_Masu |>
  select_rate(method = "manual", n = c(11,12,19:22,51:53, 77:82)) |>
  summary() |>
  mean()



Rep_TempA >= 22 & Rep_TempA <= 23

SMRMasu4_T23 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(13,23:27,38, 83:89)) |>
  summary() |>
  mean()




Rep_TempA >= 23 & Rep_TempA<= 24

SMRMasu4_T24 <- convert_Masu4 |>
  select_rate(method = "manual", n = c(28:43, 90:95)) |>
  summary() |>
  mean()

#########################################################################################################

# Channel B Temperatures

#Ito5

#########################################################################################################

Rep_TempB <= 18

SMRIto5_T18 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto5_T19 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto5_T20 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto5_T21 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto5_T22 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto5_T23 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto5_T24 <- convert_Ito5 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()


######################################################################################################
#Ito6
######################################################################################################

Rep_TempB <= 18

SMRIto6_T18 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto6_T19 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto6_T20 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto6_T21 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto6_T22 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto6_T23 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto6_T24 <- convert_Ito6 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

################################################################################
#Ito7
################################################################################

Rep_TempB <= 18

SMRIto7_T18 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRIto7_T19 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRIto7_T20 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRIto7_T21 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMIto7_T22 <- convert_Ito |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRIto7_T23 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRIto7_T24 <- convert_Ito7 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

###############################################################################
#Char1
###############################################################################


Rep_TempB <= 18

SMRChar1_T18 <- convert_Char1 |>
  select_rate(method = "manual", n = c(59,60)) |>
  summary() |>
  mean()

Rep_TempB >= 18 & Rep_TempB<= 19

SMRChar1_T19 <- convert_Char1 |>
  select_rate(method = "manual", n = c(58,61:63)) |>
  summary() |>
  mean()




Rep_TempB >= 19 & Rep_TempB <= 20

SMRChar1_T20 <- convert_Char1 |>
  select_rate(method = "manual", n = c(4,5,16:18,46,47,64:67)) |>
  summary() |>
  mean()





Rep_TempB >= 20 & Rep_TempB<= 21

SMRChar1_T21 <- convert_Char1 |>
  select_rate(method = "manual", n = c(1:3,6:8,14,15,19:23,45,48:51,68:72)) |>
  summary() |>
  mean()




Rep_TempB >= 21 & Rep_TempB<= 22

SMRMChar1_T22 <- convert_Char1 |>
  select_rate(method = "manual", n = c(9:11,24:30,44,52:54,73:79)) |>
  summary() |>
  mean()




Rep_TempB >= 22 & Rep_TempB <= 23

SMRChar1_T23 <- convert_Char1 |>
  select_rate(method = "manual", n = c(12,13,31:43,55:57,80:88)) |>
  summary() |>
  mean()




Rep_TempB >= 23 & Rep_TempB <= 24

SMRChar1_T24 <- convert_Char1 |>
  select_rate(method = "manual", n = c(89:94)) |>
  summary() |>
  mean()

############################################################################################################

#10th percentile
SMRMasu2_10 <- convert_Masu2 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu2_20 <- convert_Masu2  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu2)




#10th percentile
SMRIto4_10 <- convert_Ito4 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto4_20 <- convert_Ito4  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito4)




#10th percentile
SMRMasu3_10 <- convert_Masu3 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu3_20 <- convert_Masu3  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu3)




#10th percentile
SMRMasu4_10 <- convert_Masu4 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRMasu4_20 <- convert_Masu4  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Masu4)




#10th percentile
SMRIto5_10 <- convert_Ito5 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto5_20 <- convert_Ito5  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito5)




#10th percentile
SMRIto6_10 <- convert_Ito6 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto6_20 <- convert_Ito6  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito6)




#10th percentile
SMRIto7_10 <- convert_Ito7 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRIto7_20 <- convert_Ito7  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Ito7)




#10th percentile
SMRChar1_10 <- convert_Char1 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMRChar1_20 <- convert_Char1  |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_Char1)
>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
