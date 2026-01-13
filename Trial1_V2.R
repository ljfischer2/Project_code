<<<<<<< HEAD
rm(list = ls())

library(respR)

#################################github stuff######################################

 install.packages("usethis")

library(usethis)
use_git_config(user.name = "Lucas Fischer", user.email = "ljfischer2@wisc.edu")

devtools::install_github("r-dbi/odbc")
https://github.com/ljfischer2/project/repo.git
install.packages("devtools")



#################################################################################

ccc


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")
data1<-read.csv("Trial_1_Comp.csv",header=T)
data2<-read.csv("Trial_2_Comp.csv",header=T)
data3<-read.csv("Trial_3_Comp.csv",header=T)
data4<-read.csv("Trial_4_Comp.csv",header=T)

#################################################################################################
#Trial 1
#################################################################################################
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 4th column
chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 5th column
chamber1.3<-inspect(data1, time = 1, oxygen = 7)#time is 1st column, oxygen is 6th column
chamber1.4<-inspect(data1, time = 1, oxygen = 8)#time is 1st column, oxygen is 7th column (in our case chamber is no fish, reference chamber)
 

 
  #######################################################################################################################
#Step1_Fish oxygen cosumption
#######################################################################################################################



smelt1<-calc_rate.int(chamber1.1,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

ggplot(data = data1) +
  geom_histogram(binwidth = .5, mapping = aes(Temp)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")



#in this case, DO measurment time(420 row) + Flash time (180row) = 600 row. Please note we set Firesting to monitori DO every 2s. That is 1 row means 2s.
#start=XX, you should put the total trial time (i.e., measurement time + flash time)
#wait=YY, means removing the first XXs observations (within the measurement time) from the analysis. 
#measure=XX mean the measurement time for the analysis (i.e., total measurement time minus wait time) 
#XX+YY=total meausurement time

smelt2<-calc_rate.int(chamber1.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

smelt3<-calc_rate.int(chamber1.3,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

smelt1<-calc_rate.int(chamber1.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

#######################################################################################################################
#Step2_Substracting the bacteria oxygen cosumption
#######################################################################################################################

#in our case, chamber 4 is only bacteria metabolic rate.

adj1<-adjust_rate(smelt1, chamber4, "concurrent") 
adj2<-adjust_rate(smelt2, chamber4, "concurrent")
adj3<-adjust_rate(smelt3, chamber4, "concurrent")

summary(adj1,pos=1:50)
summary(adj1,pos=51:100)

summary(adj2,pos=1:50)
summary(adj2,pos=51:100)

summary(adj3,pos=1:50)
summary(adj3,pos=51:100)


#######################################################################################################################
#Step3_Conversiton to the exact meatabolic rate (i.e., to  mgO2/hr/g?j
#######################################################################################################################

convert_smelt1 <- convert_rate(smelt1,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.075)            # mass of the specimen in kg (fish wet mass: kg)

convert_smelt2 <- convert_rate(adj2,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.235,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0012)            # mass of the specimen in kg (fish wet mass: kg)

convert_smelt3 <- convert_rate(adj3,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.235,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0016)            # mass of the specimen in kg(fish wet mass: kg)

summary(convert_smelt1)
summary(convert_smelt2)
summary(convert_smelt3)


#######################################################################################################################
#Step4_Measuremtn of SMR from the metablic rates.
#Some studies use 10th percentile as SMR, but Chabot (2016) recommends using 20th percentile as SMR
#######################################################################################################################

#10th percentile
SMR1_10 <- convert_smelt1 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR1_20 <- convert_smelt1 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_smelt1)


#10th percentile
SMR2_10 <- convert_smelt2 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR2_20 <- convert_smelt2 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_smelt2)


#10th percentile
SMR3_10 <- convert_smelt3 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR3_20 <- convert_smelt3 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

=======
rm(list = ls())

library(respR)

#################################github stuff######################################

 install.packages("usethis")

library(usethis)
use_git_config(user.name = "Lucas Fischer", user.email = "ljfischer2@wisc.edu")

devtools::install_github("r-dbi/odbc")
https://github.com/ljfischer2/project/repo.git
install.packages("devtools")



#################################################################################




setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")
data1<-read.csv("Trial_1_Comp.csv",header=T)
data2<-read.csv("Trial_2_Comp.csv",header=T)
data3<-read.csv("Trial_3_Comp.csv",header=T)
data4<-read.csv("Trial_4_Comp.csv",header=T)

#################################################################################################
#Trial 1
#################################################################################################
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 4th column
chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 5th column
chamber1.3<-inspect(data1, time = 1, oxygen = 7)#time is 1st column, oxygen is 6th column
chamber1.4<-inspect(data1, time = 1, oxygen = 8)#time is 1st column, oxygen is 7th column (in our case chamber is no fish, reference chamber)
 

 
  #######################################################################################################################
#Step1_Fish oxygen cosumption
#######################################################################################################################



smelt1<-calc_rate.int(chamber1.1,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

ggplot(data = data1) +
  geom_histogram(binwidth = .5, mapping = aes(Temp)) +
  ggtitle("Trials Per Half Degree Change in Temperature") +
  xlab("Temperature(Celsius)")



#in this case, DO measurment time(420 row) + Flash time (180row) = 600 row. Please note we set Firesting to monitori DO every 2s. That is 1 row means 2s.
#start=XX, you should put the total trial time (i.e., measurement time + flash time)
#wait=YY, means removing the first XXs observations (within the measurement time) from the analysis. 
#measure=XX mean the measurement time for the analysis (i.e., total measurement time minus wait time) 
#XX+YY=total meausurement time

smelt2<-calc_rate.int(chamber1.2,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

smelt3<-calc_rate.int(chamber1.3,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

smelt1<-calc_rate.int(chamber1.4,
                      starts = 450,
                      wait = 375,
                      measure = 75,
                      by = "row")

#######################################################################################################################
#Step2_Substracting the bacteria oxygen cosumption
#######################################################################################################################

#in our case, chamber 4 is only bacteria metabolic rate.

adj1<-adjust_rate(smelt1, chamber4, "concurrent") 
adj2<-adjust_rate(smelt2, chamber4, "concurrent")
adj3<-adjust_rate(smelt3, chamber4, "concurrent")

summary(adj1,pos=1:50)
summary(adj1,pos=51:100)

summary(adj2,pos=1:50)
summary(adj2,pos=51:100)

summary(adj3,pos=1:50)
summary(adj3,pos=51:100)


#######################################################################################################################
#Step3_Conversiton to the exact meatabolic rate (i.e., to  mgO2/hr/g?j
#######################################################################################################################

convert_smelt1 <- convert_rate(smelt1,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.325,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.075)            # mass of the specimen in kg (fish wet mass: kg)

convert_smelt2 <- convert_rate(adj2,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.235,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0012)            # mass of the specimen in kg (fish wet mass: kg)

convert_smelt3 <- convert_rate(adj3,
                               oxy.unit = "mg/L",       # oxygen units of the original raw data
                               time.unit = "secs",      # time units of the original raw data
                               output.unit = "mg/h/g",  # desired output unit
                               volume = 0.235,           # effective volume of the respirometer in L?@chamber volume + tube volume
                               mass = 0.0016)            # mass of the specimen in kg(fish wet mass: kg)

summary(convert_smelt1)
summary(convert_smelt2)
summary(convert_smelt3)


#######################################################################################################################
#Step4_Measuremtn of SMR from the metablic rates.
#Some studies use 10th percentile as SMR, but Chabot (2016) recommends using 20th percentile as SMR
#######################################################################################################################

#10th percentile
SMR1_10 <- convert_smelt1 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR1_20 <- convert_smelt1 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_smelt1)


#10th percentile
SMR2_10 <- convert_smelt2 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR2_20 <- convert_smelt2 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

mean(convert_smelt2)


#10th percentile
SMR3_10 <- convert_smelt3 |>
  select_rate(method = "lowest_percentile", n = 0.1) |>
  summary() |>
  mean()

#20th percentile
SMR3_20 <- convert_smelt3 |>
  select_rate(method = "lowest_percentile", n = 0.2) |>
  summary() |>
  mean()

>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
mean(convert_smelt3)