library(respR)
library(tidyverse)


setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")

Fishlist <- read.csv('RMR_Fish_Data.csv')

########## Trial 1 #########

data1<-read.csv("Trial_1_Comp.csv",header=T)
#data1 <- data1[-c(42751:42937), ]
#data1 <- data1[-c(1:10800),]
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber1.3<-inspect(data1, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber1.4<-inspect(data1, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

Masu1 <- calc_rate.int(chamber1.1, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)
Ito1  <- calc_rate.int(chamber1.2, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)
Ito2  <- calc_rate.int(chamber1.3, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)
Ito3  <- calc_rate.int(chamber1.4, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Trial_1_FishID<- mget(c("Masu1", "Ito1", "Ito2", "Ito3"))
Trial_1 <- list()
for (i in 1:length(Trial_1_FishID)){
Trial_1[[i]] <- Rate.calc.lm(Fishlist$Chamber[i], 
                          Fishlist$Wt[i], Trial_1_FishID[[i]], 
                          Fishlist$Channel[i], 4, data1 )
}
Fishlist$Channel[i]

############ Trial 2 ###########

data2 <- read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
fish_T2 <- Fishlist %>%
  filter(Fishlist$Trial == 2)
fish_T2 <- Fish_T2[-c(8),]

#chambers <- list()
#Trial_2_FishID <- list()
#for (i in 1:length(Fish_T2)){
#  chambers[[i]] <- inspect(data2, time = 1, oxygen = i + 5)
#  Trial_2_FishID[[i]] <- calc_rate.int(chambers[[i]], starts = 450, wait = 390,
#                                  measure = 60, by = "row")
#}
#Trial_2_FishID[[1]] <- calc_rate.int(chambers[[1]][["dataframe"]], starts = 450, wait = 390,
#                                     measure = 60, by = "row")
#names(Trial_2_FishID) <- c(Fish_T2$FishID)

chamber2.1<-inspect(data2, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber2.2<-inspect(data2, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber2.3<-inspect(data2, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column
chamber2.4<-inspect(data2, time = 1, oxygen = 9)#time is 1st column, oxygen is 9th column
chamber2.5<-inspect(data2, time = 1, oxygen = 10)
chamber2.6<-inspect(data2, time = 1, oxygen = 11)
chamber2.7<-inspect(data2, time = 1, oxygen = 12)
chamber2.8<-inspect(data2, time = 1, oxygen = 13)

#print(Fish_T2$FishID)

Masu2 <- calc_rate.int(chamber2.1, starts = 450, wait = 390,
                   measure = 60, by = "row")
Ito4 <- calc_rate.int(chamber2.2, starts = 450, wait = 390,
                      measure = 60, by = "row")
Masu3 <- calc_rate.int(chamber2.3, starts = 450, wait = 390,
                       measure = 60, by = "row")
Masu4 <- calc_rate.int(chamber2.4, starts = 450, wait = 390,
                       measure = 60, by = "row") 
Ito5 <- calc_rate.int(chamber2.5, starts = 450, wait = 390,
                      measure = 60, by = "row")
Ito6 <- calc_rate.int(chamber2.6, starts = 450, wait = 390,
                      measure = 60, by = "row")
Ito7  <- calc_rate.int(chamber2.7, starts = 450, wait = 390,
                       measure = 60, by = "row")

Trial_2_FishID<- mget(c( "Masu2", "Ito4",  "Masu3", "Masu4",
                         "Ito5",  "Ito6",  "Ito7" ))
Trial_2 <- list()
for (i in 1:length(Trial_2_FishID)){
  Trial_2[[i]] <- Rate.calc.lm(Fish_T2$Chamber[i], 
                               Fish_T2$Wt[i], Trial_2_FishID[[i]], 
                               Fish_T2$Channel[i], 7, #num of fish in the trial
                               data2 )
}



Trial_1_FishID<- mget(c("Masu1", "Ito1", "Ito2", "Ito3"))
Trial_1 <- list()
for (i in 1:length(Trial_1_FishID)){
  Trial_1[[i]] <- Rate.calc.lm(Fishlist$Chamber[i], 
                               Fishlist$Wt[i], Trial_1_FishID[[i]], 
                               Fishlist$Channel[i], 4, data1 )
}
