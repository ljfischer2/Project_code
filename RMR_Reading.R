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


fish_T1 <- Fishlist %>%
  filter(Fishlist$Trial == 1)

Trial_1_FishID<- mget(c("Masu1", "Ito1", "Ito2", "Ito3"))
Trial_1 <- list()
for (i in 1:length(Trial_1_FishID)){
  Trial_1[[i]] <- Rate.calc.lm(fish_T1$vol[i], fish_T1$mass[i], fish_T1,
                               Trial_1_FishID[[i]], fish_T1$Channel[i], 4, data1)
  Trial_1[[i]][["raw"]]$FishID <- fish_T1$FishID[i]
  Trial_1[[i]][["raw"]]$Species <- fish_T1$Species[i]
  Trial_1[[i]][["raw"]]$trial <- 1
}



names(Trial_1) <- c("Masu1", "Ito1", "Ito2", "Ito3")



############ Trial 2 ###########

data2 <- read.csv("Trial_2_Comp.csv",header=T)
data2 <- data2[-c(42751:42937), ]
fish_T2 <- Fishlist %>%
  filter(Fishlist$Trial == 2)
fish_T2 <- fish_T2[-c(8),]

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
                   measure = 60, by = "row", pos = 1:5)
Ito4 <- calc_rate.int(chamber2.2, starts = 450, wait = 390,
                      measure = 60, by = "row", pos = 1)
Masu3 <- calc_rate.int(chamber2.3, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1)
Masu4 <- calc_rate.int(chamber2.4, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1) 
Ito5 <- calc_rate.int(chamber2.5, starts = 450, wait = 390,
                      measure = 60, by = "row", pos = 1)
Ito6 <- calc_rate.int(chamber2.6, starts = 450, wait = 390,
                      measure = 60, by = "row", pos = 1)
Ito7  <- calc_rate.int(chamber2.7, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1)

Trial_2_FishID<- mget(c("Masu2", "Ito4",  "Masu3", "Masu4",
                         "Ito5",  "Ito6",  "Ito7" ))
Trial_2 <- list()
for (i in 1:length(Trial_2_FishID)){
  Trial_2[[i]] <- Rate.calc.lm(fish_T2$vol[i], fish_T2$mass[i], fish_T2,
                               Trial_2_FishID[[i]], fish_T2$Channel[i], 7, data2)
  Trial_2[[i]][["raw"]]$FishID <- fish_T2$FishID[i]
  Trial_2[[i]][["raw"]]$Species <- fish_T2$Species[i]
  Trial_2[[i]][["raw"]]$trial <- 2
}

names(Trial_2) <- c("Masu2", "Ito4",  "Masu3", "Masu4",
                    "Ito5",  "Ito6",  "Ito7")

######## Trial 3 ##########

data3 <- read.csv("Trial_3_Comp.csv",header=T)
data3 <- data3[-c(43201:43226), ]
data3 <- data3[-c(1:13250),]
data3 <- data3[-c(20251:29950),]
fish_T3 <- Fishlist %>%
  filter(Fishlist$Trial == 3)
fish_T3 <- fish_T3[-c(7),]


chamber3.1<-inspect(data3, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber3.2<-inspect(data3, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber3.3<-inspect(data3, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column
chamber3.4<-inspect(data3, time = 1, oxygen = 9)#time is 1st column, oxygen is 9th column
chamber3.5<-inspect(data3, time = 1, oxygen = 10)
chamber3.6<-inspect(data3, time = 1, oxygen = 11)
#chamber3.7<-inspect(data3, time = 1, oxygen = 12)
chamber3.8<-inspect(data3, time = 1, oxygen = 13)

#print(fish_T3$FishID)

Ito8 <- calc_rate.int(chamber3.1, starts = 450, wait = 375,
                       measure = 60, by = "row", pos = 1)
Ito9 <- calc_rate.int(chamber3.2, starts = 450, wait = 365,
                      measure = 60, by = "row", pos = 1)
Masu5 <- calc_rate.int(chamber3.3, starts = 450, wait = 375,
                       measure = 60, by = "row", pos = 1)
Masu6 <- calc_rate.int(chamber3.4, starts = 450, wait = 375,
                       measure = 60, by = "row", pos = 1) 
Masu9 <- calc_rate.int(chamber3.5, starts = 450, wait = 375,
                       measure = 60, by = "row", pos = 1) 
Masu7 <- calc_rate.int(chamber3.6, starts = 450, wait = 375,
                      measure = 60, by = "row", pos = 1)
#Masu8 <- calc_rate.int(chamber3.7, starts = 450, wait = 375,
#                      measure = 60, by = "row", pos = 1)
Ito10  <- calc_rate.int(chamber3.8, starts = 450, wait = 375,
                       measure = 60, by = "row", pos = 1)

Trial_3_FishID<- mget(c("Ito8",  "Ito9",  "Masu5", "Masu6",
                        "Masu9", "Masu7", "Ito10"))

row.names(fish_T3) = c(1:7)
#row.names(fish_T3)

Trial_3 <- list()
for (i in 1:length(Trial_3_FishID)){
  Trial_3[[i]] <- Rate.calc.lm(fish_T3$vol[i], fish_T3$mass[i],fish_T3, 
                               Trial_3_FishID[[i]], fish_T3$Channel[i], 7, data3)
  Trial_3[[i]][["raw"]]$FishID <- fish_T3$FishID[i]
  Trial_3[[i]][["raw"]]$Species <- fish_T3$Species[i]
  Trial_3[[i]][["raw"]]$trial <- 3
}

names(Trial_3) <- c("Ito8",  "Ito9",  "Masu5", "Masu6",
                    "Masu9", "Masu7", "Ito10")


############# Trial 4 #########

data4 <- read.csv("Trial_4_Comp.csv",header=T)
data4 <- data4[-c(43201:43215), ]
fish_T4 <- Fishlist %>%
  filter(Fishlist$Trial == 4)
#fish_T4 <- fish_T4[-c(5),]


chamber4.1<-inspect(data4, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column
chamber4.2<-inspect(data4, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column
chamber4.3<-inspect(data4, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column
chamber4.4<-inspect(data4, time = 1, oxygen = 9)#time is 1st column, oxygen is 9th column
chamber4.5<-inspect(data4, time = 1, oxygen = 10)
chamber4.6<-inspect(data4, time = 1, oxygen = 11)
chamber4.7<-inspect(data4, time = 1, oxygen = 12)
chamber4.8<-inspect(data4, time = 1, oxygen = 13)

print(fish_T4$FishID)

Masu11 <- calc_rate.int(chamber4.1, starts = 450, wait = 370,
                        measure = 55, by = "row", pos = 1)
Ito13 <- calc_rate.int(chamber4.2, starts = 450, wait = 370,
                      measure = 55, by = "row", pos = 1)
Masu12 <- calc_rate.int(chamber4.3, starts = 450, wait = 370,
                       measure = 55, by = "row", pos = 1)
Masu13 <- calc_rate.int(chamber4.4, starts = 450, wait = 370,
                       measure = 55, by = "row", pos = 1) 
Blank <- calc_rate.int(chamber4.5, starts = 450, wait = 370,
                       measure = 55, by = "row", pos = 1) 
Ito12 <- calc_rate.int(chamber4.6, starts = 450, wait = 360,
                       measure = 55, by = "row", pos = 1)
Masu10 <- calc_rate.int(chamber4.7, starts = 450, wait = 370,
                      measure = 55, by = "row", pos = 1)
Ito11  <- calc_rate.int(chamber4.8, starts = 450, wait = 370,
                        measure = 55, by = "row", pos = 1)

Trial_4_FishID<- mget(c("Masu11", "Ito13", "Masu12","Masu13",
                        'Blank', "Ito12", "Masu10", "Ito11" ))

#row.names(fish_T4) = c(1:7)
#row.names(fish_T4)

Trial_4 <- list()
for (i in 1:length(Trial_4_FishID)){
  Trial_4[[i]] <- Rate.calc.lm(fish_T4$vol[i], fish_T4$mass[i],fish_T4, 
                               Trial_4_FishID[[i]], fish_T4$Channel[i], 7, data4)
  Trial_4[[i]][["raw"]]$FishID <- fish_T4$FishID[i]
  Trial_4[[i]][["raw"]]$Species <- fish_T4$Species[i]
  Trial_4[[i]][["raw"]]$trial <- 4
}

names(Trial_4) <- c("Masu11", "Ito13", "Masu12", "Masu13", 
                    'Blank', "Ito12", "Masu10", "Ito11" )


Trial_4_old <- Trial_4



######## Combining  & CleaningData #######

Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)

#Trial_All[["Masu1"]][["raw"]]

FishID_string <- Fishlist%>% # remove non-used and bad fish
  filter(!FishID %in% c("Char1", "Masu8", 'Blank')) %>%
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13',
                        'Masu4', 'Masu11', 'Masu12', 'Masu13'))

FishID_str <- FishID_string$FishID

raw_list <- list()
for (i in 1:length(Trial_All)){
  raw_list[[i]] <- Trial_All[[i]][['raw']]
}
raw_df <- bind_rows(raw_list)


raw_df <- raw_df %>%
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13','Masu4', 
                        'Masu11', 'Masu12', 'Masu13',
                        'Blank'))


Blank_df <- raw_df %>%      # Getting blank data
  filter(FishID %in% c('Blank'))



bad_rep_list <- vector('list',18)
names(bad_rep_list) <- FishID_str
FishID_str


Masu1_rm <- data.frame(FishID = 'Masu1',
                       rep = c(12:14, 16,21,41,50:52,58))
Masu2_rm <- data.frame(FishID = 'Masu2',
                       rep = c(13, 57:63,95))
Masu3_rm <- data.frame(FishID = 'Masu3',
                       rep = c(1,13,2,3,4,44:47,5,55:75,95))
Masu5_rm <- data.frame(FishID = 'Masu5',
                       rep = c(45))
Masu6_rm <- data.frame(FishID = 'Masu6',
                       rep = c(12,45))
Masu7_rm <- data.frame(FishID = 'Masu7',
                       rep = c(4,7))
Masu9_rm <- data.frame(FishID = 'Masu9',
                       rep = c(NaN))
Masu10_rm <- data.frame(FishID = 'Masu10',
                       rep = c(1,2,24,3,73:96))
Ito1_rm <- data.frame(FishID = 'Ito1',
                      rep = c(13,37))
Ito2_rm <- data.frame(FishID = 'Ito2',
                      rep = c(NaN))
Ito3_rm <- data.frame(FishID = 'Ito3',
                      rep = c(34,51))
Ito4_rm <- data.frame(FishID = 'Ito4',
                      rep = c(1,13,14,2,25,3,44,45,47,49,5,55,57,58:64,
                       66,67,68,71,74,75,76:78,94,95))
Ito5_rm <- data.frame(FishID = 'Ito5',
                      rep = c(13,57,58,62,66,76,77,95))
Ito7_rm <- data.frame(FishID = 'Ito7',
                      rep = c(13,15,16,17,19,21:29,33,37,57,95))
Ito8_rm <- data.frame(FishID = 'Ito8',
                      rep = c(45))
Ito9_rm <- data.frame(FishID = 'Ito9',
                      rep = c(15,21,34,35:37))
Ito10_rm <- data.frame(FishID = 'Ito10',
                      rep = c(22,23,35,45))
Ito12_rm <- data.frame(FishID = 'Ito12',
                      rep = c(1,2,3,49,78,85,89,90))

Masu_rm <- rbind(Masu1_rm,Masu2_rm,Masu3_rm,Masu5_rm,Masu6_rm,
      Masu7_rm,Masu9_rm,Masu10_rm)

Ito_rm <- rbind(Ito1_rm,Ito2_rm,Ito3_rm,Ito4_rm,Ito5_rm,Ito7_rm,
                Ito8_rm,Ito9_rm,Ito10_rm,Ito12_rm)


raw_df <- anti_join(raw_df, Masu_rm, by = c('FishID', 'rep'))
raw_df <- anti_join(raw_df, Ito_rm, by = c('FishID', 'rep'))
