<<<<<<< HEAD
library(ggplot2)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")
data1<-read.csv("Trial_1_Comp.csv",header=T)
data2<-read.csv("Trial_2_Comp.csv",header=T)
data3<-read.csv("Trial_3_Comp.csv",header=T)
data4<-read.csv("Trial_4_Comp.csv",header=T)

total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA, breaks = 11)
########################################
total_rows <- nrow(data2)
num_sets <- total_rows %/% 450
Rep_TempA2 <- data2$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB2 <- data2$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA2, breaks = 11)
hist(Rep_TempB2, breaks = 14)
########################################
total_rows <- nrow(data3)
num_sets <- total_rows %/% 450
Rep_TempA3 <- data3$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB3 <- data3$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA3, breaks = 11)
hist(Rep_TempB3, breaks = 14)
###########################################
total_rows <- nrow(data4)
num_sets <- total_rows %/% 450
Rep_TempA4 <- data4$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB4 <- data4$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA4, breaks = 11)
hist(Rep_TempB4, breaks = 14)

=======
library(ggplot2)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")
data1<-read.csv("Trial_1_Comp.csv",header=T)
data2<-read.csv("Trial_2_Comp.csv",header=T)
data3<-read.csv("Trial_3_Comp.csv",header=T)
data4<-read.csv("Trial_4_Comp.csv",header=T)

total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA, breaks = 11)
########################################
total_rows <- nrow(data2)
num_sets <- total_rows %/% 450
Rep_TempA2 <- data2$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB2 <- data2$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA2, breaks = 11)
hist(Rep_TempB2, breaks = 14)
########################################
total_rows <- nrow(data3)
num_sets <- total_rows %/% 450
Rep_TempA3 <- data3$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB3 <- data3$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA3, breaks = 11)
hist(Rep_TempB3, breaks = 14)
###########################################
total_rows <- nrow(data4)
num_sets <- total_rows %/% 450
Rep_TempA4 <- data4$TempA[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempB4 <- data4$TempB[seq(from = 375,  by = 450, length.out = num_sets) ]

hist(Rep_TempA4, breaks = 11)
hist(Rep_TempB4, breaks = 14)

>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
