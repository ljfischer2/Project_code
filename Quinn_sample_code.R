#Script for a function to do all of the shit I need for every fish

library(respR)
library(tidyverse)

data1<-read.csv("Trial_1_Comp.csv",header=T)
#data1 <- data1[-c(42751:42937), ]
#data1 <- data1[-c(1:10800),]
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column

#Above is the first fish (Masu1) that I wrote the code for.  I will put the original code that works for this fish at the bottom

chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column

#Next fish (Ito1) in the queue to be tested.  


# Fish and measurement phase variables:
vol <- 330 #Volume of the chamber
mass <- 3.3 #Mass of the fish in grams

#Above measurements are for Masu1

start_t <- 390 #The same for every fish.
End_t <- 450  #The same for every fish



Rate.calc <- function(vol, mass, start_t, End_t, df, FishID, Chnum, data1, avg_rate){
  
  # Fish and measurement phase variables:
  #vol <- 330 #Volume of the chamber
  #mass <- 3.3 #Mass of the fish in grams
  
  
  
  ########### Extrating DO rates#####
  
  #i = 1
  #df = data.frame(rep = 1:length(FishID[['subsets']]), #setting up the data frame.  The rep should follow exactly with the loop.
  #                DO = 1:length(FishID[['subsets']]),
  #                time = (4/1440))
  
  for (i in 1: length(FishID[['subsets']])){ #loop for converting DO
    df$rep[i] <- i #Unnecessary since it will go through each replicate sequentially, but it makes me feel better
    df$DO[i] <- FishID[["subsets"]][[i]]$Chnum[End_t] - FishID[["subsets"]][[i]]$Chnum[start_t] #delta Oxygen 
    
    #the line above is the issue.  It will not recognize the $Chnum column when I try to use it for new fish
    #The issue is that each of the chambers in the code above the function are sequential (Masu1 = Ch1, Ito1 = Ch2)
    df$rsq[i] <- FishID[["results"]][[i]][["summary"]][["rsq"]] #R-squared 
    rate <- df$DO[i] * (vol/1000)/1000
    rate <- (rate/mass)/df$time[i]
    df$rate_ggd[i] <- rate    #everything below is unit changes, comment out those that are not needed
    df$rate_ggh[i] <- rate/24
    
    
    
    
    
    
    
    
    
    df$rate_ggmin[i] <- rate/1440
    df$rate_mggh[i]  <- df$rate_ggh[i]*1000
    df$rate_mgkgh[i]  <- df$rate_mggh[i]*1000
    df$rate_mgkgmin[i]  <- df$rate_mgkgh[i]/60
  }
  #df <- df[-c(64),]
  
  
  total_rows <- nrow(data1)
  num_sets <- total_rows %/% 450
  Rep_TempA <- data1$Temp[seq(from = 375,  by = 450, length.out = num_sets) ]
  
  df$temp <- Rep_TempA_bin
  min_tempA_bin <- min(df$temp)
  max_tempA_bin <- max(df$temp)
  avg_rate <- data.frame(rate = (min_tempA_bin:max_tempA_bin))
  
  for (i in 1:length(avg_rate$rate)){
    avgdf <- df %>%
      filter(temp == min_tempA_bin + (i - 1))
    avg_rate$rate[i] <- mean(avgdf$rate_mgkgmin)
    avg_rate$temp[i] <- mean(avgdf$temp)
  }
  avg_rate$rate <- abs(avg_rate$rate)
  avg_rate$rate <- round(avg_rate$rate, digits = 3)
  
}


####Working code for Masu1 below######################################

Masu1 <- calc_rate.int(chamber1.1,
                       starts = 450, #The whole period of one flush and measure cycle
                       wait = 390, #How long the flush period is, if it comes first
                       measure = 60, #How long you measure for.
                       by = "row", #Either time (s?) or row (if row /= 1 sec)
                       pos = c(1:5)) # pos is which replicates you would like to 
# look at. the loop surrounding this is for if you want to look at each individually.  
# you would also need to specify pos = i in that case.
#}

#The function above is from RespR, I haven't tried to incorporate it in the function yet, but is necessary
#That's fine for now, I like to check each fish individually.



df = data.frame(rep = 1:length(Masu1[['subsets']]), #setting up the data frame.  The rep should follow exactly with the loop.
                DO = 1:length(Masu1[['subsets']]),
                time = (4/1440))

for (i in 1: length(Masu1[['subsets']])){ #loop for converting DO
  df$rep[i] <- i #Unnecessary since it will go through each replicate sequentially, but it makes me feel better
  df$DO[i] <- Masu1[["subsets"]][[i]]$Ch1[450] - Masu1[["subsets"]][[i]]$Ch1[390] #delta Oxygen 
  
  #In this code, specifying Ch1 works.  If I change it to Ito1 and use Ch2, it also works.
  
  
  
  
  df$rsq[i] <- Masu1[["results"]][[i]][["summary"]][["rsq"]] #R-squared 
  rate <- df$DO[i] * (vol/1000)/1000
  rate <- (rate/mass)/df$time[i]
  df$rate_ggd[i] <- rate    #everything below is unit changes, comment out those that are not needed
  df$rate_ggh[i] <- rate/24
  df$rate_ggmin[i] <- rate/1440
  df$rate_mggh[i]  <- df$rate_ggh[i]*1000
  df$rate_mgkgh[i]  <- df$rate_mggh[i]*1000
  df$rate_mgkgmin[i]  <- df$rate_mgkgh[i]/60
}
df <- df[-c(64),]


total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data1$Temp[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempA_bin <- round(Rep_TempA)
df$temp <- Rep_TempA_bin
min_tempA_bin <- min(df$temp)
max_tempA_bin <- max(df$temp)
avg_rate <- data.frame(rate = (min_tempA_bin:max_tempA_bin))

for (i in 1:length(avg_rate$rate)){
  avgdf <- df %>%
    filter(temp == min_tempA_bin + (i - 1))
  avg_rate$rate[i] <- mean(avgdf$rate_mgkgmin)
  avg_rate$temp[i] <- mean(avgdf$temp)
}
avg_rate$rate <- abs(avg_rate$rate)
avg_rate$rate <- round(avg_rate$rate, digits = 3)



