#Script for a function to do all of the shit I need for every fish


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



Rate.calc(vol = vol,
          mass = 5.5,
          start_t = 450,
          End_t = 390,
          df = dfito1,
          FishID = Ito1,
          data1 = data1,
          avg_rate = Ito1_avg)






i = 6

for (i in 1: length(Ito1[['subsets']])){ #loop for converting DO
  dfito1$rep[i] <- i #Unnecessary since it will go through each replicate sequentially, but it makes me feel better
  dfito1$DO[i] <- Ito1[["subsets"]][[i]]$Ch2[End_t] - Ito1[["subsets"]][[i]]$Ch2[start_t] #delta Oxygen 
  dfito1$rsq[i] <- Ito1[["results"]][[i]][["summary"]][["rsq"]] #R-squared 
  rate <- dfito1$DO[i] * (vol/1000)/1000
  rate <- (rate/mass)/dfito1$time[i]
  dfito1$rate_ggd[i] <- rate    #everything below is unit changes, comment out those that are not needed
  dfito1$rate_ggh[i] <- rate/24
  dfito1$rate_ggmin[i] <- rate/1440
  dfito1$rate_mggh[i]  <- dfito1$rate_ggh[i]*1000
  dfito1$rate_mgkgh[i]  <- dfito1$rate_mggh[i]*1000
  dfito1$rate_mgkgmin[i]  <- dfito1$rate_mgkgh[i]/60
}
