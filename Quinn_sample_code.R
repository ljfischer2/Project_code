#Script for a function to do all of the shit I need for every fish
#What fun!

library(respR)
library(tidyverse)

data1<-read.csv("Trial_1_Comp.csv",header=T) #Csv of raw O2 measurements
#data1 <- data1[-c(42751:42937), ]
#data1 <- data1[-c(1:10800),]
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column

#Above is the first fish (Masu1) that I wrote the code for.  I will put the original code that works for this fish at the bottom.
#You will need to run the first part of the Masu1 code since it creates the FishID

chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column


chamber1.3<-inspect(data1, time = 1, oxygen = 7)#time is 1st column, oxygen is 7th column


chamber1.4<-inspect(data1, time = 1, oxygen = 8)#time is 1st column, oxygen is 8th column

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


####Q stuff####

str(data1)
summary(data1)

#I'm going to make this a function that can be applied to each fish
#Main issue is that $Chnum is being interpreted by R incorrectly above - need [[]] not $
#I think the below is correct - but you'll want to double check because you're more familiar with the dataset

Rate.calc <- function(vol, mass, start_t, End_t,
                      FishID, Chnum, data1) {
  
  df <- data.frame(
    rep  = seq_along(FishID$subsets),
    DO   = NA_real_,
    rsq  = NA_real_,
    time = 4 / 1440
  )
  
  for (i in seq_along(FishID$subsets)) {
    
    df$DO[i] <-
      FishID$subsets[[i]][[Chnum]][End_t] -
      FishID$subsets[[i]][[Chnum]][start_t]
    #Fixing your issue in the above lines
    df$rsq[i] <-
      FishID$results[[i]]$summary$rsq
    
    rate <- df$DO[i] * (vol / 1000) / 1000
    rate <- (rate / mass) / df$time[i]
    
    df$rate_mgkgmin[i] <- abs(rate / 24 * 1000 * 1000 / 60)
  }
  
  #Triple check my calcs above - not as familiar with these
  
  ## ---- temperature binning ----
  total_rows <- nrow(data1)
  num_sets   <- total_rows %/% 450
  
  Rep_TempA <- data1$Temp[
    seq(from = 390, by = 450, length.out = num_sets)
  ]
  
  n <- min(nrow(df), length(Rep_TempA))
  
  df <- df[seq_len(n), ]
  df$temp <- round(Rep_TempA[seq_len(n)])
  
  
  avg_rate <- df %>%
    group_by(temp) %>%
    summarise(rate = mean(rate_mgkgmin, na.rm = TRUE)) %>%
    mutate(rate = round(rate, 3)) %>%
    ungroup()
  
  return(list(raw = df, avg = avg_rate))
}

#Example use for this data: 
Masu1 <- calc_rate.int(chamber1.1, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito1  <- calc_rate.int(chamber1.2, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito2  <- calc_rate.int(chamber1.3, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito3  <- calc_rate.int(chamber1.4, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

#I think I interpreted the above correctly

Masu1_out <- Rate.calc(vol = 330, mass = 3.3,
                      start_t = 390, End_t = 450,
                      FishID = Masu1,
                      Chnum = "Ch1",
                      data1 = data1)

Ito1_out  <- Rate.calc(vol = 330, mass = 5.5,
                      start_t = 390, End_t = 450,
                      FishID = Ito1,
                      Chnum = "Ch2",
                      data1 = data1)


Ito2_out  <- Rate.calc(vol = 330, mass = 11.8,
                      start_t = 390, End_t = 450,
                      FishID = Ito2,
                      Chnum = "Ch3",
                      data1 = data1)


Ito3_out  <- Rate.calc(vol = 330, mass = 5.6,
                       start_t = 390, End_t = 450,
                       FishID = Ito3,
                       Chnum = "Ch4",
                       data1 = data1)

#There is some mismatch in the dataset - one respiration replicate does not have temp
#Just a warning! I hard coded initially but changed that to read sequences 

#Some simple example figs 

ggplot(Masu1_out$avg, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Masu1 respiration rate (diff method)",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")
  ) +
  theme_classic()

ggplot(Ito1_out$avg, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Ito1 respiration rate",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")
  ) +
  theme_classic()

ggplot(Ito2_out$avg, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Ito2 respiration rate",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")
  ) +
  theme_classic()

ggplot(Ito3_out$avg, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Ito3 respiration rate",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")
  ) +
  theme_classic()



#Raw stuff for variability 
ggplot(Masu1_out$raw, aes(x = temp, y = rate_mgkgmin)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Masu1 replicate respiration rates",
    x = "Temperature (°C)",
    y = expression("mg O"[2]*" kg"^{-1}*" min"^{-1})
  ) +
  theme_classic()

#plot them on the same 
Masu1_avg <- Masu1_out$avg %>% mutate(Fish = "Masu1")
Ito1_avg  <- Ito1_out$avg  %>% mutate(Fish = "Ito1")
Ito2_avg  <- Ito2_out$avg  %>% mutate(Fish = "Ito2")
Ito3_avg  <- Ito3_out$avg  %>% mutate(Fish = "Ito3")

plot_df <- bind_rows(Masu1_avg, Ito1_avg, Ito2_avg, Ito3_avg)

ggplot(plot_df, aes(x = temp, y = rate, color = Fish)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  theme_classic()

#Scale up to many fish
ggplot(plot_df, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Fish) +
  theme_classic()

#SE or SD with temp option
Masu1_se <- Masu1_out$raw %>%
  group_by(temp) %>%
  summarise(
    mean_rate = mean(rate_mgkgmin),
    se = sd(rate_mgkgmin) / sqrt(n())
  )

ggplot(Masu1_se, aes(x = temp, y = mean_rate)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_rate - se,
                  ymax = mean_rate + se),
              alpha = 0.2) +
  theme_classic()

#Again, make sure to triple check everything, I think I'm interpreting the dataset correctly, but it's possible that something is wonky

#you don't have to use the function above, I believe you can fix the issue in your for loop by double checking where you use $ and using [[]] for chamber numbers 


