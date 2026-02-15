rm(list = ls())
#install.packages('respR')
library(respR)
library(tidyverse)

rm(list = ls())
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")

data1<-read.csv("Trial_1_Comp.csv",header=T)
#data1 <- data1[-c(42751:42937), ]
#data1 <- data1[-c(1:10800),]
chamber1.1<-inspect(data1, time = 1, oxygen = 5)#time is 1st column, oxygen is 5th column
chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column

# Fish and measurement phase variables:
vol <- 330 #Volume of the chamber
mass <- 3.3 #Mass of the fish in grams

start_t <- 390 #these values will be different than the ones 
# in the the calc.rate.int function.  
End_t <- 450

############Subsetting Replicates ######
#for (i in 1:64) {
  Masu1 <- calc_rate.int(chamber1.1,
                         starts = 450, #The whole period of one flush and measure cycle
                         wait = 390, #How long the flush period is, if it comes first
                         measure = 60, #How long you measure for.
                         by = "row", #Either time (s?) or row (if row /= 1 sec)
                         pos = c(1:5)) # pos is which replicates you would like to 
# look at. the loop surrounding this is for if you want to look at each individually.  
# you would also need to specify pos = i in that case.
#}

########### Extrating DO rates#####

#i = 1
df = data.frame(rep = 1:length(Masu1[['subsets']]), #setting up the data frame.  The rep should follow exactly with the loop.
  DO = 1:length(Masu1[['subsets']]),
                time = (4/1440))

for (i in 1: length(Masu1[['subsets']])){ #loop for converting DO
  df$rep[i] <- i #Unnecessary since it will go through each replicate sequentially, but it makes me feel better
  df$DO[i] <- Masu1[["subsets"]][[i]]$Ch1[450] - Masu1[["subsets"]][[i]]$Ch1[390] #delta Oxygen 
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


ggplot(avg_rate, aes(x = temp, y = rate, color = temp)) + 
  geom_point(size = 6) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  scale_color_distiller(palette = "RdYlBu")





i = 1

############# Trial loop for using lm ######################## Trial loop for using lm ######################## Trial loop for using lm ################
lm_list <- NA_real_
#lm_list[1] <- list(Masu1[["results"]][[1]][["subsets"]][[1]])


for (i in 1: length(Masu1[['subsets']])){       #subsetting all of the measure periods
  lm_list[i] <- list(Masu1[["results"]][[i]][["subsets"]][[1]])
  if (length(lm_list[[i]][["Time"]]) == 60){
    lm_list[[i]][["Time"]] <- seq(from = 4, to = 240, by = 4)
  } else {}
}


#lm_list <- lm_list[-c(64)]

## Temp Stuff
total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data1$Temp[seq(from = 375,  by = 450, length.out = num_sets) ]
Rep_TempA_bin <- round(Rep_TempA)


## Into data frame and rate convert
lmtest <- list()
lm_rate_df = data.frame(lmrate = 1:length(lm_list))

for (i in 1:length(lm_list)){
lmtest[i] <- list(lm(lm_list[[i]][['Ch1']] ~ lm_list[[i]][["Time"]], data = lm_list[i]))
lm_rate_df$temp[i] <- Rep_TempA_bin[i]
lm_rate_df$lmrate[i] <- data.frame(rate = lmtest[[i]][["coefficients"]][[2]])


rate <- lm_rate_df$lmrate[[i]]
rate <- rate * 60 #mg per L per min
rate <- rate * (325/1000) #remove vol, mg per min
rate <- rate / 3.3 #mg per g per min
rate <- rate *1000 #mg per kg per min
lm_rate_df$lmratemgkgmin[i] <- abs(rate)
lm_rate_df$rsq[i] <- summary(lmtest[[i]])$r.squared
lm_rate_df$sd[i] <- sigma(lmtest[[i]])
}

ggplot(lm_rate_df, aes(temp, lmratemgkgmin, color = rsq)) +
  geom_point(position = 'jitter') +
  scale_color_gradient(
    low = "red",
    high = "blue"
  )

ggplot(lm_rate_df, aes(temp, lmratemgkgmin, color = rsq)) +
  geom_point(size = 2, 
             position = position_jitter(
               width = 0.2,
               height = 0)) +
  scale_color_viridis_c() +
  theme_minimal()




min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)
avg_rate <- data.frame(rate = (min_tempA_bin:max_tempA_bin))


for (i in 1:length(avg_rate$rate)){
  avgdf <- lm_rate_df %>%
    filter(temp == min_tempA_bin + (i - 1))
  avg_rate$rate[i] <- mean(avgdf$lmratemgkgmin)
  avg_rate$temp[i] <- mean(avgdf$temp)
}
avg_rate$rate <- abs(avg_rate$rate)
avg_rate$rate <- round(avg_rate$rate, digits = 3)

ggplot(avg_rate, aes(x = temp, y = rate)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Masu1 respiration rate (lm method)",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")
  ) +
  theme_classic()


############## Debug zone #####


