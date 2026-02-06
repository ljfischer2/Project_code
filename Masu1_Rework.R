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

############# Trial loop for using lm ######################## Trial loop for using lm ######################## Trial loop for using lm ################
lm_list <- NULL
#lm_list[1] <- list(Masu1[["results"]][[1]][["subsets"]][[1]])

i <- 1
for (i in 1: length(Masu1[['subsets']])){       #subsetting all of the measure periods
  lm_list[i] <- list(Masu1[["results"]][[i]][["subsets"]][[1]])
  lm_list[[i]][["Time"]] <- seq(from = 4, to = 240, by = 4)
}
lmtest <- list()
for (i in 1:length(lm_list)){
lmtest[i] <- list(lm(lm_list[[i]][["Ch1"]] ~ lm_list[[i]][["Time"]], data = lm_list[i]))
#plot(lmtest)
}









seq(from = 4, to = 240, by = 4)
for (i in 1: length(Masu1[['subsets']])){ #loop for converting DO
  lm_df$rep[i] <- i #Unnecessary since it will go through each replicate sequentially, but it makes me feel better
  #lm_df$DO[i] <- Masu1[["subsets"]][[i]]$Ch1[450] - Masu1[["subsets"]][[i]]$Ch1[390] #delta Oxygen 
  #lm_df$rsq[i] <- Masu1[["results"]][[i]][["summary"]][["rsq"]] #R-squared }
  #rate <- df$DO[i] * (vol/1000)/1000
  #rate <- (rate/mass)/df$time[i]
  
  
  df$rate_ggd[i] <- rate    #everything below is unit changes, comment out those that are not needed
  df$rate_ggh[i] <- rate/24
  df$rate_ggmin[i] <- rate/1440
  df$rate_mggh[i]  <- df$rate_ggh[i]*1000
  df$rate_mgkgh[i]  <- df$rate_mggh[i]*1000
  df$rate_mgkgmin[i]  <- df$rate_mgkgh[i]/60
}






##########

#What if we select the temp at the start of every trial?
total_rows <- nrow(data1)
num_sets <- total_rows %/% 450
Rep_TempA <- data1$Temp[seq(from = 375,  by = 450, length.out = num_sets) ]

#hist(Rep_TempA, breaks = 11)

Rep_TempA_bin <- round(Rep_TempA)
min_tempA_bin <- min(Rep_TempA_bin)
max_tempA_bin <- max(Rep_TempA_bin)


#plot(convert_Ito4, type = "rate")
#plot(convert_Ito4, type = "overlap")



#######################################################
#Masu 1
######################################################
#### llx start ###
dfMasu1 <- data.frame(temp_bin = numeric(),
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
    SMRMasu1_T <- convert_Masu1 |>
      select_rate(method = "manual", n = n) |>
      summary() |>
      mean()
    
    # Safely extract values
    temp_val <- if (!is.na(Rep_TempA[n[o]])) Rep_TempA[n[o]] else NA
    rep_val <- if (!is.null(SMRMasu1_T$rate.output)) SMRMasu1_T$summary$rep[o] else NA
    rate_val <- if (!is.null(SMRMasu1_T$rate.output) && length(SMRMasu1_T$rate.output) >= o) {
      SMRMasu1_T$rate.output[o]
    } else {
      NA
    }
    
    dfMasu1 <- rbind(dfMasu1, data.frame(
      temp_bin = min_tempA_bin + i - 1,
      temp = temp_val,
      rate_output = rate_val,
      trial = 1,
      rep = rep_val
    ))
  }
}


View(dfMasu1)
dfMasu1$FishID <- "Masu1"
dfMasu1$Species <- "Masu"
### llx end ####

posrate <- abs(dfMasu1$rate_output)
plot(posrate)
ggplot(data = dfMasu1, aes(x = temp, y = rate_output)) + 
  geom_point() +
  labs(x = "Temp",
       y = 'Oxygen Consumption(g O2·g−1·day−1)',
       title  = "Oxygen Consumption Rate of Masu 1") + 
  scale_color_manual(values = c('#1b9e77'))

mean(dfMasu1$rate_output)
write.csv(dfMasu1, file = "Fish_Reps/Masu_1_Reps.csv" )

















chamber1.2<-inspect(data1, time = 1, oxygen = 6)#time is 1st column, oxygen is 6th column

Ito1 <- calc_rate.int(chamber1.2,
                      starts = 450,
                      wait = 390,
                      measure = 75,
                      by = "row",
                      pos = c(1:5))

dfito1 <- data.frame(rep = 1:length(Ito1[['subsets']]), #setting up the data frame.  The rep should follow exactly with the loop.
                                DO = 1:length(Ito1[['subsets']]),
                                time = (4/1440))

Rate.calc(vol = vol,
          mass = 5.5,
          start_t = 390,
          End_t = 450,
          df = dfito1,
          FishID = Ito1,
          Chnum = Ch2,
          data1 = data1,
          avg_rate = Ito1_avg)

