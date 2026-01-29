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
vol <- 330 #Volume of the chamber
mass <- 3.3 #Mass of the fish in grams
############Subsetting Replicates ######
#for (i in 1:64) {
  Masu1 <- calc_rate.int(chamber1.1,
                         starts = 450,
                         wait = 390,
                         measure = 61,
                         by = "row",
                         pos = c(1:5))
#}

########### Extrating DO rates#####

i = 1
df = data.frame(rep = 1:length(Masu1[['subsets']]),
  DO = 1:length(Masu1[['subsets']]),
                time = (4/1440))
for (i in 1: length(Masu1[['subsets']])){
  df$rep[i] <- i #replicate
  df$DO[i] <- Masu1[["subsets"]][[i]]$Ch1[450] - Masu1[["subsets"]][[i]]$Ch1[390] #delta Oxygen 
  df$rsq[i] <- Masu1[["results"]][[i]][["summary"]][["rsq"]] #R-squared 
  rate <- df$DO[i] * (vol/1000)/1000
  rate <- (rate/mass)/df$time[i]
  df$rate_ggd[i] <- rate    #everything below is unit changes
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


