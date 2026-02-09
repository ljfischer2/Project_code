Rate.calc <- function(vol, mass, start_t, End_t,
                      FishID, Chnum, data) {
  
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
  total_rows <- nrow(data)
  num_sets   <- total_rows %/% 450
  
  Rep_TempA <- data$Temp[
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

Masu1_out <- Rate.calc(vol = 330, mass = 3.3,
                       start_t = 390, End_t = 450,
                       FishID = Masu1,
                       Chnum = "Ch1",
                       data = data1)

##################### Same function for lm ################################




Rate.calc.lm <- function(vol, mass, FishID, data) {



lm_list <- NA_real_

for (i in 1: length(FishID[['subsets']])){       #subsetting all of the measure periods
  lm_list[i] <- list(FishID[["results"]][[i]][["subsets"]][[1]])
  if (length(lm_list[[i]]) == 60){
    lm_list[[i]][["Time"]] <- seq(from = 4, to = 240, by = 4)
  } else {}
}



lmtest <- list()
lm_rate_df = data.frame(lmrate = 1:length(lm_list))
for (i in 1:length(lm_list)){
  lmtest[i] <- list(lm(lm_list[[i]][["Ch1"]] ~ lm_list[[i]][["Time"]], data = lm_list[i]))
  lm_rate_df$lmrate[i] <- data.frame(rate = lmtest[[i]][["coefficients"]][[2]])
  rate <- lm_rate_df$lmrate[[i]]
  rate <- rate * 60 #mg per L per min
  rate <- rate * (vol/1000) #remove vol, mg per min
  rate <- rate / mass #mg per g per min
  rate <- rate *1000 #mg per kg per min
  lm_rate_df$lmratemgkgmin[i] <- rate
}


## ---- temperature binning ----
total_rows <- nrow(data)
num_sets   <- total_rows %/% 450

Rep_TempA <- data$Temp[
  seq(from = 390, by = 450, length.out = num_sets)
]

n <- min(nrow(lm_rate_df), length(Rep_TempA))

lm_rate_df <- lm_rate_df[seq_len(n), ]
lm_rate_df$temp <- round(Rep_TempA[seq_len(n)])


avg_rate <- lm_rate_df %>%
  group_by(temp) %>%
  summarise(rate = mean(lm_rate_df, na.rm = TRUE)) %>%
  mutate(rate = round(rate, 3)) %>%
  ungroup()

return(list(raw = df, avg = avg_rate))
}

Rate.calc.lm(325, 3.3, Masu1, data1)
