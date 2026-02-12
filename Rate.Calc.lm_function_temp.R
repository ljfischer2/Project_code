Rate.calc.lm <- function(fishlist, FishID, Chnum, numfish, data) {
  
  
  
  lm_list <- NA_real_
  
  for (i in 1: length(FishID[['subsets']])){       #subsetting all of the measure periods
    lm_list[i] <- list(FishID[["results"]][[i]][["subsets"]][[1]])
    if (length(lm_list[[i]][['Time']]) == 60){
      lm_list[[i]][["Time"]] <- seq(from = 4, to = 240, by = 4)
    } else {}
  }
  
  
  
  lmtest <- list()
  lm_rate_df = data.frame(lmrate = 1:length(lm_list))
  for (i in 1:length(lm_list)){
    lmtest[i] <- list(lm(lm_list[[i]][[Chnum]] ~
                           lm_list[[i]][["Time"]], data = lm_list[i]))
    lm_rate_df$lmrate[i] <- lmtest[[i]][["coefficients"]][[2]]
    
    
    rate <- lm_rate_df$lmrate[[i]]
    rate <- rate * 60 #mg per L per min
    rate <- rate * (fishlist$vol/1000) #remove vol, mg per min
    rate <- rate / fishlist$mass #mg per g per min
    rate <- rate *1000 #mg per kg per min
    lm_rate_df$lmratemgkgmin[i] <- abs(rate)
    lm_rate_df$rsq[i] <- summary(lmtest[[i]])$r.squared
    lm_rate_df$sd[i] <- sigma(lmtest[[i]])
  }
  
  
  ## ---- temperature binning ----
  total_rows <- nrow(data)
  num_sets   <- total_rows %/% 450
  
  if (numfish == 4){         #FishID is going to be length 7 because it is not the fishlist
    Rep_TempA <- data$Temp[
      seq(from = 390, by = 450, length.out = num_sets)
    ]
    
    n <- min(nrow(lm_rate_df), length(Rep_TempA))
    
    lm_rate_df <- lm_rate_df[seq_len(n), ]
    lm_rate_df$temp <- round(Rep_TempA[seq_len(n)])
    
    
    avg_rate <- lm_rate_df %>%
      group_by(temp) %>%
      summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
      mutate(rate = round(rate, 3)) %>%
      ungroup()
    
    return(list(raw = lm_rate_df, avg = avg_rate))
  } else if (numfish > 4){
    if (any(fishlist$FSID %in% c(1:4))){
      Rep_TempA <- data$TempA[
        seq(from = 390, by = 450, length.out = num_sets)
      ]
      
      n <- min(nrow(lm_rate_df), length(Rep_TempA))
      
      lm_rate_df <- lm_rate_df[seq_len(n), ]
      lm_rate_df$temp <- round(Rep_TempA[seq_len(n)])
      
      
      avg_rate <- lm_rate_df %>%
        group_by(temp) %>%
        summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
        mutate(rate = round(rate, 3)) %>%
        ungroup()
      
      return(list(raw = lm_rate_df, avg = avg_rate))
    } else if (any(fishlist$FSID %in% c(5:8))){
      Rep_TempB <- data$TempB[
        seq(from = 390, by = 450, length.out = num_sets)
      ]
      
      n <- min(nrow(lm_rate_df), length(Rep_TempB))
      
      lm_rate_df <- lm_rate_df[seq_len(n), ]
      lm_rate_df$temp <- round(Rep_TempB[seq_len(n)])
      
      
      avg_rate <- lm_rate_df %>%
        group_by(temp) %>%
        summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
        mutate(rate = round(rate, 3)) %>%
        ungroup()
      
      return(list(raw = lm_rate_df, avg = avg_rate))
    } else {}
  }
  
}