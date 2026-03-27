Rate.calc.lm <- function(vol, mass, fishlist, FishID, Chnum, numfish, data) {
  
  
  
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
    rate <- rate * (vol/1000) #remove vol, mg per min
    lm_rate_df$mass_corrected[i] <- abs(rate) #needs to be corrected for units
    lm_rate_df$mass_corrected[i] <- lm_rate_df$mass_corrected[i] * 1.44
    rate <- rate / mass #mg per g per min 
    rate <- rate *1000 #mg per kg per min
    lm_rate_df$lmratemgkgmin[i] <- abs(rate)
    lm_rate_df$rsq[i] <- summary(lmtest[[i]])$r.squared
    lm_rate_df$sd[i] <- sigma(lmtest[[i]])
    lm_rate_df$rep[i] <- i
    lm_rate_df$mass[i] <- mass
    lm_rate_df$vol[i] <- vol
  }
  
  
  ## ---- temperature binning ----
  total_rows <- nrow(data)
  num_sets   <- total_rows %/% 450
  
  if (numfish == 4){         #FishID is going to be length 7 because it is not the fishlist
    Rep_TempA <- data$Temp[
      seq(from = 390, by = 450, length.out = num_sets)
    ]
    Rep_TempA_end <- data$Temp[
      seq(from = 450, by = 450, length.out = num_sets)
    ]
    
    n <- min(nrow(lm_rate_df), length(Rep_TempA))
    
    lm_rate_df <- lm_rate_df[seq_len(n), ]
    lm_rate_df$temp <- round(Rep_TempA[seq_len(n)])
    lm_rate_df$deltatemp <- abs(Rep_TempA - Rep_TempA_end)
    
    
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
      Rep_TempA_end <- data$TempA[
        seq(from = 450, by = 450, length.out = num_sets)
      ]
      
      n <- min(nrow(lm_rate_df), length(Rep_TempA))
      
      lm_rate_df <- lm_rate_df[seq_len(n), ]
      lm_rate_df$temp <- round(Rep_TempA[seq_len(n)])
      lm_rate_df$deltatemp <- abs(Rep_TempA - Rep_TempA_end)
      
      
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
      Rep_TempB_end <- data$TempB[
        seq(from = 450, by = 450, length.out = num_sets)
      ]
      
      n <- min(nrow(lm_rate_df), length(Rep_TempB))
      
      lm_rate_df <- lm_rate_df[seq_len(n), ]
      lm_rate_df$temp <- round(Rep_TempB[seq_len(n)])
      lm_rate_df$deltatemp <- abs(Rep_TempB - Rep_TempB_end)
      
      
      avg_rate <- lm_rate_df %>%
        group_by(temp) %>%
        summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
        mutate(rate = round(rate, 3)) %>%
        ungroup()
      
      return(list(raw = lm_rate_df, avg = avg_rate))
    } else {}
  }
  
}
