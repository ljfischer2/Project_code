#Script for a function to do all of the shit I need for every fish


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
