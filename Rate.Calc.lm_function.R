
Rate.calc.lm <- function(vol, mass, FishID, Chnum, data) {



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
  rate <- rate / mass #mg per g per min
  rate <- rate *1000 #mg per kg per min
  lm_rate_df$lmratemgkgmin[i] <- abs(rate)
  lm_rate_df$rsq[i] <- summary(lmtest[[i]])$r.squared
  lm_rate_df$sd[i] <- sigma(lmtest[[i]])
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
  summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
  mutate(rate = round(rate, 3)) %>%
  ungroup()

return(list(raw = lm_rate_df, avg = avg_rate))
}

########### Function Testing ############
Masu1 <- calc_rate.int(chamber1.1, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito1  <- calc_rate.int(chamber1.2, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito2  <- calc_rate.int(chamber1.3, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)

Ito3  <- calc_rate.int(chamber1.4, starts = 450, wait = 390,
                       measure = 60, by = "row", pos = 1:5)





Masu1_lm_out <- Rate.calc.lm(325, 3.3, Masu1, Chnum  = 'Ch1', data1)
Ito1_lm_out <- Rate.calc.lm(325, 5.5, Ito1, 'Ch2', data1)
Ito2_lm_out <- Rate.calc.lm(325, 11.8, Ito2, 'Ch3', data1)
Ito3_lm_out <- Rate.calc.lm(325, 5.6, Ito3, 'Ch4', data1)
