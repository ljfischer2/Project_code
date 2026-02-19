######## Combining Data #######

Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)

#Trial_All[["Masu1"]][["raw"]]

FishID_string <- Fishlist%>%
  filter(!FishID %in% c("Char1", "Masu8", 'Blank'))

FishID_str <- FishID_string$FishID

raw_list <- list()
for (i in 1:length(Trial_All)){
  raw_list[[i]] <- Trial_All[[i]][['raw']]
}
raw_df <- bind_rows(raw_list)


##### Filtering  & Plotting by species######





avg_rate_raw <-  raw_df%>%
  filter(rsq >0.95) %>%
  group_by(temp, Species) %>%
  summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
  mutate(rate = round(rate, 3))
  


ggplot(avg_rate_raw, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "rsq = 0.95",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  theme_classic()

###### rates less than 10, median


submedrate <- raw_df%>%
  filter(lmratemgkgmin < 10) %>%
  filter(rsq > 0.95) %>%
  group_by(temp, Species) %>%
  summarise(rate = median(lmratemgkgmin, na.rm = TRUE)) %>%
  mutate(rate = round(rate, 3))

  ggplot(submedrate, aes(x = temp, y = rate, color = Species)) +
    geom_point(size = 3) +
    geom_line() +
    labs(
      title = "rsq = 0.95, rate < 10",
      x = "Temperature (°C)",
      y = expression("Med Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    theme_classic()
######## rates less than 10, mean
  
  subavgrate <- raw_df%>%
    filter(lmratemgkgmin < 10) %>%
    filter(rsq > 0.95) %>%
    group_by(temp, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
    mutate(rate = round(rate, 3))
  
  ggplot(subavgrate, aes(x = temp, y = rate, color = Species)) +
    geom_point(size = 3) +
    geom_line() +
    labs(
      title = "rsq = 0.95, rate < 10",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    theme_classic()
################ Grouping by FishID

  avg_rate_raw <-  raw_df%>%
    filter(rsq >0.90) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                          'Masu11', 'Masu10', 'Ito11')) %>% # bad apples
    group_by(temp,FishID) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd = sd(lmratemgkgmin, na.rm = T)) %>%
    mutate(rate = round(rate, 3))
  
  avg_rate_raw <- raw_df %>%
    filter(rsq > 0.90) %>%
    filter(!FishID %in% c("Masu13", "Masu12", "Ito2",
                          "Masu11", "Masu10", "Ito11")) %>%
    group_by(temp, FishID, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd   = sd(lmratemgkgmin, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(rate = round(rate, 3))
  
avg_norsqfilter <- raw_df %>%
    filter(!FishID %in% c("Masu13", "Masu12", "Ito2",
                          "Masu11", "Masu10", "Ito11")) %>%
    group_by(temp, FishID, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd   = sd(lmratemgkgmin, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(rate = round(rate, 3))
  
  
  

########## plot with rsq below 90 in red
  ggplot(avg_rate_raw, aes(x = temp, y = rate)) +
    geom_point(size = 2) +
    geom_point(data = avg_norsqfilter, aes(x = temp, y = rate),
               color = 'red', alpha = 0.3) +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd)) +
    geom_errorbar(data = avg_norsqfilter, aes(ymin = rate - sd,
                      ymax = rate + sd),
                  color = 'red', alpha = 0.6) +
    labs(
      title = "rsq = 0.95, rate < 10",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    facet_wrap(~FishID) +
    theme_classic()

  
  
  
  

#length(unique(subrsq_rm$temp))  #count number of unique temps
subrsq_rm90 <- raw_df %>%
    filter(rsq > 0.90) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2')) %>% # bad apples
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
  )


pos <- position_dodge(width = 0.5)
ggplot(subrsq_rm90, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3, position = pos) +
  geom_errorbar(aes(ymin = rate - sd,
                    ymax = rate + sd),
                position = pos) +
  #geom_line() +
  #geom_smooth(method = 'lm') +
  labs(
    title = "rsq = 0.95, rate < 10",
    x = "Temperature (°C)",
    y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  theme_classic()





  group_by(Species, temp) %>%
  summarise(
    n = n(),
    rate = mean(lmratemgkgmin, na.rm = TRUE),
    sd = sd(lmratemgkgmin, na.rm = T),
    .groups = "drop"
  )




  subrsq_rm95b <- raw_df %>%
    filter(rsq > 0.95) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2')) %>% # bad apples
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
    )

