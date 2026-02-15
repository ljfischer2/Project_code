######## Combining Data #######

Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)

Trial_All[["Masu1"]][["raw"]]

FishID_string <- Fishlist%>%
  filter(!FishID %in% c("Char1", "Masu8", "Blank"))

FishID_str <- FishID_string$FishID

raw_list <- list()
for (i in 1:length(Trial_All)){
  raw_list[[i]] <- Trial_All[[i]][['raw']]
}
raw_df <- bind_rows(raw_list)

avg_rate_raw <-  raw_df%>%
  filter(rsq >0.95) %>%
  group_by(temp, Species) %>%
  summarise(rate = mean(lmratemgkgmin, na.rm = TRUE)) %>%
  mutate(rate = round(rate, 3))
  


ggplot(avg_rate_raw, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  theme_classic()
