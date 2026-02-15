######## Combining Data #######

Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)

Trial_All[["Masu1"]][["raw"]]

FishID_string <- Fishlist%>%
  filter(!FishID %in% c('Masu1', "Char1", "Masu8", "Blank"))

FishID_str <- FishID_string$FishID

raw_list <- list()
for (i in 1:length(Trial_All)){
  raw_list[[i]] <- Trial_All[[i]][['raw']]
}
raw_df <- bind_rows(raw_list)

