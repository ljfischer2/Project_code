######## Combining  & CleaningData #######

Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)

#Trial_All[["Masu1"]][["raw"]]

FishID_string <- Fishlist%>% # remove non-used and bad fish
  filter(!FishID %in% c("Char1", "Masu8", 'Blank')) %>%
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13',
                        'Masu4', 'Masu11', 'Masu12', 'Masu13'))

FishID_str <- FishID_string$FishID

raw_list <- list()
for (i in 1:length(Trial_All)){
  raw_list[[i]] <- Trial_All[[i]][['raw']]
}
raw_df <- bind_rows(raw_list)


raw_df <- raw_df %>%
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13','Masu4', 
                        'Masu11', 'Masu12', 'Masu13',
                        'Blank'))


Blank_df <- raw_df %>%      # Getting blank data
  filter(FishID %in% c('Blank'))



bad_rep_list <- vector('list',18)
names(bad_rep_list) <- FishID_str
FishID_str


Masu1_rm <- data.frame(FishID = 'Masu1',
                       rep = c(12:14, 16,21,41,50:52,58))
Masu2_rm <- data.frame(FishID = 'Masu2',
                       rep = c(13, 57:63,95))
Masu3_rm <- data.frame(FishID = 'Masu3',
                       rep = c(1,13,2,3,4,44:47,5,55:75,95))
Masu5_rm <- data.frame(FishID = 'Masu5',
                       rep = c(45))
Masu6_rm <- data.frame(FishID = 'Masu6',
                       rep = c(12,45))
Masu7_rm <- data.frame(FishID = 'Masu7',
                       rep = c(4,7))
Masu9_rm <- data.frame(FishID = 'Masu9',
                       rep = c(NaN))
Masu10_rm <- data.frame(FishID = 'Masu10',
                        rep = c(1,2,24,3,73:96))
Ito1_rm <- data.frame(FishID = 'Ito1',
                      rep = c(13,37))
Ito2_rm <- data.frame(FishID = 'Ito2',
                      rep = c(NaN))
Ito3_rm <- data.frame(FishID = 'Ito3',
                      rep = c(34,51))
Ito4_rm <- data.frame(FishID = 'Ito4',
                      rep = c(1,13,14,2,25,3,44,45,47,49,5,55,57,58:64,
                              66,67,68,71,74,75,76:78,94,95))
Ito5_rm <- data.frame(FishID = 'Ito5',
                      rep = c(13,57,58,62,66,76,77,95))
Ito7_rm <- data.frame(FishID = 'Ito7',
                      rep = c(13,15,16,17,19,21:29,33,37,57,95))
Ito8_rm <- data.frame(FishID = 'Ito8',
                      rep = c(45))
Ito9_rm <- data.frame(FishID = 'Ito9',
                      rep = c(15,21,34,35:37))
Ito10_rm <- data.frame(FishID = 'Ito10',
                       rep = c(22,23,35,45))
Ito12_rm <- data.frame(FishID = 'Ito12',
                       rep = c(1,2,3,49,78,85,89,90))

Masu_rm <- rbind(Masu1_rm,Masu2_rm,Masu3_rm,Masu5_rm,Masu6_rm,
                 Masu7_rm,Masu9_rm,Masu10_rm)

Ito_rm <- rbind(Ito1_rm,Ito2_rm,Ito3_rm,Ito4_rm,Ito5_rm,Ito7_rm,
                Ito8_rm,Ito9_rm,Ito10_rm,Ito12_rm)


raw_df <- anti_join(raw_df, Masu_rm, by = c('FishID', 'rep'))
raw_df <- anti_join(raw_df, Ito_rm, by = c('FishID', 'rep'))
