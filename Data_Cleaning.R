######## Combining  & CleaningData #######

#Trial_All <- c(Trial_1, Trial_2, Trial_3, Trial_4)
Trial_All <- c(Trial_1, Trial_2, Trial_3)
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
                        'Masu11', 'Masu12', 'Masu13', 'Blank'))


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





######## Prepping Columns

data_2023 <- read.csv('Resp_23_data.csv')

head(data_2023)
head(raw_df)

raw_df <- raw_df %>% #10 of them colander bois
  select(FishID, Species, temp, lmrate, lmratemgkgmin, rep, trial, mass, vol,
         rsq, sd, deltatemp)

data_2023 <- data_2023 %>%   # 9 ariablse
  select(FishID, Species, temp_bin, rate_output, rate_final, rep,
         trial, Method, mass, vol, temp)

colnames(data_2023) <- c('FishID', 'Species', 'temp_bin', 'rate', 'rate_final',
                         'rep', 'trial', 'Method', 'mass', 'vol', 'temp_exact')

colnames(raw_df) <- c('FishID', 'Species', 'temp_bin', 'rate', 'rate_final',
                      'rep', 'trial', 'mass', 'vol', 'rsq', 'sd', 'deltatemp')

########## Actual Join (Only run if you intend to join, as you will lose some variables)

#;(
raw_df_join <- raw_df %>%
  select(FishID, Species, temp_bin, rate, rate_final,
         rep, mass, vol, trial)
raw_df_join$Method <- 'Intermittent'

data_2023_join <- data_2023 %>%
  select(FishID, Species, temp_bin, rate, rate_final,
         rep, mass, vol, trial, Method) %>%
  filter(!FishID %in% 'Ito15')

two_year_data <- rbind(raw_df_join, data_2023_join)
two_year_data$lograte <- log(two_year_data$rate_final)

# Plotting the log transformed data
plot_data <- two_year_data %>%
  filter(Method == 'Intermittent') %>%
  group_by(Species, temp_bin) %>%
  summarize(lograte = mean(lograte, na.rm = T),
            logsd = sd(lograte, na.rm = T))

ggplot(plot_data, aes(x = temp_bin, y = lograte, color = Species)) + 
  geom_point(size = 3) + 
  xlim(10,25) + 
  ylim(0, 2) +
  labs(title = 'Log Rate Int Only')



















##

Blank_df <- raw_df %>%      # Getting blank data
  filter(FishID %in% c('Blank'))



bad_rep_list <- vector('list',18)
names(bad_rep_list) <- FishID_str
FishID_str




############# Background ################
########### Background Rate checking
ggplot(Blank_df, aes(x = rep, y = lmratemgkgmin)) + 
  geom_point()

ggplot(Blank_df, aes(x = rep, y = temp)) + 
  geom_point()


################# Subtracting background from trials

# corrected is (oxy solu(ratew/ * volw/ - ratew/o * vol w/o)) / fish mass

trial4Ito_df <- raw_df %>%
  filter(trial == 4) %>%
  filter(Species == 'Ito')

trial4Masu_df <- raw_df %>%
  filter(trial == 4) %>%
  filter(Species == 'Masu')


CRrate <- Blank_df[(Blank_df$rep %in% trial4Masu_df$rep),] # filters only the reps same as Masu

for (i in 1:length(trial4Masu_df$rsq)){
  trial4Masu_df$bkgcorrect[i] <- trial4Masu_df$lmrate[i] - CRrate$lmrate[i]
  rate <- trial4Masu_df$bkgcorrect[i]
  rate <- trial4Masu_df$bkgcorrect[i]
  rate <- rate * 60 #mg per L per min
  rate <- rate * (350/1000) #remove vol, mg per min
  rate <- rate / 4.2 #mg per g per min
  rate <- rate *1000 #mg per kg per min
  trial4Masu_df$bkgcorrect_mgkgmin[i] <- abs(rate)
}

CRrate <- Blank_df[(Blank_df$rep %in% trial4Ito_df$rep),] # filters only the reps same as Masu

for (i in 1:length(trial4Ito_df$rsq)){
  trial4Ito_df$bkgcorrect[i] <- trial4Ito_df$lmrate[i] - CRrate$lmrate[i]
  rate <- trial4Ito_df$bkgcorrect[i]
  rate <- rate * 60 #mg per L per min
  rate <- rate * (350/1000) #remove vol, mg per min
  rate <- rate / 4.2 #mg per g per min
  rate <- rate *1000 #mg per kg per min
  trial4Ito_df$bkgcorrect_mgkgmin[i] <- abs(rate)
}






CRrate_sum <- CRrate%>%
  group_by(temp, Species) %>%
  summarize(rate = mean(lmrate, na.rm = TRUE,),
            sd = sd(lmrate, na.rm = T),
            ratemgkgmin = mean(lmratemgkgmin, na.rm = T),
            .groups = "keep")


trial4Masu_df_sum <- trial4Masu_df%>%
  group_by(temp, Species) %>%
  summarize(rate = mean(lmrate, na.rm = TRUE,),
            sd = sd(lmrate, na.rm = T),
            ratemgkgmin = mean(lmratemgkgmin, na.rm = T),
            .groups = "keep")


for (i in 1:length(trial4Masu_df_sum)){
  trial4Masu_df_sum$bkgcorrect[i] <- trial4Masu_df_sum$rate[i] - CRrate_sum$rate[i]
rate <- trial4Masu_df_sum$bkgcorrect[i]
rate <- rate * 60 #mg per L per min
rate <- rate * (350/1000) #remove vol, mg per min
rate <- rate / 4.2 #mg per g per min
rate <- rate *1000 #mg per kg per min
trial4Masu_df_sum$bkgcorrect_mgkgmin[i] <- abs(rate)
}

ggplot(trial4Masu_df, aes(x = temp, y = lmratemgkgmin)) + 
  geom_point(size = 3) + 
  geom_point(size = 3, aes(x = temp, y = bkgcorrect_mgkgmin), color = 'red') +
  theme_minimal()
