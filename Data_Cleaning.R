library(tidyverse)
#install.packages("ggpattern")
library(ggpattern)
library(patchwork)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")


#Start from lowest quartile section




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
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13', 'Ito4', 'Masu4', 
                        'Masu3', 'Masu11', 'Masu12', 'Masu13', 'Blank'))


Masu1_rm <- data.frame(FishID = 'Masu1',
                       rep = c(12:14, 16,21,41,50:52,58))
Masu2_rm <- data.frame(FishID = 'Masu2',
                       rep = c(13, 57:63,95))
#Masu3_rm <- data.frame(FishID = 'Masu3',
#                       rep = c(1,13,2,3,4,44:47,5,55:75,95))
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
#Ito4_rm <- data.frame(FishID = 'Ito4',
#                      rep = c(1,13,14,2,25,3,44,45,47,49,5,55,57,58:64,
#                              66,67,68,71,74,75,76:78,94,95))
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

data_2023 <- data_2023 %>%   # 9 ariablse
  select(FishID, Species, temp_bin, rate_output, rate_final, rep,
         trial, Method, mass, vol, temp) %>%
  filter(!FishID %in% 'Ito15')

colnames(data_2023) <- c('FishID', 'Species', 'temp_bin', 'rate_ggd', 'rate_final',
                         'rep', 'trial', 'Method', 'mass', 'vol', 'temp_exact')


fishlist_2023 <- data_2023 %>%
  select(FishID, mass, vol) %>%
  group_by(FishID, mass, vol) %>%
  mutate(FishID = FishID,
         Mass = mass,
         Volume = vol) %>%
  count(FishID, name = 'Replicates') 

#write.csv(fishlist_2023, 'fishlist_2023.csv')



fishlist_2024 <- Fishlist %>%
  filter(!FishID %in% 'Blank') %>%
  select(FishID, mass, vol, Trial) %>%
  group_by(FishID, mass, vol, Trial) %>%
  mutate(FishID = FishID,
         Mass = mass,
         Volume = vol,
         Trial = Trial)
#write.csv(fishlist_2024, 'fishlist_2024.csv')

########## Actual Join (Only run if you intend to join, as you will lose some variables)

#;(
final_df <- read.csv('final_df.csv')
final_df_join <- final_df %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, mass, vol, trial)
final_df_join$Method <- 'Intermittent'

data_2023_join <- data_2023 %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, mass, vol, trial, Method) %>%
  filter(!FishID %in% 'Ito15')

two_year_data <- rbind(final_df_join, data_2023_join)



two_year_data_table <- two_year_data %>%
  select(FishID, Species,mass, vol, Method) %>%
  group_by(FishID, Species, mass, vol, Method) %>%
  mutate(FishID = FishID,
         Species = Species,
         Mass = mass,
         Volume = vol,
         Method = Method)
#write.csv(two_year_data_table, 'two_year_data_table.csv')

#two_year_data$rate <- abs(two_year_data$rate)
two_year_data$lograte <- log(two_year_data$rate_final)
two_year_data$lograte_ggd <- log(two_year_data$rate_ggd)




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



















################ Taking lowest quartile only ################


# Every other line is the same as above, just filtered to lowest quartile


raw_df <- bind_rows(raw_list)


raw_df <- raw_df %>%
  filter(!FishID %in% c('Ito6', 'Ito11', 'Ito13', 'Ito4', 'Masu4', 
                        'Masu3', 'Masu11', 'Masu12', 'Masu13', 'Blank'))


Masu1_rm <- data.frame(FishID = 'Masu1',
                       rep = c(12:14, 16,21,41,50:52,58))
Masu2_rm <- data.frame(FishID = 'Masu2',
                       rep = c(13, 57:63,95))
#Masu3_rm <- data.frame(FishID = 'Masu3',
#                       rep = c(1,13,2,3,4,44:47,5,55:75,95))
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
#Ito4_rm <- data.frame(FishID = 'Ito4',
#                      rep = c(1,13,14,2,25,3,44,45,47,49,5,55,57,58:64,
#                              66,67,68,71,74,75,76:78,94,95))
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

raw_df <- raw_df %>% #10 of them colander bois
  select(FishID, Species, temp,
         lmrate, lmratemgkgmin, lmrateggd,
         rep, trial, mass, vol,
         rsq, sd, deltatemp)

colnames(raw_df) <- c('FishID', 'Species', 'temp_bin',
                      'rate', 'rate_final', 'rate_ggd',
                      'rep', 'trial', 'mass', 'vol',
                      'rsq', 'sd', 'deltatemp')



raw_df_lowquar <- raw_df %>% 
  group_by(FishID, temp_bin) %>%
  filter(rate_final <= quantile(rate_final, 0.25, na.rm = TRUE))


#write.csv(raw_df, 'raw_df.csv')


temp_counts <- raw_df_lowquar_rm %>% 
  group_by(trial, Species, temp_bin) %>%
  summarize(rate_avg = mean(rate_final, na.rm = T))




final_fish <- unique(raw_df_lowquar$FishID)
final_fish
lowcount_list <- as.list(final_fish)
lowcount_temps <- list(17, NaN, NaN, NaN, c(17,23, 24), c(14:17), c(14:17), 20, c(18, 20, 21),
                       20, 20, c(18, 20), c(18, 20), 20)



lowcount_tbl <- tibble(
  FishID = final_fish,
  temp_bin = lowcount_temps
) %>%
  # remove NaN-only entries
  filter(!map_lgl(temp_bin, ~ all(is.nan(.x)))) %>%
  unnest(temp_bin)  


# Remove matching rows
raw_df_lowquar_rm <- raw_df_lowquar %>%
  anti_join(lowcount_tbl, by = c("FishID", "temp_bin"))



final_df <- raw_df_lowquar_rm
final_df <- final_df %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, mass, vol, trial)


Fishlist_b1 <- Fishlist %>%
  filter(FSID %in% c(1:4))
Fishlist_b2 <- Fishlist %>%
  filter(FSID %in% c(5:8))

final_df <- final_df %>%
  mutate(bath = case_when(
    FishID %in% Fishlist_b1$FishID ~ 1,
    FishID %in% Fishlist_b2$FishID ~ 2,
    TRUE ~ NA_real_))

final_df$lograte <- log(final_df$rate_final)
final_df$lograte_ggd <- log(final_df$rate_ggd)

final_df$day <- NaN
final_df$day[final_df$trial != 3] <- 1

final_df$day[final_df$trial != 3 & final_df$rep >= 48] <- 2

final_df$day[final_df$trial == 3 & final_df$rep <= 19] <- 1

final_df$day[final_df$trial == 3 & final_df$rep >= 19] <- 2

#write.csv(final_df, 'final_df.csv')

################# mass correction for body size ##########


final_df <- read.csv('final_df.csv')




final_df$no_mass_rate <- final_df$rate_final *60 # mg per kg per hour
final_df$mass_corrected <- final_df$no_mass_rate * (((final_df$mass/1000)/0.15)^(1-0.89)) #corrected for body mass
final_df$mass_corrected <- final_df$mass_corrected / 60


plot1 <- ggplot(final_df, aes(x = temp_bin, y = rate_final)) + 
  geom_point() +
  ylim(0,10)
plot1

plot2 <- ggplot(final_df, aes(x = temp_bin, y = mass_corrected)) + 
  geom_point() +
  ylim(0,10)
plot2
plot1 + plot2




############ Adding cumulative time for intermittent ############

final_df$time <- 0
final_df$time[final_df$trial == 1] <- (0 + (final_df$rep[final_df$trial == 1] * 30))


final_df$time[final_df$trial == 2 & final_df$bath == 1] <- 3075 + (final_df$rep[final_df$trial == 2 & final_df$bath == 1] * 30)

final_df$time[final_df$trial == 2 & final_df$bath == 2] <- 0 + (final_df$rep[final_df$trial == 2 & final_df$bath == 2] * 30)


final_df$time[final_df$trial == 3 & final_df$bath == 1] <- 7150 + (final_df$rep[final_df$trial == 3 & final_df$bath == 1] * 30)

final_df$time[final_df$trial == 3 & final_df$bath == 2] <- 4075 + (final_df$rep[final_df$trial == 3 & final_df$bath == 2] * 30)

final_df_2 <- final_df %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, trial, bath, mass_corrected, time)

write.csv(final_df, 'Clean_final_df.csv')




raw_df <- read.csv('raw_df.csv')


report_table <- raw_df %>%
  group_by(FishID, Species, mass, vol, trial) %>%
  select(FishID, Species, mass, vol, trial) %>%
  count(FishID) %>%
  select(-n)
  
names(report_table) <- c('Fish ID', 'Species', 'Fish Mass', 'Chamber Volume', 'Trial')
  
#write.csv(report_table, 'Final_table.csv')
  

########## Mass correction for static ##########

data_2023 <- read.csv('Resp_23_data.csv')

data_2023$no_mass_rate <- 0
data_2023$mass_corrected <- 0

data_2023$no_mass_rate <- data_2023$rate_final * 60 # mg per kg per hour
data_2023$mass_corrected <- data_2023$no_mass_rate * (((data_2023$mass/1000)/0.15)^(1-0.89)) #corrected for body mass
data_2023$mass_corrected <- data_2023$mass_corrected / 60 #back to mg/kg/min

data_2023_clean <- data_2023 %>%
  filter(!FishID %in% 'Ito15') 


data_2023_clean <- data_2023 %>%
  filter(!FishID %in% 'Ito15') %>%
  select(FishID, Species, temp_bin, rate_output, rate_final,
         mass_corrected, trial)

write.csv(data_2023_clean, '2023_resp_data.csv')


Fish_table_2023 <- data_2023 %>%
  filter(!FishID %in% 'Ito15') %>%
  group_by(FishID, Species, mass, vol) %>%
  select(FishID, Species, mass, vol) %>%
  count(FishID) %>%
  select(-n)

names(Fish_table_2023) <- c('FishID', 'Species', 'Fish Mass', 'Chamber Volume')


#write.csv(Fish_table_2023, '2023_table.csv')


temp_counts <- data_2023_clean %>% 
  group_by(Species, FishID, temp_bin) %>%
  count()












######## Prepping Columns

data_2023 <- read.csv('Resp_23_data.csv')

#head(data_2023)
#head(raw_df)

raw_df <- raw_df %>% #10 of them colander bois
  select(FishID, Species, temp,
         lmrate, lmratemgkgmin, lmrateggd,
         rep, trial, mass, vol,
         rsq, sd, deltatemp)

colnames(raw_df) <- c('FishID', 'Species', 'temp_bin',
                      'rate', 'rate_final', 'rate_ggd',
                      'rep', 'trial', 'mass', 'vol',
                      'rsq', 'sd', 'deltatemp')


data_2023 <- data_2023 %>%   # 9 ariablse
  select(FishID, Species, temp_bin, rate_output, rate_final, rep,
         trial, Method, mass, vol, temp)

colnames(data_2023) <- c('FishID', 'Species', 'temp_bin', 'rate_ggd', 'rate_final',
                         'rep', 'trial', 'Method', 'mass', 'vol', 'temp_exact')



########## Actual Join (Only run if you intend to join, as you will lose some variables)

#;(
raw_df_join <- raw_df %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, mass, vol, trial)
raw_df_join$Method <- 'Intermittent'

data_2023_join <- data_2023 %>%
  select(FishID, Species, temp_bin, rate_ggd, rate_final,
         rep, mass, vol, trial, Method) %>%
  filter(!FishID %in% 'Ito15')

################################### adding day to plot 

two_year_data <- rbind(raw_df_join, data_2023_join)
#two_year_data$rate <- abs(two_year_data$rate)
two_year_data$lograte <- log(two_year_data$rate_final)
two_year_data$lograte_ggd <- log(two_year_data$rate_ggd)
two_year_data$day <- NaN
two_year_data$day[
  two_year_data$trial != 3 &
    two_year_data$Method == "Intermittent"
] <- 1

two_year_data$day[
  two_year_data$trial != 3 &
    two_year_data$Method == "Intermittent" &
    two_year_data$rep >= 48
] <- 2

two_year_data$day[
  two_year_data$trial == 3 &
    two_year_data$Method == "Intermittent" &
    two_year_data$rep <= 19
] <- 1


two_year_data$day[
  two_year_data$trial == 3 &
    two_year_data$Method == "Intermittent" &
    two_year_data$rep >= 19
] <- 2

write.csv(two_year_data, 'two_year_data.csv')


################ Testing difference in the trials

aov(rate ~ trial, data = raw_df)

raw_df$trial <- as.factor(raw_df$trial)

model <- aov(rate ~ trial, data = raw_df)
summary(model)
TukeyHSD(model)



tri1_rates <- raw_df$rate_final[raw_df$trial == '1']
tri2_rates <- raw_df$rate_final[raw_df$trial == '2']
tri3_rates <- raw_df$rate_final[raw_df$trial == '3']

anova(tri1_rates, tri2_rates, tri3_rates)




############################ Extra Plot by day ##################




plot_data_int <- two_year_data_Masu%>%
  filter(Method == 'Intermittent') %>%
  group_by(FishID, temp_bin, day) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))


plot6 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size = 3) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size  = 2) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,12) +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)) +
  labs(shape = "Data Type",
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c( '1' = 16,  '2' = 17))
plot6

#

plot_data_int <- two_year_data_Ito%>%
  filter(Method == 'Intermittent') %>%
  group_by(FishID, temp_bin, day) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))


plot5 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size = 3) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size  = 2) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,12) +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)) +
  labs(shape = "Data Type",
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c( '1' = 16,  '2' = 17))
plot5







plot5 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size = 3) +
  geom_point(data = plot_data_int, aes(shape = factor(day), color = FishID), size  = 2) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,12) +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)) +
  labs(shape = "Data Type",
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c( '1' = 16,  '2' = 17))
plot5

















################ Plotting MO2 over temp for every fish, separated by lowest quartile






















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
