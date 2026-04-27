library(tidyverse)
library(performance)


df_2023 <- read.csv('data_2023_clean.csv')

df_2023_ito <- df_2023 %>%
  filter(Species == 'Ito')

df_2023_masu <- df_2023 %>%
  filter(Species == 'Masu')


Itomodel_stat <- lme(log(mass_corrected) ~ temp_bin, random =  ~1 | FishID,
                     data = df_2023_ito)
summary(Itomodel_stat)
test_r2 <- r2_nakagawa(test)
test_r2[['R2_conditional']]


########## Null model 

test <- lme(log(mass_corrected) ~ 1, random = ~1 | FishID,
            data = df_2023_ito)

test_r2 <- r2_nakagawa(test)
test_r2[['R2_conditional']]



Masumodel_stat <- lme(log(mass_corrected) ~ temp_bin, random =  ~1 | FishID,
                      data = df_2023_masu)
summary(Masumodel_stat)




