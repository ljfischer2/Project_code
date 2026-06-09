################ ANCOVA and other additional analyses ##################

######### ANCOVA of CTmax #########

library(tidyverse)
library(rstatix)
library(broom)
library(lme4)
library(nlme)
library(lmtest)
library(ggfortify)
library(performance)
require(gridExtra)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")
ctm_data <- read.csv("CTM_Data_File.csv")


# Fit the linear model
ancova_model <- lm(Temp.LOE ~ Species * Trial * Fork.Length.mm, data = ctm_data)

# Perform ANCOVA
ancova_result <- aov(ancova_model)

# Print the ANCOVA result
summary(ancova_result)



################ Model Table for RMR ###############
####### Import and prep data to join

final_df <- read.csv('Clean_final_df.csv')
data_2023 <- read.csv('2023_resp_data.csv')



final_df_join <- final_df %>%
  select(FishID, Species, temp_bin, mass_corrected, mass, vol, rep)

names(final_df_join) <- c('FishID', 'Species', 'Temperature', 'RMR', 'Fish Mass', 'Chamber Volume',
                     'Replicate')

final_df_join$Method <- 'Intermittent'




data_2023_join <- data_2023 %>%
  select(FishID, Species, temp_bin, mass_corrected, mass, vol, trial)
names(data_2023_join) <- c('FishID', 'Species', 'Temperature', 'RMR', 'Fish Mass', 'Chamber Volume',
                             'Replicate')
data_2023_join$Method <- 'Static'

combined_data <- rbind(final_df_join, data_2023_join)


##### First Testing Species together ########


M1 <- lme(log(RMR) ~ Temperature, random =  ~1 | FishID,
    data = combined_data)

M2 <- lme(log(RMR) ~ Temperature + Species, random =  ~1 | FishID,
          data = combined_data)

M3 <- lme(log(RMR) ~ Temperature + Replicate, random =  ~1 | FishID,
          data = combined_data)

M4 <- lme(log(RMR) ~ Temperature + Method, random =  ~1 | FishID,
          data = combined_data)

M5 <- lme(log(RMR) ~ Temperature + Species + Replicate, random =  ~1 | FishID,
          data = combined_data)

M6 <- lme(log(RMR) ~ Temperature + Species + Method, random =  ~1 | FishID,
          data = combined_data)

M7 <- lme(log(RMR) ~ Temperature + Method + Replicate, random =  ~1 | FishID,
          data = combined_data)

M8 <- lme(log(RMR) ~ Temperature + Species + Method + Replicate, random =  ~1 | FishID,
          data = combined_data)

M9 <- lme(log(RMR) ~ Temperature * Species + Method + Replicate, random =  ~1 | FishID,
          data = combined_data)

M10 <- lme(log(RMR) ~ Temperature * Species, random =  ~1 | FishID,
           data = combined_data)

M11 <- lme(log(RMR) ~ Temperature * Species + Method, random =  ~1 | FishID,
           data = combined_data)


AIC(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)
lrtest(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)

model_list <- mget(c('M1', 'M2', 'M3', 'M4', 'M5',
                      'M6', 'M7', 'M8', 'M9', 'M10', 'M11'))
model_table <- data.frame(model_name = c('M1', 'M2', 'M3', 'M4', 'M5',
                                         'M6', 'M7', 'M8', 'M9', 'M10', 'M11'))
for (i in 1:length(model_list)){
  model_table$temp_val[i] <- model_list[[i]][["coefficients"]][["fixed"]][["Temperature"]]
  model_table$intercept[i] <- model_list[[i]][["coefficients"]][["fixed"]][["(Intercept)"]]
  model_table$sigma[i] <- model_list[[i]][["sigma"]]
  r2_val <- r2_nakagawa(model_list[[i]])
  model_table$margR2[i] <- r2_val[["R2_marginal"]]
  model_table$condR2[i] <- r2_val[["R2_conditional"]]
  pvals <- summary(model_list[[i]])$tTable
  model_table$p_temp[i] <- pvals["Temperature", "p-value"]
}



AIC_results <- AIC(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11)

model_table$df <- AIC_results$df
model_table$AIC <- AIC_results$AIC
names(model_table) <- c('Model', 'Temperature', 'Intercept','Sigma',
                        'Marginal R²', 'Conditional R²', 'Temperature P-Value',
                        'df', 'AIC')

numeric_cols <- c('Temperature', 'Intercept', 'Sigma', 'Marginal R²', 'Conditional R²', 'AIC')
model_table[numeric_cols] <- lapply(model_table[numeric_cols], round, 3)

model_table$Model <- c('Rate ~ Temperature',
                       'Rate ~ Temperature + Species',
                       'Rate ~ Temperature + Replicate',
                       'Rate ~ Temperature + Method', 
                       'Rate ~ Temperature + Species + Replicate',
                       'Rate ~ Temperature + Species + Method',
                       'Rate ~ Temperature + Method + Replicate',
                       'Rate ~ Temperature + Species + Method + Replicate',
                       'Rate ~ Temperature * Species + Method + Replicate',
                       'Rate ~ Temperature * Species',
                       'Rate ~ Temperature * Species + Method')

model_table$DeltaAIC <- min(model_table$AIC) - model_table$AIC
order(model_table$DeltaAIC)
write.csv(model_table, 'Model_Selection_Table.csv')
