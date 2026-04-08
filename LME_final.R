library(tidyverse)
library(lme4)
library(nlme)
library(patchwork)

############# importing cleanded data ###########


final_df <- read.csv('Clean_final_df.csv')

final_df_ito <- final_df %>%
  filter(Species == 'Ito')

final_df_masu <- final_df %>%
  filter(Species == 'Masu')


df_2023 <- read.csv('data_2023_clean.csv')

df_2023_ito <- df_2023 %>%
  filter(Species == 'Ito')

df_2023_masu <- df_2023 %>%
  filter(Species == 'Masu')

################## Fitting models for every DF #############


#intermittent
Itomodel_int <- lme(log(mass_corrected) ~ temp_bin + time, random =  ~1 | FishID,
                    data = final_df_ito)
summary(Itomodel_int)$tTable


Masumodel_int <- lme(log(mass_corrected) ~ temp_bin + time, random =  ~1 | FishID,
                    data = final_df_masu)
summary(Masumodel_int)


#Static
Itomodel_stat <- lme(log(mass_corrected) ~ temp_bin, random =  ~1 | FishID,
                    data = df_2023_ito)
summary(Itomodel_stat)
summary(Itomodel_stat)$tTable


Masumodel_stat <- lme(log(mass_corrected) ~ temp_bin, random =  ~1 | FishID,
                    data = df_2023_masu)
summary(Masumodel_stat)


model_list <- mget(c('Itomodel_int', 'Masumodel_int', 'Itomodel_stat', 'Masumodel_stat'))
model_table <- data.frame(model_name = c('Itomodel_int', 'Masumodel_int', 'Itomodel_stat', 'Masumodel_stat'))
for (i in 1:length(model_list)){
  model_table$temp_val[i] <- model_list[[i]][["coefficients"]][["fixed"]][["temp_bin"]]
  model_table$intercept[i] <- model_list[[i]][["coefficients"]][["fixed"]][["(Intercept)"]]
  model_table$sigma[i] <- model_list[[i]][["sigma"]]
  r2_val <- r2_nakagawa(model_list[[i]])
  model_table$margR2[i] <- r2_val[["R2_marginal"]]
  model_table$condR2[i] <- r2_val[["R2_conditional"]]
}

model_table$time_val[1] <- Itomodel_int[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[2] <- Masumodel_int[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[3] <- NA
model_table$time_val[4] <- NA
model_table$model_name <- c('Itomodel_int', 'Masumodel_int', 'Itomodel_stat', 'Masumodel_stat')

model_table$condR2[3] <- NA

# Temp CI limits

model_table$tempUCI[1] <- intervals(Itomodel_int)$fixed[8]
model_table$tempLCI[1] <- intervals(Itomodel_int)$fixed[2]

model_table$tempUCI[2] <- intervals(Masumodel_int)$fixed[8]
model_table$tempLCI[2] <- intervals(Masumodel_int)$fixed[2]

model_table$tempUCI[3] <- intervals(Itomodel_stat)$fixed[6]
model_table$tempLCI[3] <- intervals(Itomodel_stat)$fixed[2]

model_table$tempUCI[4] <- intervals(Masumodel_stat)$fixed[6]
model_table$tempLCI[4] <- intervals(Masumodel_stat)$fixed[2]


#Time CI Limits

model_table$timeUCI[1] <- intervals(Itomodel_int)$fixed[9]
model_table$timeLCI[1] <- intervals(Itomodel_int)$fixed[3]

model_table$timeUCI[2] <- intervals(Masumodel_int)$fixed[9]
model_table$timeLCI[2] <- intervals(Masumodel_int)$fixed[3]

model_table$timeUCI[3] <- NA
model_table$timeLCI[3] <- NA

model_table$timeUCI[4] <- NA
model_table$timeLCI[4] <- NA


#plus/minus for CI

model_table$tempCI[1] <- (model_table$tempUCI[1] - model_table$tempLCI[1])/2
model_table$tempCI[2] <- (model_table$tempUCI[2] - model_table$tempLCI[2])/2
model_table$tempCI[3] <- (model_table$tempUCI[3] - model_table$tempLCI[3])/2
model_table$tempCI[4] <- (model_table$tempUCI[4] - model_table$tempLCI[4])/2


model_table$timeCI[1] <- (model_table$timeUCI[1] - model_table$timeLCI[1])/2
model_table$timeCI[2] <- (model_table$timeUCI[2] - model_table$timeLCI[2])/2
model_table$timeCI[3] <- NA
model_table$timeCI[4] <- NA

#adding t-value and p-value

#ito

model_table$inttvalue[1] <- summary(Itomodel_int)$tTable[10]
model_table$temptvalue[1] <- summary(Itomodel_int)$tTable[11]
model_table$timetvalue[1] <- summary(Itomodel_int)$tTable[12]

model_table$intPvalue[1] <- summary(Itomodel_int)$tTable[13]
model_table$tempPvalue[1] <- summary(Itomodel_int)$tTable[14]
model_table$timePvalue[1] <- summary(Itomodel_int)$tTable[15]

#masu

model_table$inttvalue[2] <- summary(Itomodel_int)$tTable[10]
model_table$temptvalue[2] <- summary(Itomodel_int)$tTable[11]
model_table$timetvalue[2] <- summary(Itomodel_int)$tTable[12]

model_table$intPvalue[2] <- summary(Masumodel_int)$tTable[13]
model_table$tempPvalue[2] <- summary(Masumodel_int)$tTable[14]
model_table$timePvalue[2] <- summary(Masumodel_int)$tTable[15]


# static models

#ito

model_table$inttvalue[3] <- summary(Itomodel_stat)$tTable[7]
model_table$temptvalue[3] <- summary(Itomodel_stat)$tTable[8]
model_table$timetvalue[3] <- NA

model_table$intPvalue[3] <- summary(Itomodel_stat)$tTable[9]
model_table$tempPvalue[3] <- summary(Itomodel_stat)$tTable[10]
model_table$timePvalue[3] <- NA

#masu

model_table$inttvalue[4] <- summary(Itomodel_int)$tTable[7]
model_table$temptvalue[4] <- summary(Itomodel_int)$tTable[8]
model_table$timetvalue[4] <- NA

model_table$intPvalue[4] <- summary(Masumodel_int)$tTable[9]
model_table$tempPvalue[4] <- summary(Masumodel_int)$tTable[10]
model_table$timePvalue[4] <- NA
# selecting final values

model_table <- model_table %>%
  select(model_name, intercept, temp_val, tempCI, time_val, timeCI, margR2, condR2)

colnames(model_table) <- c('Species~Method', 'Intercept', 'Temperature',
                           'Temp CI', 'Time', 'Time CI', 
                           'Marginal R²', 'Conditional R²')
model_table$`Species~Method` <- c('Ito Intermittent', 'Masu Intermittent',
                                  'Ito Static', 'Masu Static')

#write.csv(model_table, 'model_table.csv')

################## making prediction lines ################

# Ito intermittent

x_vals_ito <- seq(10, 25, by = 0.5)


newdata_ito <- data.frame(temp_bin = x_vals_ito,
                          time = 0)

pred_vals_ito <- predict(Itomodel_int, newdata = newdata_ito, level = 0)

line_vals_ito <- data.frame(
  x = x_vals_ito,
  y = exp(pred_vals_ito)
)

# Ito Static

x_vals_ito_stat <- seq(10, 25, by = 0.5)


newdata_ito_stat <- data.frame(temp_bin = x_vals_ito_stat)

pred_vals_ito_stat <- predict(Itomodel_stat, newdata = newdata_ito_stat, level = 0)

line_vals_ito_stat <- data.frame(
  x = x_vals_ito_stat,
  y = exp(pred_vals_ito_stat)
)

############ Ito Plot

ito_plot <- ggplot(final_df_ito, aes(x = temp_bin, y = mass_corrected)) +
  geom_point(data = final_df_ito, aes(shape = "Intermittent"),
             size = 2 , alpha = 0.6) +
  geom_point(data = df_2023_ito, aes(shape = "Static"),
             size  = 2, alpha = 0.6) +
  geom_line(data = line_vals_ito, aes(x = x, y = y, linetype = 'Intermittent'),
            color = "black", linewidth = 1.2) + 
  geom_line(data = line_vals_ito_stat, aes(x = x, y = y, linetype = 'Static'),
            color = 'black', linewidth = 1.2) + 
  xlim(5,25) + 
  ylim(0,8) +  
  theme_minimal() +
  labs(#title = "Ito Static & Intermittent Models",
       shape = "Type",
       linetype = 'Type',
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (mgO"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2)) + 
  scale_linetype_manual(values = c("Intermittent" = "solid",
                                   "Static" = "dashed")) 




  theme(
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )

#line_vals_ito_stat[31,] - line_vals_ito_stat[1,]
#line_vals_ito[31,] - line_vals_ito[1,]

########### Masu Intermittent
x_vals_masu <- seq(10, 25, by = 0.5)


newdata_masu <- data.frame(temp_bin = x_vals_masu,
                          time = 0)

pred_vals_masu <- predict(Masumodel_int, newdata = newdata_masu, level = 0)

line_vals_masu <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu)
)


# Masu Static
x_vals_masu <- seq(10, 25, by = 0.5)


newdata_masu_stat <- data.frame(temp_bin = x_vals_masu)

pred_vals_masu_stat <- predict(Masumodel_stat, newdata = newdata_masu_stat, level = 0)

line_vals_masu_stat <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu_stat)
)


masu_plot <- ggplot(final_df_masu, aes(x = temp_bin, y = mass_corrected)) +
  geom_point(data = final_df_masu, aes(shape = "Intermittent"),
             size = 2 , alpha = 0.6) +
  geom_point(data = df_2023_masu, aes(shape = "Static"),
             size  = 2, alpha = 0.6) +
  geom_line(data = line_vals_masu, aes(x = x, y = y, linetype = 'Intermittent'),
            color = "black", linewidth = 1.2) +
  geom_line(data = line_vals_masu_stat, aes(x = x, y = y, linetype = 'Static'),
            color = 'black', linewidth = 1.2) + 
  xlim(5,25) + 
  ylim(0,8) +  
  theme_minimal() +
labs(#title = "Masu Static & Intermittent Models",
     x = 'Temperature (°C)',
     shape = 'Type',
     linetype = 'Type',
     y = expression("Metabolic Rate (mgO"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2)) +
  scale_linetype_manual(values = c("Intermittent" = "solid",
                                   "Static" = "dashed")) 




  theme(
    plot.background = element_rect(color = "black", fill = NA, linewidth = 1)
  )
  
  
masu_plot

ito_plot + masu_plot + plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = "A")
  

############ Masu Comparison with Chinook ############


