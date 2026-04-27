library(tidyverse)
library(lme4)
library(nlme)
library(patchwork)
library(lmtest)
library(ggfortify)
library(performance)
require(gridExtra)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")

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
summary(Itomodel_int)


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
  pvals <- summary(model_list[[i]])$tTable
  model_table$p_temp[i] <- pvals["temp_bin", "p-value"]
}

model_table$time_val[1] <- Itomodel_int[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[2] <- Masumodel_int[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[3] <- NA
model_table$time_val[4] <- NA
model_table$model_name <- c('Itomodel_int', 'Masumodel_int', 'Itomodel_stat', 'Masumodel_stat')

model_table$condR2[3] <- model_table$margR2[3]

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
  select(model_name, intercept, temp_val, p_temp, tempLCI, tempUCI,
         time_val, timeLCI, timeUCI, margR2, condR2)

colnames(model_table) <- c('Species~Method', 'Intercept',
                           'Temperature', 'p-value', 'Temp LCI', 'Temp UCI',
                           'Time', 'Time LCI', 'Time UCI', 
                           'Marginal R²', 'Conditional R²')
model_table$`Species~Method` <- c('Ito Intermittent', 'Masu Intermittent',
                                  'Ito Static', 'Masu Static')
table_wider <- model_table %>%
  pivot_longer(
    cols = -'Species~Method',
    names_to = 'Parameter',
    values_to = 'Value'
  ) %>%
  pivot_wider(
    names_from = 'Species~Method',
    values_from = 'Value'
  )



#table_wider_nosci <- table_wider
#table_wider_nosci$`Masu Static` <- format(table_wider_nosci$`Masu Static`, scientific = FALSE)
#table_wider_nosci$`Masu Static` <- as.numeric(sprintf("%.0f", table_wider_nosci$`Masu Static`))
#table_wider_nosci$`Masu Intermittent` <- format(table_wider_nosci$`Masu Intermittent`, scientific = FALSE)
#table_wider_nosci$`Ito Intermittent` <- format(table_wider_nosci$`Ito Intermittent`, scientific = FALSE)
#write.csv(table_wider, 'model_table.csv')

################## making prediction lines ################

# Ito intermittent

x_vals_ito <- seq(16, 24, by = 0.5)


newdata_ito <- data.frame(temp_bin = x_vals_ito,
                          time = 0)

pred_vals_ito <- predict(Itomodel_int, newdata = newdata_ito, level = 0)

line_vals_ito <- data.frame(
  x = x_vals_ito,
  y = exp(pred_vals_ito)
)

# Ito Static

x_vals_ito_stat <- seq(12, 24, by = 0.5)


newdata_ito_stat <- data.frame(temp_bin = x_vals_ito_stat)

pred_vals_ito_stat <- predict(Itomodel_stat, newdata = newdata_ito_stat, level = 0)

line_vals_ito_stat <- data.frame(
  x = x_vals_ito_stat,
  y = exp(pred_vals_ito_stat)
)

############ Ito Plot

ito_plot <- ggplot(final_df_ito, aes(x = temp_bin, y = mass_corrected)) +
  geom_point(data = final_df_ito, aes(shape = "Intermittent"),
             size = 2 , alpha = 0.5) +
  geom_point(data = df_2023_ito, aes(shape = "Static"),
             size  = 2, alpha = 0.5) +
  geom_line(data = line_vals_ito, aes(x = x, y = y, linetype = 'Intermittent'),
            color = "black", linewidth = 1) + 
  geom_line(data = line_vals_ito_stat, aes(x = x, y = y, linetype = 'Static'),
            color = 'black', linewidth = 1) + 
  xlim(8,25) + 
  ylim(0,8) +  
  theme_minimal() +
  labs(#title = "Ito Static & Intermittent Models",
       shape = "Type",
       linetype = 'Type',
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (mgO"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 16, "Static" = 17)) + 
  scale_linetype_manual(values = c("Intermittent" = "solid",
                                   "Static" = "dashed")) + 
  theme(
    text = element_text(size = 14),
    legend.position = 'none'
  )

ito_plot

#line_vals_ito_stat[31,] - line_vals_ito_stat[1,]
#line_vals_ito[31,] - line_vals_ito[1,]

########### Masu Intermittent
x_vals_masu <- seq(17, 23, by = 0.5)


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
             size = 2 , alpha = 0.5) +
  geom_point(data = df_2023_masu, aes(shape = "Static"),
             size  = 2, alpha = 0.5) +
  geom_line(data = line_vals_masu, aes(x = x, y = y, linetype = 'Intermittent'),
            color = "black", linewidth = 1) +
  geom_line(data = line_vals_masu_stat, aes(x = x, y = y, linetype = 'Static'),
            color = 'black', linewidth = 1) + 
  xlim(8,25) + 
  ylim(0,8) +  
  theme_minimal() +
labs(#title = "Masu Static & Intermittent Models",
     x = 'Temperature (°C)',
     shape = 'Type',
     linetype = 'Type',
     y = expression("Metabolic Rate (mgO"[2]*" kg"^{-1}*" min"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 16, "Static" = 17)) +
  scale_linetype_manual(values = c("Intermittent" = "solid",
                                   "Static" = "dashed")) + 
  theme(
    text = element_text(size = 14)
)
  
  
masu_plot

ito_plot + masu_plot + plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A")
  








############ Masu Comparison with Rainbow & Brook ############


# mass corrected in g/g/day for both spp
rate_ggd <- final_df_masu$mass_corrected
rate_ggd <- rate_ggd * 0.00144
final_df_masu$mass_corrected_ggd <- rate_ggd

rate_ggd <- final_df_ito$mass_corrected
rate_ggd <- rate_ggd * 0.00144
final_df_ito$mass_corrected_ggd <- rate_ggd


# for static as well
rate_ggd <- df_2023_ito$mass_corrected
rate_ggd <- rate_ggd * 0.00144
df_2023_ito$mass_corrected_ggd <- rate_ggd

rate_ggd <- df_2023_masu$mass_corrected
rate_ggd <- rate_ggd * 0.00144
df_2023_masu$mass_corrected_ggd <- rate_ggd


################### Models using ggd
Itomodel_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin + time, random =  ~1 | FishID,
                     data = final_df_ito)          #new model using ggd

Itomodel_stat_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin, random =  ~1 | FishID,
                          data = df_2023_ito)  

Masumodel_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin + time, random =  ~1 | FishID,
                     data = final_df_masu)          #new model using ggd

Masumodel_stat_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin, random =  ~1 | FishID,
                     data = df_2023_masu)          #new model using ggd

#same as above for line prediction

# Ito Static

x_vals_ito_stat <- seq(12, 24, by = 0.5)


newdata_ito_stat <- data.frame(temp_bin = x_vals_ito_stat)

pred_vals_ito_stat <- predict(Itomodel_stat_ggd, newdata = newdata_ito_stat, level = 0)

line_vals_ito_stat <- data.frame(
  x = x_vals_ito_stat,
  y = exp(pred_vals_ito_stat)
)

# Masu Static
x_vals_masu <- seq(10, 23, by = 0.5)

newdata_masu <- data.frame(temp_bin = x_vals_masu,
                           time = 0)

pred_vals_masu <- predict(Masumodel_stat_ggd, newdata = newdata_masu, level = 0)

line_vals_masu <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu)
)




####### importing rainbow text file from Tyler & Bolduc

rainbow_data <- read.table('Temp_MR_Figure.txt', sep = ',')
names(rainbow_data) <- c('Temperature', 'Rate')
rainbow_data$Temperature <- round(rainbow_data$Temperature)
rainbow_data$mass <- rnorm(14, 14.45, 3.016)

#mass correcting the rainbow trout
rainbow_ggd <- rainbow_data$Rate * 1000 * 1000 * (1/24) 

rainbow_data$mass_corrected <- rainbow_ggd * (((rainbow_data$mass/1000)/0.15)^(1-0.89)) #corrected for body mass

rainbow_data$mass_corrected <- rainbow_data$mass_corrected / 1000 / 1000 / (1/24)





########### brook trout
#mass correcting the brook trout
brook_data <- read.table('Hartman_Cox_brookies.txt', sep = ',')
names(brook_data) <- c('Temperature', 'Rate')
brook_data$Temperature <- round(brook_data$Temperature)
brook_data$mass <- rnorm(12, 14.45, 3.016)

#mass correcting the brook trout
brook_ggd <- brook_data$Rate * 1000 * 1000 * (1/24) 

brook_data$mass_corrected <- brook_ggd * (((brook_data$mass/1000)/0.15)^(1-0.89)) #corrected for body mass

brook_data$mass_corrected <- brook_data$mass_corrected / 1000 / 1000 / (1/24)




# Plotting
ggplot(df_2023_masu, aes(x = temp_bin, y = mass_corrected_ggd)) +
  geom_point(data = df_2023_masu, color = 'red2', shape = 16,
             size = 2.5) +
  geom_point(data = df_2023_ito, color = 'yellow2', shape = 16,
             size = 2.5) +
  geom_point(data = rainbow_data, color = 'green3',
             aes(y = mass_corrected, x = Temperature),
             size = 2.5,shape = 15) +
  geom_point(data = brook_data, color = 'blue',
             aes(y = mass_corrected, x = Temperature),
             size = 2.5,shape = 17) +
  geom_line(data = line_vals_masu, aes(x = x, y = y, linetype = 'Masu'),
            color = "black", linewidth = 1) +
  geom_line(data = line_vals_ito_stat, aes(x = x, y = y, linetype = 'Ito'),
            color = "black", linewidth = 1) +
  xlim(5,25) + 
  ylim(0,0.01) +  
  theme_minimal() +
  labs(#title = "Masu Static & Intermittent Models",
    x = 'Temperature (°C)',
#    shape = 'Type',
    linetype = 'Species',
    y = expression("Metabolic Rate (gO"[2]*" g"^{-1}*" day"^{-1}*")")) + 
#  scale_shape_manual(values = c("Masu" = 16, "Rainbow" = 17)) +
  scale_linetype_manual(values = c("Masu" = "solid",
                                   'Ito' = 'dotted')) + 
  theme(
    text = element_text(size = 14)
  )



####################### Table Creation for ggd Models ###################

# mass corrected in g/g/day for both spp
rate_ggd <- final_df_masu$mass_corrected
rate_ggd <- rate_ggd * 0.00144
final_df_masu$mass_corrected_ggd <- rate_ggd

rate_ggd <- final_df_ito$mass_corrected
rate_ggd <- rate_ggd * 0.00144
final_df_ito$mass_corrected_ggd <- rate_ggd


# for static as well
rate_ggd <- df_2023_ito$mass_corrected
rate_ggd <- rate_ggd * 0.00144
df_2023_ito$mass_corrected_ggd <- rate_ggd

rate_ggd <- df_2023_masu$mass_corrected
rate_ggd <- rate_ggd * 0.00144
df_2023_masu$mass_corrected_ggd <- rate_ggd


################### Models using ggd
Itomodel_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin + time, random =  ~1 | FishID,
                    data = final_df_ito)          #new model using ggd

Itomodel_stat_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin, random =  ~1 | FishID,
                         data = df_2023_ito)  

Masumodel_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin + time, random =  ~1 | FishID,
                     data = final_df_masu)          #new model using ggd

Masumodel_stat_ggd <- lme(log(mass_corrected_ggd) ~ temp_bin, random =  ~1 | FishID,
                          data = df_2023_masu)          #new model using ggd







model_list <- mget(c('Itomodel_ggd', 'Masumodel_ggd', 'Itomodel_stat_ggd', 'Masumodel_stat_ggd'))
model_table <- data.frame(model_name = c('Itomodel_ggd', 'Masumodel_ggd', 'Itomodel_stat_ggd', 'Masumodel_stat_ggd'))
for (i in 1:length(model_list)){
  model_table$temp_val[i] <- model_list[[i]][["coefficients"]][["fixed"]][["temp_bin"]]
  model_table$intercept[i] <- model_list[[i]][["coefficients"]][["fixed"]][["(Intercept)"]]
  model_table$sigma[i] <- model_list[[i]][["sigma"]]
  r2_val <- r2_nakagawa(model_list[[i]])
  model_table$margR2[i] <- r2_val[["R2_marginal"]]
  model_table$condR2[i] <- r2_val[["R2_conditional"]]
  pvals <- summary(model_list[[i]])$tTable
  model_table$p_temp[i] <- pvals["temp_bin", "p-value"]
}

model_table$time_val[1] <- Itomodel_ggd[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[2] <- Masumodel_ggd[["coefficients"]][["fixed"]][["time"]]
model_table$time_val[3] <- NA
model_table$time_val[4] <- NA
model_table$model_name <- c('Itomodel_ggd', 'Masumodel_ggd', 'Itomodel_stat_ggd', 'Masumodel_stat_ggd')

model_table$condR2[3] <- model_table$margR2[3]

# Temp CI limits

model_table$tempUCI[1] <- intervals(Itomodel_ggd)$fixed[8]
model_table$tempLCI[1] <- intervals(Itomodel_ggd)$fixed[2]

model_table$tempUCI[2] <- intervals(Masumodel_ggd)$fixed[8]
model_table$tempLCI[2] <- intervals(Masumodel_ggd)$fixed[2]

model_table$tempUCI[3] <- intervals(Itomodel_stat_ggd)$fixed[6]
model_table$tempLCI[3] <- intervals(Itomodel_stat_ggd)$fixed[2]

model_table$tempUCI[4] <- intervals(Masumodel_stat_ggd)$fixed[6]
model_table$tempLCI[4] <- intervals(Masumodel_stat_ggd)$fixed[2]


#Time CI Limits

model_table$timeUCI[1] <- intervals(Itomodel_ggd)$fixed[9]
model_table$timeLCI[1] <- intervals(Itomodel_ggd)$fixed[3]

model_table$timeUCI[2] <- intervals(Masumodel_ggd)$fixed[9]
model_table$timeLCI[2] <- intervals(Masumodel_ggd)$fixed[3]

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

model_table$inttvalue[1] <- summary(Itomodel_ggd)$tTable[10]
model_table$temptvalue[1] <- summary(Itomodel_ggd)$tTable[11]
model_table$timetvalue[1] <- summary(Itomodel_ggd)$tTable[12]

model_table$intPvalue[1] <- summary(Itomodel_ggd)$tTable[13]
model_table$tempPvalue[1] <- summary(Itomodel_ggd)$tTable[14]
model_table$timePvalue[1] <- summary(Itomodel_ggd)$tTable[15]

#masu

model_table$inttvalue[2] <- summary(Itomodel_ggd)$tTable[10]
model_table$temptvalue[2] <- summary(Itomodel_ggd)$tTable[11]
model_table$timetvalue[2] <- summary(Itomodel_ggd)$tTable[12]

model_table$intPvalue[2] <- summary(Masumodel_ggd)$tTable[13]
model_table$tempPvalue[2] <- summary(Masumodel_ggd)$tTable[14]
model_table$timePvalue[2] <- summary(Masumodel_ggd)$tTable[15]


# static models

#ito

model_table$inttvalue[3] <- summary(Itomodel_stat_ggd)$tTable[7]
model_table$temptvalue[3] <- summary(Itomodel_stat_ggd)$tTable[8]
model_table$timetvalue[3] <- NA

model_table$intPvalue[3] <- summary(Itomodel_stat_ggd)$tTable[9]
model_table$tempPvalue[3] <- summary(Itomodel_stat_ggd)$tTable[10]
model_table$timePvalue[3] <- NA

#masu

model_table$inttvalue[4] <- summary(Itomodel_ggd)$tTable[7]
model_table$temptvalue[4] <- summary(Itomodel_ggd)$tTable[8]
model_table$timetvalue[4] <- NA

model_table$intPvalue[4] <- summary(Masumodel_ggd)$tTable[9]
model_table$tempPvalue[4] <- summary(Masumodel_ggd)$tTable[10]
model_table$timePvalue[4] <- NA
# selecting final values

model_table <- model_table %>%
  select(model_name, intercept, temp_val, p_temp, tempLCI, tempUCI,
         time_val, timeLCI, timeUCI, margR2, condR2)

colnames(model_table) <- c('Species~Method', 'Intercept',
                           'Temperature', 'p-value', 'Temp LCI', 'Temp UCI',
                           'Time', 'Time LCI', 'Time UCI', 
                           'Marginal R²', 'Conditional R²')
model_table$`Species~Method` <- c('Ito Intermittent', 'Masu Intermittent',
                                  'Ito Static', 'Masu Static')
table_wider <- model_table %>%
  pivot_longer(
    cols = -'Species~Method',
    names_to = 'Parameter',
    values_to = 'Value'
  ) %>%
  pivot_wider(
    names_from = 'Species~Method',
    values_from = 'Value'
  )



table_wider_nosci <- table_wider
table_wider_nosci$`Masu Static` <- format(table_wider_nosci$`Masu Static`, scientific = FALSE)
table_wider_nosci$`Ito Static` <- format(table_wider_nosci$`Masu Static`, scientific = FALSE)
table_wider_nosci$`Masu Intermittent` <- format(table_wider_nosci$`Masu Intermittent`, scientific = FALSE)
table_wider_nosci$`Ito Intermittent` <- format(table_wider_nosci$`Ito Intermittent`, scientific = FALSE)
write.csv(table_wider, 'model_table_ggd.csv')




x_vals_masu <- seq(-1, 23, by = 0.5)

newdata_masu <- data.frame(temp_bin = x_vals_masu,
                           time = 0)

pred_vals_masu <- predict(Masumodel_stat, newdata = newdata_masu, level = 0)

line_vals_masu <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu)
)




ggplot(df_2023_masu, aes(x = temp_bin, y = mass_corrected)) +
  geom_point(data = df_2023_masu, shape = 16,
             size = 2) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 1) +
  theme_minimal() +
  labs(#title = "Masu Static & Intermittent Models",
    x = 'Temperature (°C)',
    #    shape = 'Type',
    #    linetype = 'Type',
    y = expression("Metabolic Rate (gO"[2]*" g"^{-1}*" day"^{-1}*")")) + 
  #  scale_shape_manual(values = c("Masu" = 16, "Rainbow" = 17)) +
  #  scale_linetype_manual(values = c("Static" = "solid")) + 
  theme(
    text = element_text(size = 14)
  )

