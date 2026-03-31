library(lme4)
library(nlme)
library(lmtest)
library(ggfortify)
library(performance)
require(gridExtra)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")
############
head(raw_df)   # checks to make sure that the data is present
head(two_year_data)

two_year_data_Ito <- two_year_data %>%
  filter(Species == 'Ito')

two_year_data_Ito_int <- two_year_data_Ito %>%
  filter(Method == 'Intermittent')
#write.csv(two_year_data_Ito, "Ito_only_data.csv")


two_year_data_Masu <- two_year_data %>%
  filter(Species == 'Masu')

two_year_data_Masu_int <- two_year_data_Masu %>%
  filter(Method == 'Intermittent')
#write.csv(two_year_data_Masu, "Masu_only_data.csv")

######### Ito Models ############


ggdItomodel1 <- lme(lograte_ggd ~ Method, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel2 <- lme(lograte_ggd ~ temp_bin, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel3 <- lme(lograte_ggd ~ mass, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel4 <- lme(lograte_ggd ~ temp_bin + Method, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel5 <- lme(lograte_ggd ~ mass + Method, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel6 <- lme(lograte_ggd ~ temp_bin + mass, random =  ~1 | FishID,
                 data = two_year_data_Ito)
ggdItomodel7 <- lme(lograte_ggd ~ temp_bin + Method + mass, random =  ~1 | FishID,
                 data = two_year_data_Ito)

AIC(ggdItomodel1, ggdItomodel2, ggdItomodel3, ggdItomodel4,
    ggdItomodel5, ggdItomodel6, ggdItomodel7)

lrtest(ggdItomodel1, ggdItomodel2, ggdItomodel3, ggdItomodel4,
       ggdItomodel5, ggdItomodel6, ggdItomodel7)


ggdItomodel_list <- mget(c('ggdItomodel1', 'ggdItomodel2', 'ggdItomodel3', 'ggdItomodel4',
                        'ggdItomodel5', 'ggdItomodel6', 'ggdItomodel7'))
ggdItomodel_list_str <- c('ggdItomodel1', 'ggdItomodel2', 'ggdItomodel3', 'ggdItomodel4',
                       'ggdItomodel5', 'ggdItomodel6', 'ggdItomodel7')
ggdItomodel_var_list <- c('lograte ~ Method, random =  ~1 | FishID',
                       'lograte ~ temp_bin, random =  ~1 | FishID',
                       'lograte ~ mass, random =  ~1 | FishID',
                       'lograte ~ temp_bin + Method, random =  ~1 | FishID',
                       'lograte ~ mass + Method, random =  ~1 | FishID',
                       'lograte ~ temp_bin + mass, random =  ~1 | FishID',
                       'lograte ~ temp_bin + Method + mass, random =  ~1 | FishID')

ggdItomodel_params <- data.frame(model_name = Itomodel_var_list)


ggdItoAIC_vector <- vapply(ggdItomodel_list, AIC, numeric(1))

for (i in 1:length(ggdItomodel_list)){
  r2_val <- r2_nakagawa(ggdItomodel_list[[i]])
  ggdItomodel_params$margR2[i] <- r2_val[["R2_marginal"]]
  ggdItomodel_params$condR2[i] <- r2_val[["R2_conditional"]]
  dfAIC <- logLik(ggdItomodel_list[[i]])
  ggdItomodel_params$df[i] <- attr(dfAIC, "df")
  ggdItomodel_params$AIC[i] <- AIC(ggdItomodel_list[[i]])
  ggdItomodel_params$deltaAIC[i] <- (AIC(ggdItomodel_list[[i]]) - min(ggdItoAIC_vector))
}

ggdItomodel_params <- ggdItomodel_params %>%
  arrange(deltaAIC)  %>%
  mutate(margR2 = round(margR2, 3),
         condR2 = round(condR2, 3),
         AIC = round(AIC, 3),
         deltaAIC = round(deltaAIC, 3))


colnames(ggdItomodel_params) <- c('Model', 'Marginal R²', 'Conditional R²',
                               'df', 'AIC', '\u0394AIC')

#write.csv(ggdItomodel_params, file = 'Ito_Model.csv')



############# Masu Models ##############

ggdMamodel1 <- lme(lograte_ggd ~ Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel2 <- lme(lograte_ggd ~ temp_bin, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel3 <- lme(lograte_ggd ~ mass, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel4 <- lme(lograte_ggd ~ temp_bin + Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel5 <- lme(lograte_ggd ~ mass + Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel6 <- lme(lograte_ggd ~ temp_bin + mass, random =  ~1 | FishID,
                data = two_year_data_Masu)
ggdMamodel7 <- lme(lograte_ggd ~ temp_bin + Method + mass, random =  ~1 | FishID,
                data = two_year_data_Masu)

AIC(ggdMamodel1, ggdMamodel2, ggdMamodel3, ggdMamodel4, ggdMamodel5, ggdMamodel6, ggdMamodel7)

lrtest(ggdMamodel1, ggdMamodel2, ggdMamodel3, ggdMamodel4, ggdMamodel5, ggdMamodel6, ggdMamodel7)



ggdMamodel_list <- mget(c('ggdMamodel1', 'ggdMamodel2', 'ggdMamodel3', 'ggdMamodel4',
                       'ggdMamodel5', 'ggdMamodel6', 'ggdMamodel7'))
ggdMamodel_list_str <- c('ggdMamodel1', 'ggdMamodel2', 'ggdMamodel3', 'Mamodel4',
                      'ggdMamodel5', 'ggdMamodel6', 'ggdMamodel7')
ggdMamodel_var_list <- c('lograte ~ Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin, random =  ~1 | FishID',
                      'lograte ~ mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method, random =  ~1 | FishID',
                      'lograte ~ mass + Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin + mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method + mass, random =  ~1 | FishID')

ggdMamodel_params <- data.frame(model_name = ggdMamodel_var_list)

ggdMaAIC_vector <- vapply(ggdMamodel_list, AIC, numeric(1))
#test <- r2_nakagawa(ggdMamodel1)

for (i in 1:length(ggdMamodel_list)){
  r2_val <- r2_nakagawa(ggdMamodel_list[[i]])
  ggdMamodel_params$margR2[i] <- r2_val[["R2_marginal"]]
  ggdMamodel_params$condR2[i] <- r2_val[["R2_conditional"]]
  dfAIC <- logLik(ggdMamodel_list[[i]])
  ggdMamodel_params$df[i] <- attr(dfAIC, "df")
  ggdMamodel_params$AIC[i] <- AIC(ggdMamodel_list[[i]])
  ggdMamodel_params$deltaAIC[i] <- (AIC(ggdMamodel_list[[i]]) - min(ggdMaAIC_vector))
}

ggdMamodel_params <- ggdMamodel_params %>%
  arrange(deltaAIC) %>%
  mutate(margR2 = round(margR2, 3),
         condR2 = round(condR2, 3),
         AIC = round(AIC, 3),
         deltaAIC = round(deltaAIC, 3))

colnames(ggdMamodel_params) <- c('Model', 'Marginal R²', 'Conditional R²',
                              'df', 'AIC', '\u0394AIC')

#write.csv(ggdMamodel_params, file = 'Masu_Model.csv')

########### Prediction Visualizing #######

require(gridExtra)


##### Ito
summary(ggdItomodel2) #RA is 0.70, RQ is 0.0466, no RB
                  #ggdRA is -5.83, RQ is still 0.0466
ItoRA <- ggdItomodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
ItoRQ <- ggdItomodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model


x_vals_ito <- seq(5, 25, by = 0.5)

newdata_ito <- data.frame(temp_bin = x_vals_ito)

pred_vals_ito <- predict(ggdItomodel2, newdata = newdata_ito, level = 0)

line_vals_ito <- data.frame(
  x = x_vals_ito,
  y = exp(pred_vals_ito)
)

str(pred_vals_ito)
dim(pred_vals_ito)




#Raw Data Plot
plot_data_int <- two_year_data_Ito %>%
  filter(Method == 'Intermittent') 


plot_data_stat <- two_year_data_Ito %>%
  filter(Method == 'Static') 

plot1 <- ggplot(two_year_data_Ito, aes(x = temp_bin, y = rate_ggd)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             shape = 1, size = 1 , alpha = 0.4) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             shape = 2, size  = 1, alpha = 0.4) +
  geom_line(data = line_vals_ito, aes(x = x, y = y),
            color = "blue", linewidth = 1.2) + 
  xlim(5,25) + 
  ylim(0,0.015) +
  theme_minimal()
labs(title = "Ito raw data with Model 2",
     shape = "Data Type") + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot1



### Summarized by temp
plot_data_int <- two_year_data_Ito %>%
  filter(Method == 'Intermittent') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_ggd, na.rm = T),
            sd = sd(rate_ggd, na.rm = T))


plot_data_stat <- two_year_data_Ito %>%
  filter(Method == 'Static') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_ggd, na.rm = T),
            sd = sd(rate_ggd, na.rm = T))

plot3 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             shape = 1, size = 3) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             shape = 2, size  = 2) +
  geom_line(data = line_vals_ito, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,0.012) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)) +
  labs(shape = "Data Type",
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (g O"[2]*" g"^{-1}*" day"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot3
####### Masu


summary(ggdMamodel2) #RA is -0.312, RQ is 0.095, no RB
                  #ggdRA is -6.86, RQ is still 0.095
MaRA <- ggdMamodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
MaRQ <- ggdMamodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model


x_vals_masu <- seq(5, 25, by = 0.5)

newdata_masu <- data.frame(temp_bin = x_vals)

pred_vals_masu <- predict(ggdMamodel2, newdata = newdata_masu, level = 0)

line_vals_masu <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu)
)


#Raw Data Plot
plot_data_int <- two_year_data_Masu %>%
  filter(Method == 'Intermittent') 


plot_data_stat <- two_year_data_Masu %>%
  filter(Method == 'Static') 

plot2 <- ggplot(two_year_data_Masu, aes(x = temp_bin, y = rate_ggd)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             size = 1 , alpha = 0.4) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             size  = 1, alpha = 0.4) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,0.015) +
  theme_minimal() + 
  labs(shape = "Data Type") + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot2

### Summarized by temp
plot_data_int <- two_year_data_Masu %>%
  filter(Method == 'Intermittent') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_ggd, na.rm = T),
            sd = sd(rate_ggd, na.rm = T))


plot_data_stat <- two_year_data_Masu %>%
  filter(Method == 'Static') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_ggd, na.rm = T),
            sd = sd(rate_ggd, na.rm = T))

plot4 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"), size = 3) +
  geom_point(data = plot_data_stat, aes(shape = "Static"), size  = 2) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,0.012) +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 12)) +
  labs(shape = "Data Type",
       x = 'Temperature (°C)',
       y = expression("Metabolic Rate (g O"[2]*" g"^{-1}*" day"^{-1}*")")) + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))



######## Model Grouping (Odds are Ito, Evens are Masu)


library(patchwork)
plot1 + plot2 + plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A")
plot3 + plot4 + plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A")






