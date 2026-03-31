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


Itomodel1 <- lme(lograte ~ Method, random =  ~1 | FishID,
                 data = two_year_data_Ito)
Itomodel2 <- lme(lograte ~ temp_bin, random =  ~1 | FishID,
                  data = two_year_data_Ito)
Itomodel3 <- lme(lograte ~ mass, random =  ~1 | FishID,
                  data = two_year_data_Ito)
Itomodel4 <- lme(lograte ~ temp_bin + Method, random =  ~1 | FishID,
                  data = two_year_data_Ito)
Itomodel5 <- lme(lograte ~ mass + Method, random =  ~1 | FishID,
                  data = two_year_data_Ito)
Itomodel6 <- lme(lograte ~ temp_bin + mass, random =  ~1 | FishID,
                  data = two_year_data_Ito)
Itomodel7 <- lme(lograte ~ temp_bin + Method + mass, random =  ~1 | FishID,
                  data = two_year_data_Ito)

AIC(Itomodel1, Itomodel2, Itomodel3, Itomodel4,
    Itomodel5, Itomodel6, Itomodel7)

lrtest(Itomodel1, Itomodel2, Itomodel3, Itomodel4,
       Itomodel5, Itomodel6, Itomodel7)


Itomodel_list <- mget(c('Itomodel1', 'Itomodel2', 'Itomodel3', 'Itomodel4',
                     'Itomodel5', 'Itomodel6', 'Itomodel7'))
Itomodel_list_str <- c('Itomodel1', 'Itomodel2', 'Itomodel3', 'Itomodel4',
                     'Itomodel5', 'Itomodel6', 'Itomodel7')
Itomodel_var_list <- c('lograte ~ Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin, random =  ~1 | FishID',
                      'lograte ~ mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method, random =  ~1 | FishID',
                      'lograte ~ mass + Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin + mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method + mass, random =  ~1 | FishID')

Itomodel_params <- data.frame(model_name = Itomodel_var_list)
                              

ItoAIC_vector <- vapply(Itomodel_list, AIC, numeric(1))

for (i in 1:length(Itomodel_list)){
  r2_val <- r2_nakagawa(Itomodel_list[[i]])
  Itomodel_params$margR2[i] <- r2_val[["R2_marginal"]]
  Itomodel_params$condR2[i] <- r2_val[["R2_conditional"]]
  dfAIC <- logLik(Itomodel_list[[i]])
  Itomodel_params$df[i] <- attr(dfAIC, "df")
  Itomodel_params$AIC[i] <- AIC(Itomodel_list[[i]])
  Itomodel_params$deltaAIC[i] <- (AIC(Itomodel_list[[i]]) - min(ItoAIC_vector))
}

Itomodel_params <- Itomodel_params %>%
  arrange(deltaAIC)  %>%
  mutate(margR2 = round(margR2, 3),
         condR2 = round(condR2, 3),
         AIC = round(AIC, 3),
         deltaAIC = round(deltaAIC, 3))


colnames(Itomodel_params) <- c('Model', 'Marginal R²', 'Conditional R²',
                              'df', 'AIC', '\u0394AIC')

#write.csv(Itomodel_params, file = 'low_quart_Ito_Model.csv')



############# Masu Models ##############

Mamodel1 <- lme(log(rate_final) ~ Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel2 <- lme(log(rate_final) ~ temp_bin, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel3 <- lme(log(rate_final) ~ mass, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel4 <- lme(log(rate_final) ~ temp_bin + Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel5 <- lme(log(rate_final) ~ mass + Method, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel6 <- lme(log(rate_final) ~ temp_bin + mass, random =  ~1 | FishID,
                data = two_year_data_Masu)
Mamodel7 <- lme(log(rate_final) ~ temp_bin + Method + mass, random =  ~1 | FishID,
                data = two_year_data_Masu)

AIC(Mamodel1, Mamodel2, Mamodel3, Mamodel4, Mamodel5, Mamodel6, Mamodel7)

lrtest(Mamodel1, Mamodel2, Mamodel3, Mamodel4, Mamodel5, Mamodel6, Mamodel7)



Mamodel_list <- mget(c('Mamodel1', 'Mamodel2', 'Mamodel3', 'Mamodel4',
                        'Mamodel5', 'Mamodel6', 'Mamodel7'))
Mamodel_list_str <- c('Mamodel1', 'Mamodel2', 'Mamodel3', 'Mamodel4',
                       'Mamodel5', 'Mamodel6', 'Mamodel7')
Mamodel_var_list <- c('lograte ~ Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin, random =  ~1 | FishID',
                      'lograte ~ mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method, random =  ~1 | FishID',
                      'lograte ~ mass + Method, random =  ~1 | FishID',
                      'lograte ~ temp_bin + mass, random =  ~1 | FishID',
                      'lograte ~ temp_bin + Method + mass, random =  ~1 | FishID')

Mamodel_params <- data.frame(model_name = Mamodel_var_list)

MaAIC_vector <- vapply(Mamodel_list, AIC, numeric(1))
test <- r2_nakagawa(Mamodel1)

for (i in 1:length(Mamodel_list)){
  r2_val <- r2_nakagawa(Mamodel_list[[i]])
  Mamodel_params$margR2[i] <- r2_val[["R2_marginal"]]
  Mamodel_params$condR2[i] <- r2_val[["R2_conditional"]]
  dfAIC <- logLik(Mamodel_list[[i]])
  Mamodel_params$df[i] <- attr(dfAIC, "df")
  Mamodel_params$AIC[i] <- AIC(Mamodel_list[[i]])
  Mamodel_params$deltaAIC[i] <- (AIC(Mamodel_list[[i]]) - min(MaAIC_vector))
}

Mamodel_params <- Mamodel_params %>%
  arrange(deltaAIC) %>%
  mutate(margR2 = round(margR2, 3),
         condR2 = round(condR2, 3),
         AIC = round(AIC, 3),
         deltaAIC = round(deltaAIC, 3))

colnames(Mamodel_params) <- c('Model', 'Marginal R²', 'Conditional R²',
                              'df', 'AIC', '\u0394AIC')

#write.csv(Mamodel_params, file = 'low_quart_Masu_Model.csv')

########### Prediction Visualizing #######

require(gridExtra)


##### Ito
summary(Itomodel2) #RA is 0.70, RQ is 0.0466, no RB
ItoRA <- Itomodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
ItoRQ <- Itomodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model


x_vals_ito <- seq(5, 25, by = 0.5)

newdata_ito <- data.frame(temp_bin = x_vals)

pred_vals_ito <- predict(Itomodel2, newdata = newdata_ito, level = 0)

line_vals_ito <- data.frame(
  x = x_vals_ito,
  y = exp(pred_vals_ito)
)


#Raw Data Plot
plot_data_int <- two_year_data_Ito %>%
  filter(Method == 'Intermittent') 


plot_data_stat <- two_year_data_Ito %>%
  filter(Method == 'Static') 
  
plot1 <- ggplot(two_year_data_Ito, aes(x = temp_bin, y = rate_final)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             shape = 1, size = 1 , alpha = 0.4) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             shape = 2, size  = 1, alpha = 0.4) +
  geom_line(data = line_vals_ito, aes(x = x, y = y),
            color = "blue", linewidth = 1.2) + 
  xlim(5,25) + 
  ylim(0,12) +
  theme_minimal()
  labs(title = "Ito raw data with Model 2",
       shape = "Data Type") + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot1



### Summarized by temp
plot_data_int <- two_year_data_Ito %>%
  filter(Method == 'Intermittent') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))


plot_data_stat <- two_year_data_Ito %>%
  filter(Method == 'Static') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))

plot3 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             shape = 1, size = 3) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             shape = 2, size  = 2) +
  geom_line(data = line_vals_ito, aes(x = x, y = y),
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
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot3
####### Masu


summary(Mamodel2) #RA is -0.312, RQ is 0.095, no RB
MaRA <- Mamodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
MaRQ <- Mamodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model


x_vals_masu <- seq(5, 25, by = 0.5)

newdata_masu <- data.frame(temp_bin = x_vals)

pred_vals_masu <- predict(Mamodel2, newdata = newdata_masu, level = 0)

line_vals_masu <- data.frame(
  x = x_vals_masu,
  y = exp(pred_vals_masu)
)

#Raw Data Plot
plot_data_int <- two_year_data_Masu %>%
  filter(Method == 'Intermittent') 


plot_data_stat <- two_year_data_Masu %>%
  filter(Method == 'Static') 

plot2 <- ggplot(two_year_data_Masu, aes(x = temp_bin, y = rate_final)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"),
             size = 1 , alpha = 0.4) +
  geom_point(data = plot_data_stat, aes(shape = "Static"),
             size  = 1, alpha = 0.4) +
  geom_line(data = line_vals_masu, aes(x = x, y = y),
            color = "black", linewidth = 0.8, linetype = 'dashed') + 
  xlim(5,25) + 
  ylim(0,12) +
  theme_minimal() + 
  labs(shape = "Data Type") + 
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))
plot2

### Summarized by temp
plot_data_int <- two_year_data_Masu %>%
  filter(Method == 'Intermittent') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))


plot_data_stat <- two_year_data_Masu %>%
  filter(Method == 'Static') %>%
  group_by(temp_bin) %>%
  summarize(rate = mean(rate_final, na.rm = T),
            sd = sd(rate_final, na.rm = T))

plot4 <- ggplot(plot_data, aes(x = temp_bin, y = rate)) +
  geom_point(data = plot_data_int, aes(shape = "Intermittent"), size = 3) +
  geom_point(data = plot_data_stat, aes(shape = "Static"), size  = 2) +
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
  scale_shape_manual(values = c("Intermittent" = 1, "Static" = 2))



######## Model Grouping (Odds are Ito, Evens are Masu)


library(patchwork)
plot1 + plot2 + plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A")
plot3 + plot4 + plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(tag_levels = "A")









####################### Intermittent only ################











#### Extra modelling code

####Q Stuff####
#Basically the same, but using predict to make workflow easier
#With the back transformation, just know it is biased low unless you correct for variance (generally ok)

library(nlme)

Itomodel2 <- lme(log(rate_final) ~ temp_bin,
                 random = ~1 | FishID,
                 data = two_year_data_Ito)

x_vals <- seq(5, 25, by = 0.5)

newdata <- data.frame(temp_bin = x_vals)

pred_vals <- predict(Itomodel2, newdata = newdata, level = 0)

line_vals <- data.frame(
  x = x_vals,
  y = exp(pred_vals)
)

ggplot(two_year_data_Ito, aes(x = temp_bin, y = rate_final)) +
  geom_point(alpha = 0.4) +
  geom_line(data = line_vals, aes(x = x, y = y),
            color = "blue", linewidth = 1.2)





#George's code using ggpredict which log transforms automatically for you
library(nlme)
library(ggplot2)
library(ggeffects)

m1 <- lme(log(rate_final) ~ temp_bin, random =  ~1 | FishID,
          data = Ito_data) 

plot(ggpredict(m1, terms = "temp_bin [5:25]"), show_data = T) # ggpredict back-transforms automatically if you use log() directly in the formula

###

