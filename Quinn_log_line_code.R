library(tidyverse)
library(lme4)
library(nlme)

two_year_data_Ito <- read.csv("Ito_only_data.csv")

Itomodel2 <- lme(lograte ~ temp_bin, random =  ~1 | FishID,
                 data = two_year_data_Ito)    #log_transformed model


ItoRA <- Itomodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
ItoRQ <- Itomodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model


x_vals <- seq(5,25, by = 0.5)    #generating temp values

y_vals <- exp(ItoRA) * exp(ItoRQ * x_vals)  #equation for line, transformed back from log

line_vals = data.frame(x = x_vals, y = y_vals) # line df


ggplot(two_year_data_Ito, aes(x = temp_bin, y = rate_final)) +  #plotting
  geom_point() + 
  geom_line(data = line_vals, aes(x = x_vals, y = y_vals))



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







tri1_rates <- raw_df$rate_final[raw_df$trial == '1']
