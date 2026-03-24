
Ito_data <- read.csv("Ito_only_data.csv")

Itomodel2 <- lme(lograte ~ temp_bin, random =  ~1 | FishID,
                 data = Ito_data)    #log_transformed model


ItoRA <- Itomodel2[["coefficients"]][["fixed"]][[1]] #Value is from linear log model
ItoRQ <- Itomodel2[["coefficients"]][["fixed"]][[2]] #Value is from linear log model
 

x_vals <- seq(5,25, by = 0.5)    #generating temp values

y_vals <- exp(ItoRA) * exp(ItoRQ * x_vals)  #equation for line, transformed back from log

line_vals = data.frame(x = x_vals, y = y_vals) # line df


ggplot(Ito_data, aes(x = temp_bin, y = rate_final)) +  #plotting
  geom_point() + 
  geom_line(data = line_vals, aes(x = x, y = y)) + 
  labs(title = "Ito points with Best Fit Model")
