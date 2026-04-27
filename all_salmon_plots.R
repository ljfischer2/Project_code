library(tidyverse)
library(lme4)
library(nlme)
library(patchwork)
library(lmtest)
library(ggfortify)
library(performance)
require(gridExtra)

setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/Project_code")


################ Importing table #################
table <- read.csv('Biogen_salmon_table.csv')
table <- table %>%
  filter(!Model %in% c('Brook Trout (juvenile & adult)',
                       'Rainbow Trout (adult) ')
)

formula_list <- data.frame(Species = table$Model)

intercept_vector <- c(table$Intercept..RA.)
tempvar_vector <- c(table$Temp.Function..RQ.)
temp_range <- seq(10, 25, 0.5)


predictions <- table %>%
  expand_grid(temperature = temp_range) %>%
  mutate(
    # Linear model: y = intercept + temperature_coef * temperature
    prediction = Intercept..RA. * exp(Temp.Function..RQ. * temperature)
  )

table$Model

ggplot(predictions, aes(x = temperature, y = prediction, color = Model)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = c(
      "Baikal Grayling" = 'royalblue',
      "Brown Trout" = 'lightblue',
      "Bull trout (adult) " = 'cyan',
      "Chinook salmon (adult)" = 'slateblue4',
      "Lake trout (adult)" = 'slateblue1',
      "Lenok" = 'darkorchid4',
      "Sockeye salmon (adult)" = 'darkorchid1',
      "Steelhead (adult)" = 'seagreen3',
      "Ito Intermittent" = 'yellow2',
      "Masu Intermittent" = 'red2',
      "Ito Static" = 'yellow2',
      "Masu Static" = 'red2'
    )
  ) +
  labs(
    
    x = "Temperature",
    y = expression("Metabolic Rate (gO"[2]*" g"^{-1}*" day"^{-1}*")"),
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14)
  )
