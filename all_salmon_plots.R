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
                       "Chinook salmon (adult)",
                    'Rainbow Trout (adult) ')
)

formula_list <- data.frame(Species = table$Model)

intercept_vector <- c(table$Intercept..RA.)
tempvar_vector <- c(table$Temp.Function..RQ.)
temp_range <- seq(10, 25, 0.5)


predictions <- table %>%
  expand_grid(temperature = temp_range) %>%
  mutate(
    prediction = if_else(
      Model %in% c("Ito Intermittent", "Masu Intermittent", 
                   "Ito Static", "Masu Static"),
      `Intercept..RA.` * exp(`Temp.Function..RQ.` * temperature),
      `Intercept..RA.` * (150 ^ `Weight..RB.`) * exp(`Temp.Function..RQ.` * temperature)
    )
  )


table$Model

ggplot(predictions, aes(x = temperature, y = prediction, color = Model, linewidth = Model)) +
  geom_line(linewidth = Model, linetype = Model) +
  scale_color_manual(
    values = c(
      "Baikal Grayling" = 'royalblue',
      "Brown Trout" = 'lightblue',
      "Bull trout (adult) " = 'cyan',
      "Chinook salmon (adult)" = 'slateblue4',
      "Lake trout (adult)" = 'slateblue1',
      "Lenok" = 'darkorchid4',
      'Rainbow Trout (adult) ' = 'yellow',
      "Sockeye salmon (adult)" = 'darkorchid1',
      "Steelhead (adult)" = 'green3',
      "Ito Intermittent" = 'goldenrod',
      "Masu Intermittent" = 'red2',
      "Ito Static" = 'goldenrod',
      "Masu Static" = 'red2'
    )
  ) +
  scale_linewidth_manual(values = c(
    "Baikal Grayling" = 1,
    "Brown Trout" = 1,
    "Bull trout (adult) " = 1,
    "Chinook salmon (adult)" = 1,
    "Lake trout (adult)" = 1,
    "Lenok" = 1,
    'Rainbow Trout (adult) ' = 2,
    "Sockeye salmon (adult)" = 1,
    "Steelhead (adult)" = 2,
    "Ito Intermittent" = 2,
    "Masu Intermittent" = 2,
    "Ito Static" = 2,
    "Masu Static" = 2)) +
  scale_linetype_manual(values = c(
    "Baikal Grayling" = 1,
    "Brown Trout" = 1,
    "Bull trout (adult) " = 1,
    "Chinook salmon (adult)" = 1,
    "Lake trout (adult)" = 1,
    "Lenok" = 1,
    'Rainbow Trout (adult) ' = 3,
    "Sockeye salmon (adult)" = 1,
    "Steelhead (adult)" = 2,
    "Ito Intermittent" = 2,
    "Masu Intermittent" = 2,
    "Ito Static" = 2,
    "Masu Static" = 2))  
   + 
  labs(
    
    x = "Temperature(°C)",
    y = expression("Metabolic Rate (gO"[2]*" g"^{-1}*" day"^{-1}*")"),
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = 'right',
    plot.title = element_text(size = 14)
  )














ggplot(predictions, aes(x = temperature, y = prediction, color = Model)) +
  geom_line(aes(linewidth = Model, linetype = Model)) +
  scale_color_manual(
    values = c(
      "Baikal Grayling" = 'royalblue',
      "Brown Trout" = 'grey40',
      "Bull trout (adult) " = 'royalblue',
      "Chinook salmon (adult)" = 'royalblue',
      "Lake trout (adult)" = 'royalblue',
      "Lenok" = 'royalblue',
      'Rainbow Trout (adult) ' = 'green3',
      "Sockeye salmon (adult)" = 'royalblue',
      "Steelhead (adult)" = 'green2',
      "Ito Intermittent" = 'goldenrod',
      "Masu Intermittent" = 'red2',
      "Ito Static" = 'goldenrod',
      "Masu Static" = 'red2'
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Baikal Grayling" = 1,
      "Brown Trout" = 2,
      "Bull trout (adult) " = 1,
      "Chinook salmon (adult)" = 1,
      "Lake trout (adult)" = 1,
      "Lenok" = 1,
      'Rainbow Trout (adult) ' = 1.5,
      "Sockeye salmon (adult)" = 1,
      "Steelhead (adult)" = 2,
      "Ito Intermittent" = 1.5,
      "Masu Intermittent" = 1.5,
      "Ito Static" = 1.5,
      "Masu Static" = 1.5
    ),
    guide = "none"  # Hide the linewidth legend
  ) +
  scale_linetype_manual(values = c(
    "Baikal Grayling" = "solid",
    "Brown Trout" = "dotted",
    "Bull trout (adult) " = "solid",
    "Chinook salmon (adult)" = "solid",
    "Lake trout (adult)" = "solid",
    "Lenok" = "solid",
    'Rainbow Trout (adult) ' = 'solid',
    "Sockeye salmon (adult)" = "solid",
    "Steelhead (adult)" = 'dotted', # Same as Brown Trout
    "Ito Intermittent" = "solid",
    "Masu Intermittent" = "solid",
    "Ito Static" = "dashed",
    "Masu Static" = "dashed"
  )) +
  labs(
    x = "Temperature(°C)",
    y = expression("Metabolic Rate (gO"[2]*" g"^{-1}*" day"^{-1}*")"),
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_text(size = 14),
    axis.text = element_text(size = 11),
  )


