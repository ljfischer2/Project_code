<<<<<<< HEAD
# Install and load required packages
install.packages("Matrix")
install.packages("lme4")
library(lme4)

# Generate sample data
set.seed(123)
num_individuals <- 50
num_measurements <- 4
data <- data.frame(
  ID = rep(1:num_individuals, each = num_measurements),
  Measurement = rep(1:num_measurements, times = num_individuals),
  Variable1 = rnorm(num_individuals * num_measurements, mean = 10, sd = 2),
  Variable2 = rnorm(num_individuals * num_measurements, mean = 20, sd = 3)
)

?Random


#Random# Perform linear mixed-effects model
model <- lmer(Variable1 ~ Variable2 + (1 | ID), data = data)

# Summarize the model
summary(model)

plot(model)























?as.factor
JPS_Data <- read.csv("May2023_data_analysis (1).csv")
JPS_Data1 <- factor(JPS_Data$Species,
                    labels = c("Oncorhynchus masou",
                               "Parahucho perryi",
                               "Salvelinus leucomaenis"))
JPS_Data1

JPS_DataPAPE <- JPS_Data1[JPS_Data1 == "Parahucho perryi"]
JPS_DataPAPE

JPS_DataSALE <- JPS_Data1[JPS_Data1 == "Salvelinus leucomaenis"]
JPS_DataSALE

JPS_DataONMA <- JPS_Data1[JPS_Data1 == "Oncorhynchus masou"]
JPS_DataONMA

JPS_Data <- JPS_Data[order(JPS_Data$species), ]

SALE <- JPS_Data[, 10 == "Salvelinus leucomaenis"]

=======
# Install and load required packages
install.packages("Matrix")
install.packages("lme4")
library(lme4)

# Generate sample data
set.seed(123)
num_individuals <- 50
num_measurements <- 4
data <- data.frame(
  ID = rep(1:num_individuals, each = num_measurements),
  Measurement = rep(1:num_measurements, times = num_individuals),
  Variable1 = rnorm(num_individuals * num_measurements, mean = 10, sd = 2),
  Variable2 = rnorm(num_individuals * num_measurements, mean = 20, sd = 3)
)

?Random


#Random# Perform linear mixed-effects model
model <- lmer(Variable1 ~ Variable2 + (1 | ID), data = data)

# Summarize the model
summary(model)

plot(model)























?as.factor
JPS_Data <- read.csv("May2023_data_analysis (1).csv")
JPS_Data1 <- factor(JPS_Data$Species,
                    labels = c("Oncorhynchus masou",
                               "Parahucho perryi",
                               "Salvelinus leucomaenis"))
JPS_Data1

JPS_DataPAPE <- JPS_Data1[JPS_Data1 == "Parahucho perryi"]
JPS_DataPAPE

JPS_DataSALE <- JPS_Data1[JPS_Data1 == "Salvelinus leucomaenis"]
JPS_DataSALE

JPS_DataONMA <- JPS_Data1[JPS_Data1 == "Oncorhynchus masou"]
JPS_DataONMA

JPS_Data <- JPS_Data[order(JPS_Data$species), ]

SALE <- JPS_Data[, 10 == "Salvelinus leucomaenis"]

>>>>>>> 4ab97b844f7b4f7c69d7f3d9be8f4017901363c7
