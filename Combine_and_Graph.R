
library(lme4)
library(nlme)
pos <- position_dodge(width = 0.5)




############# Rates over time ##########

ggplot(raw_df, aes(x = rep, y = lmratemgkgmin, color = temp)) + 
  geom_point(size = 2) + 
  facet_wrap(~FishID) + 
  theme_minimal()
  



##### Filtering  & Plotting by species######
######### rsq > 0.90
plot_data <-  raw_df%>%
  filter(rsq >0.90) %>%
  group_by(temp, Species) %>%
  summarize(rate = mean(lmratemgkgmin, na.rm = TRUE,),
            sd = sd(lmratemgkgmin, na.rm = T),
            .groups = "keep") %>%
  mutate(rate = round(rate, 3))
  


ggplot(plot_data, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3, position = pos) +
  #geom_line() +
  geom_errorbar(aes(ymin = rate - sd,
                    ymax = rate + sd),
                position = pos) +
  labs(
    title = "Trials 1-4",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  xlim(10, 25) +
  theme_classic()
             



             
################## rsq > 0.95
plot_data <-  raw_df%>%
  filter(rsq >0.95) %>%
  group_by(temp, Species) %>%
  summarise(rate = mean(lmratemgkgmin, na.rm = TRUE,
  ),
  sd = sd(lmratemgkgmin, na.rm = T)) %>%
  mutate(rate = round(rate, 3))



ggplot(plot_data, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3, position = pos) +
  #geom_line() +
  geom_errorbar(aes(ymin = rate - sd,
                    ymax = rate + sd),
                position = pos) +
  labs(
    title = "rsq = 0.95",
    x = "Temperature (°C)",
    y = expression("Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  xlim(10, 25) +
  theme_classic()

########## Checking only rates <10 #################
######## rates less than 10, mean
  
  plot_data <- raw_df%>%
    filter(lmratemgkgmin < 10) %>%
    filter(rsq > 0.95) %>%
    group_by(temp, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd = sd(lmratemgkgmin, na.rm = T)) %>%
    mutate(rate = round(rate, 3))
  
  ggplot(plot_data, aes(x = temp, y = rate, color = Species)) +
    geom_point(size = 3, position = pos) +
    #geom_line() +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd),
                  position = pos) +
    labs(
      title = "rsq = 0.95, rate < 10, mean",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    ylim(0, 10) +
    xlim(10, 25) +
    theme_classic()
  
  ###### rates less than 10, median
  
  plot_data <- raw_df%>%
    filter(lmratemgkgmin < 10) %>%
    filter(rsq > 0.95) %>%
    group_by(temp, Species) %>%
    summarise(rate = median(lmratemgkgmin, na.rm = TRUE),
              sd = sd(lmratemgkgmin, na.rm = T)) %>%
    mutate(rate = round(rate, 3))
  
  ggplot(plot_data, aes(x = temp, y = rate, color = Species)) +
    geom_point(size = 3, position = pos) +
    #geom_line() +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd),
                  position = pos) +
    labs(
      title = "rsq = 0.95, rate < 10, median",
      x = "Temperature (°C)",
      y = expression("Med Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10, 25) +
    ylim(0, 10) +
    theme_classic()
########### Individual Fish Plots #######
################ Grouping by FishID, all fish

avg_rate_raw <-  raw_df%>%
  filter(rsq >0.90) %>%
  group_by(temp,FishID) %>%
  summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
            sd = sd(lmratemgkgmin, na.rm = T)) %>%
  mutate(rate = round(rate, 3))
  
  
#adding points without filtering rsq
  avg_norsqfilter <- raw_df %>%
    group_by(temp, FishID, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd   = sd(lmratemgkgmin, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(rate = round(rate, 3))
  
  
  ggplot(avg_rate_raw, aes(x = temp, y = rate)) +
    geom_point(size = 2) +
    geom_point(data = avg_norsqfilter, aes(x = temp, y = rate),
               color = 'red', alpha = 0.3) +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd)) +
    geom_errorbar(data = avg_norsqfilter, aes(ymin = rate - sd,
                                              ymax = rate + sd),
                  color = 'red', alpha = 0.5) +
    labs(
      title = "FishID individual plots with sd",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10,25) +
    ylim(0, 15) +
    facet_wrap(~FishID) +
    theme_classic()

  
####### removing bad fish
  
  avg_rate_raw <-  raw_df%>%
    filter(rsq >0.90) %>%
    group_by(temp,FishID) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd = sd(lmratemgkgmin, na.rm = T)) %>%
    mutate(rate = round(rate, 3))
  
#  avg_rate_raw <- raw_df %>%
#    filter(rsq > 0.90) %>%
#    filter(!FishID %in% c("Masu13", "Masu12", "Ito2",
#                          "Masu11", "Masu10", "Ito11")) %>%
#    group_by(temp, FishID, Species) %>%
#    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
#              sd   = sd(lmratemgkgmin, na.rm = TRUE),
#              .groups = "drop") %>%
#    mutate(rate = round(rate, 3))
  
avg_norsqfilter <- raw_df %>%
    group_by(temp, FishID, Species) %>%
    summarise(rate = mean(lmratemgkgmin, na.rm = TRUE),
              sd   = sd(lmratemgkgmin, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(rate = round(rate, 3))
  

########## plot with rsq below 90 in red
  ggplot(avg_rate_raw, aes(x = replicate, y = rate)) +
    geom_point(size = 2) +
    geom_point(data = avg_norsqfilter, aes(x = temp, y = rate),
               color = 'red', alpha = 0.3) +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd)) +
    geom_errorbar(data = avg_norsqfilter, aes(ymin = rate - sd,
                      ymax = rate + sd),
                  color = 'red', alpha = 0.5) +
    labs(
      title = "FishID individual plots with sd",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10,25) +
    ylim(0, 15) +
    facet_wrap(~FishID) +
    theme_classic()

  
  
  
  

#############length(unique(subrsq_rm$temp))  #count number of unique temps
subrsq_rm90 <- raw_df %>%
    filter(rsq > 0.90) %>%
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
  )


##### Checking rsq again without outlier fish #########
####################graphing values above rsq = 0.9 w/o outliers
pos <- position_dodge(width = 0.5)
ggplot(subrsq_rm90, aes(x = temp, y = rate, color = Species)) +
  geom_point(size = 3, position = pos) +
  geom_errorbar(aes(ymin = rate - sd,
                    ymax = rate + sd),
                position = pos) +
  #geom_line() +
  #geom_smooth(method = 'lm') +
  labs(
    title = "Rates by Spp w/ sd, rsq > 90, no outlier fish",
    x = "Temperature (°C)",
    y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
    color = "Fish"
  ) +
  xlim(10,25) +
  ylim(0, 10) +
  theme_classic()




# Checking for rates  @0.95 rsq without outlier fish
  subrsq_rm95b <- raw_df %>%
    filter(rsq > 0.95) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                          'Masu11','Ito13')) %>% # bad apples
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
    )

  ggplot(subrsq_rm95b, aes(x = temp, y = rate, color = Species)) +
    geom_point(size = 3, position = pos) +
    geom_errorbar(aes(ymin = rate - sd,
                      ymax = rate + sd),
                  position = pos) +
    #geom_line() +
    #geom_smooth(method = 'lm') +
    labs(
      title = "Rates by Spp w/ sd, rsq > 95, no outlier fish",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10,25) +
    ylim(0, 10) +
    theme_classic()
  
################# Lm modelling ###################
mod1 <- lm(lmratemgkgmin ~ temp + FishID, data = raw_df)

# ChatGPT log-linked
  ggplot(data = raw_df, aes(x = temp, y = lmratemgkgmin, color = Species)) + 
    geom_point(alpha = 0.3, size = 2) +
    geom_smooth(method = "glm",
                method.args = list(family = gaussian(link = "log")),
                formula = y ~ poly(x^2), 
                se = FALSE) +
    labs(
      title = 'Log-linked linear raw data plotting',
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) + 
    facet_wrap(~Species) +
    theme_minimal()

  
  
  



# All raw data  
  ggplot(raw_df, aes(x = temp, y = lmratemgkgmin, color = Species)) +
    geom_point() +
    geom_smooth( se=FALSE, 
                method="lm", formula = y ~ poly(x^2)) +
    facet_wrap(~Species) +
    labs(
      title = "quadratic Model",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish") +
    theme_minimal()
  
  ############## Back to only avg. Temp @ different rsq #########
  subrsq_rm90 <- raw_df %>%
    filter(rsq > 0.90) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                          'Masu11','Ito13')) %>% # bad apples
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
    )
  
  ggplot(subrsq_rm90, aes(x = temp, y = rate, color = Species)) +
    geom_point() +
    geom_smooth(
      se = FALSE,
      method = "nls",
      formula = y ~ a * exp(b * x),
      method.args = list(start = list(a = 1, b = 0.05))
    ) +
    labs(
      title = "Exp growth by Spp, rsq > 90, no outliers",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10,25) +
    ylim(0,10) +
    facet_wrap(~Species) +
    theme_minimal()
  
  
  
# rsq > 0.95, Taking out bad apple fish, avg temp
  subrsq_rm95b <- raw_df %>%
    filter(rsq > 0.95) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                          'Masu11','Ito13')) %>% # bad apples
    group_by(Species, temp) %>%
    summarise(
      n = n(),
      rate = mean(lmratemgkgmin, na.rm = TRUE),
      sd = sd(lmratemgkgmin, na.rm = T),
      .groups = "drop"
    )
  ggplot(subrsq_rm95b, aes(x = temp, y = rate, color = Species)) +
    geom_point() +
    geom_smooth(
      se = FALSE,
      method = "nls",
      formula = y ~ a * exp(b * x),
      method.args = list(start = list(a = 1, b = 0.05))
    ) +
    labs(
      title = "Exp growthby Spp, rsq > 95, no outliers",
      x = "Temperature (°C)",
      y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")"),
      color = "Fish"
    ) +
    xlim(10,25) +
    ylim(0,10) +
    facet_wrap(~Species) +
    theme_minimal()
  
  
  
# rsq > 0.9, no Bad apples
plot_data <- raw_df %>%
    filter(rsq > 0.90) %>%
    filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                          'Masu11','Ito13')) # bad apples

ggplot(plot_data, aes(x = temp, y = lmratemgkgmin, color = Species)) +
  geom_point() +
  geom_smooth(
    se = FALSE,
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = list(a = 1, b = 0.05))
  ) +
  labs(title = "raw rates, rsq > 0.90, no outliers",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  xlim(10,25) +
  ylim(0,10) + 
  facet_wrap(~Species) +
  theme_minimal()

# rsq > 0.95, no Bad apples
plot_data <- raw_df %>%
  filter(rsq > 0.95) %>%
  filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                        'Masu11','Ito13')) # bad apples

ggplot(plot_data, aes(x = temp, y = lmratemgkgmin, color = Species)) +
  geom_point() +
  geom_smooth(
    se = FALSE,
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = list(a = 1, b = 0.05))
  ) +
  labs(title = "raw rates, rsq > 0.95, no outliers",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  ylim(0,10) + 
  facet_wrap(~Species) +
  theme_minimal()


#### Facet Wrap By FishID
plot_data <- raw_df %>%
  filter(rsq > 0.90) %>%
  filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                        'Masu11','Ito13')) # bad apples

rep_count <- plot_data %>%
  group_by(FishID) %>%
  mutate(n = n()) %>%
  ungroup()

ggplot(plot_data, aes(x = temp, y = lmratemgkgmin, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    se = FALSE,
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = list(a = 1, b = 0.05))
  ) +
  labs(title = "exp fit by FishID, rsq > 0.90, no outliers",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  facet_wrap(~paste0(FishID, " (n = ", rep_count$n, ")")) +
  xlim(10,25) +
  ylim(0,10) +
  theme_minimal()


#### Facet Wrap By FishID
plot_data <- raw_df %>%
  filter(rsq > 0.95) %>%
  filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                        'Masu11','Ito13')) # bad apples

rep_count <- plot_data %>%
  group_by(FishID) %>%
  mutate(n = n()) %>%
  ungroup()

ggplot(plot_data, aes(x = temp, y = lmratemgkgmin, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    se = FALSE,
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = list(a = 1, b = 0.05))
  ) +
  labs(title = "exp fit by FishID, rsq > 0.95, no outliers",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  facet_wrap(~paste0(FishID, " (n = ", rep_count$n, ")")) +
  xlim(10,25) +
  ylim(0,10) +
  theme_minimal()




plot_data <- raw_df %>%
  filter(lmratemgkgmin < 10) %>%
  filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2',
                        'Masu11','Ito13')) # bad apples

ggplot(plot_data, aes(x = temp, y = lmratemgkgmin, color = Species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    se = FALSE,
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = list(a = 1, b = 0.05))
  ) +
  labs(title = "exp fit by Spp, rates < 10, no outliers",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  facet_wrap(~Species) +
  xlim(10,25) + 
  ylim(0,10) + 
  theme_minimal()


########## Creating models of the raw data 

# Model 1: Fixed slope, random intercept
fit1 <- lme(lmratemgkgmin ~ temp, random = ~1 | FishID, data = raw_df)

# Model 2: Random effects for intercept and slope, no interaction for Length
fit2 = lme(lmratemgkgmin ~ temp, random = ~temp | FishID, data = raw_df)


# Model 3: Random intercept and slope, with interaction term for Length
fit3 = lme(lmratemgkgmin ~ temp * rep, random = ~temp | FishID, data = raw_df)


anova(fit1, fit2, fit3)  




########## Creating models of the filter by rsq


model_data <- raw_df %>%
  filter(rsq > 0.9) %>%
  filter(!FishID %in% c("Masu13", 'Masu12', 'Ito2', 'Masu11')) %>%
  group_by(FishID) %>%
  filter(n() > 4) %>%
  ungroup() # bad apples




# Model 1: Fixed slope, random intercept
fit1 <- lme(lmratemgkgmin ~ temp, random = ~1 | FishID, data = model_data)

# Model 2: Random effects for intercept and slope, no interaction for Length
fit2 = lme(lmratemgkgmin ~ temp, random = ~temp | FishID,
           data = model_data,
           control = lmeControl(maxIter = 100, msMaxIter = 100))


# Model 3: Random intercept and slope, with interaction term for Length
fit3= lme(lmratemgkgmin ~ temp * rep, random = ~temp | FishID,
           data = model_data,
           control = lmeControl(maxIter = 100, msMaxIter = 100))




anova(fit1, fit2, fit3)  







####### CHatGPT Model Fitting
library(nlme)

model_data$temp_c <- scale(model_data$temp, center = TRUE, scale = FALSE)

fit2 <- lme(
  log(lmratemgkgmin) ~ temp_c,
  random = ~ temp_c | FishID,
  data = model_data)

newdat <- expand.grid(
  temp_c = seq(min(model_data$temp_c),
               max(model_data$temp_c),
               length.out = 100),
  FishID = unique(model_data$FishID))

newdat$pred <- predict(fit2, newdat, level = 1)
newdat$pred_rate <- exp(newdat$pred)

ggplot(model_data, aes(x = temp, y = lmratemgkgmin)) +
  geom_point(alpha = 0.6) +
  geom_line(data = newdat,
            aes(x = temp_c + mean(model_data$temp),  # un-center
                y = pred_rate,
                group = FishID),
            color = "red",
            size = 1.2) +
  labs(title = "exp fit by individual",
       y = expression("Mean Oxygen consumption (mg O"[2]*" kg"^{-1}*" min"^{-1}*")")) +
  facet_wrap(~FishID) +
  theme_minimal()
  
