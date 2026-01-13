#Libraries ####
library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(lme4) #for lmer & glmer models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) 
library(nlme)
library(ggplot2)
library(ggeffects)
#library(flexplot)
# Importing & RBinds ####
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/projectrepo")


df23 <- read.csv("Resp_23_data.csv")


dfMasu_All <- rbind(dfMasu1,dfMasu2,dfMasu3,dfMasu4,dfMasu5,dfMasu6,dfMasu7,
                 dfMasu9,dfMasu10,dfMasu11,dfMasu12,dfMasu13)
dfMasu_All$Method <- "Intermittent"
dfMasu_All$rate_output <-  dfMasu_All$rate_output / 1000

##


dfIto_Int <- rbind(dfIto1,dfIto2,dfIto3,dfIto4,dfIto5,dfIto6,dfIto7,dfIto8,
                   dfIto9,dfIto10,dfIto11,dfIto12,dfIto13)

dfIto_Int$Method <- "Intermittent"
dfIto_Int$rate_output <- dfIto_Int$rate_output / 1000
dfIto_All <- dfIto_Int

##


dfFish_All <- rbind(dfIto_All, dfMasu_All, dfBlank, df23)
dfFish_All$rate_num <- seq(from = 1, to = 1300, by = 1)

dfFish_All$rate_output <- abs(dfFish_All$rate_output)

###Only Intermittent Fish #####
dfFish_2024 <- rbind(dfIto_All, dfMasu_All, dfBlank)
dfFish_2024$rate_output <- abs(dfFish_2024$rate_output)

dfFish_noB <- rbind(dfIto_All, dfMasu_All, df23)
dfFish_noB$rate_output <- abs(dfFish_noB$rate_output)

#dfMasu_All$rate_output <- abs(dfMasu_All$rate_output)
#dfIto_All$rate_output <- abs(dfIto_All$rate_output)




#write.csv(dfFish_All,"~/Project stuff/LucasProject/projectrepo/dfFish_All.csv", row.names = FALSE)


#hist(dfFish_All$rate_output)
#Initial plotting of Rates ####
#?t.test
t.test(dfFish_All$rate_output, df23$rate_output)


ggplot(dfFish_All, aes(x = temp, y = rate_output, color = Species)) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(group = FishID), se=FALSE, method="lm", formula = y ~ poly(x^2)) +
  theme_minimal() +
  facet_wrap(~Species)

#Plot with no blank measurements
ggplot(dfFish_noB, aes(x = temp, y = rate_output, color = Species)) +
  geom_point(aes(color = Method)) +
  geom_smooth(aes(group = Species), se=FALSE, method="lm", formula = y ~ poly(x^2)) +
  theme_minimal() +
  facet_wrap(~Species) + 
  labs(title = 'Metabolic Rate',
       x = 'Temperature (Celsius)',
       y = 'Respiration(g_g_day) ') +
  theme(text = element_text(size = 20)) 

#ggplot(dfIto_All, aes(x = temp, y = rate_output, color = FishID)) +
#    geom_point(aes(color = FishID)) +
#    geom_smooth(se=FALSE, method="lm", formula = y ~ poly(x^2)) +
#    facet_wrap(~Species)
#  theme_minimal()
  
#Data manipulation in order to make modelling easier(not necessary?)####  
  
mean(dfIto_All$rate_output[dfIto_All$temp_bin == 23])
mean(dfIto_All$temp)
mean(dfMasu_All$rate_output[dfMasu_All$temp_bin == 23], na.rm = TRUE)
mean(dfMasu_All$temp)

#dfFish_All$Method <- "Intermittent"

dfFish_All$FishID <- as.factor(dfFish_All$FishID)
dfFish_All$Species <- as.factor(dfFish_All$Species)
dfFish_All_nona <- na.omit(dfFish_All)
dfFish_All_nona$trial <- as.factor(dfFish_All_nona$trial)
?nlme

dfFish_All <- na.omit(dfFish_All)

#start_val = c(temp = 10, ) 

#Model Creation and plotting ####

nlm1 <- nlme(rate_output ~ temp + FishID, data = dfFish_All,
              random = temp ~ FishID, start = NULL,
             fixed = temp ~ 1)




lm1 <- lmer(log(rate_output) ~ temp + (1 | trial), data = dfIto_All)
summary(lm1)
plot(lm1)
plot(ggpredict(lm1)) + 
  geom_point(data = dfIto_All, aes(x = temp, y = rate_output))


lm2 <- lmer(log(rate_output) ~ temp +  (1 | FishID), data = dfIto_All)
summary(lm2)
plot(lm2)
plot(ggpredict(lm2)) + 
  geom_point(data = dfIto_All, aes(x = temp, y = rate_output))


lm3 <- lmer(log(rate_output) ~ temp + (1 | trial) + (1 | FishID), data = dfIto_All)
summary(lm3)
plot(lm3)
plot(ggpredict(lm3)) + 
  geom_point(data = dfIto_All, aes(x = temp, y = rate_output))




mean(dfBlank$rate_output)



effects_fish <- effects::effect(term = "temp", mod = lm2)
summary(effects_fish)

as.data.frame(effects_fish)

x_fish <- as.data.frame(effects_fish)

ggplot() + 
  geom_point(data = dfFish_All, aes(temp, rate_output)) + 
  geom_point(data = x_fish, aes(temp, y = fit), color = "blue") + 
  geom_line(data = x_fish, aes(temp, y = fit), color = "blue") + 
  geom_ribbon(data = x_fish, aes(temp, ymin = lower, ymax = upper), alpha = 0.3, fill = "blue")


################################################################################
#Auto regression model####
auto_mod <- lme(
  rate_output ~ FishID * temp,
  random = ~ 1 | Species,
  correlation = corAR1(form = ~ rate_num | Species),
  data = dfFish_All_nona,
  method = "REML"
)

plot(auto_mod)
#model error, checking fotemp_val#model error, checking for colinearity
lme4::lmer(rate_output ~ FishID * temp * trial + (rate_num|Species), data = dfFish_All_nona)
X <- model.matrix(~ Species * Method * temp, data = dfFish_All_nona)
caret::findLinearCombos(X)
#cool, but idk how to interpret this result

#answer source code
lme4::lmer(Hour~Age*Ethnicity*GHUsedFlag*Disability + (Age|Id), data=mydf)

X <- model.matrix(~Age*Ethnicity*GHUsedFlag*Disability, data=mydf)
caret::findLinearCombos(X)
