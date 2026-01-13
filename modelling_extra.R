q10_model <- function(temp, R0, Q10) {
  R0 * Q10^((temp - 20) / 10)
}

start_vals <- c(R0 = 1, Q10 = .196)  # choose based on data

modfit <- nlme(
  rate_output ~ q10_model(temp, R0, Q10),
  data = dfFish_All,
  fixed = R0 + Q10 ~ 1,                           # fixed effects
  random = R0 + Q10 ~ 1 | Species/FishID,     # random by individual nested in species
  start = start_vals,
  control = nlmeControl(pnlsTol = 0.1, maxIter = 200)
)

summary(modfit)
plot(modfit)
