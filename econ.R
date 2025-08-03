# ===================================================
# 1. Load Libraries
# ===================================================
library(tidyverse)
library(readxl)
library(fpp3)
library(stargazer)
library(lmtest)
library(sandwich)
library(forecast)

# ===================================================
# 2. Load and Prepare Data
# ===================================================
setwd("C:/Users/Dhruv/Documents/CLG/econometrics")

# Read and tidy the Excel data
df <- read_excel("MST2074_A2_Data.xlsx", col_names = TRUE)

df_long <- df %>%
  pivot_longer(
    cols = -`Indicator Name`,
    names_to = "year",
    values_to = "value"
  )

df_transposed <- df_long %>%
  pivot_wider(
    names_from = `Indicator Name`, 
    values_from = value
  ) %>%
  mutate(year = as.integer(year)) %>% 
  arrange(year)

# Rename columns
colnames(df_transposed) <- c("Year", "ALR", "RPR", "FAR", "PGR", "CO2", "FDI", "GDP", "TPR", "EUE", "REC")

# Convert to time series tibble
data_ts <- df_transposed %>%
  as_tsibble(index = Year)

# ===================================================
# 3. Visualization (Optional)
# ===================================================

# Plot CO2 over time
ggplot(data_ts, aes(x = Year, y = CO2)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(color = "blue") +
  labs(
    title = "CO2 Emissions Over Time (Metric Tons per Capita)",
    x = "Year",
    y = "CO2 Emissions"
  ) +
  theme_minimal()

# Plot independent variables
data_ts %>%
  select(-CO2) %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "Time Series of Independent Variables",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()

# ===================================================
# 4. Full Model (Raw CO2)
# ===================================================
raw_model <- lm(CO2 ~ ALR + RPR + FAR + PGR + FDI + GDP + TPR + EUE + REC, data = data_ts)

summary(raw_model)
bptest(raw_model)                 # Test for heteroscedasticity
stargazer(raw_model, type = "text")
checkresiduals(raw_model)
mean(raw_model$residuals)

# ===================================================
# 5. Log-Transform CO2 and Refit Model
# ===================================================
data_ts <- data_ts %>%
  mutate(log_CO2 = log(CO2))

log_model <- lm(log_CO2 ~ ALR + RPR + FAR + PGR + FDI + GDP + TPR + EUE + REC, data = data_ts)

summary(log_model)
bptest(log_model)                # Still heteroscedastic?
coeftest(log_model, vcov = vcovHC(log_model, type = "HC1"))  # Robust SEs
checkresiduals(log_model)

mean(log_model$residuals)


# ===================================================
# 6. Reduced Model (Only Significant Predictors)
# ===================================================
reduced_model <- lm(log_CO2 ~ EUE + REC, data = data_ts)

summary(reduced_model)
bptest(reduced_model)           # Heteroscedasticity resolved?
coeftest(reduced_model, vcov = vcovHC(reduced_model, type = "HC1"))
stargazer(reduced_model, type = "text")
checkresiduals(reduced_model)

mean(reduced_model$residuals)

CO2_ar_best <- data_ts %>%
  select(Year, CO2) %>%
  model(AR = AR(CO2))

report(CO2_ar_best)
Pacf(data_ts$CO2, main = "PACF of CO2")
#CO2_ar_best %>%
 # augment() %>%
  #ggplot(aes(x = Year)) +
  #geom_line(aes(y = CO2), color = "black", size = 1, linetype = "dashed") +
#  geom_line(aes(y = .fitted), color = "blue", size = 1) +
 # labs(title = "Fitted vs Actual CO₂ Emissions (AR Model)",
  #     y = "CO₂ Emissions",
   #    x = "Year") +
#  theme_minimal()

autoplot(data_ts, CO2) +
  labs(title = "CO₂ Emissions Over Time", y = "CO₂", x = "Year") +
  theme_minimal()

Pacf(data_ts$CO2, main = "PACF of CO₂ Emissions")

Acf(data_ts$CO2, main = "ACF of CO₂ Emissions")

data_ts <- data_ts %>%
  mutate(diff_CO2 = difference(CO2))

autoplot(data_ts, diff_CO2) +
  labs(title = "First Difference of CO₂ Emissions", y = "ΔCO₂", x = "Year") +
  theme_minimal()

data_ts <- data_ts %>%
  mutate(diff_CO2 = difference(CO2))

Pacf(data_ts$CO2, main = "PACF of CO₂")
Acf(data_ts$CO2, main = "ACF of CO₂ Emissions")


Acf(data_ts$diff_CO2, main = "ACF of Differenced CO₂")
Pacf(data_ts$diff_CO2, main = "PACF of Differenced CO₂")

data_ts_clean <- data_ts %>%
  filter(!is.na(diff_CO2))

ar_model <- data_ts_clean %>%
  model(AR3 = AR(diff_CO2 ~ order(3)))

resids <- ar_model %>%
  augment() %>%
  pull(.resid)

Box.test(resids, lag = 10, type = "Ljung-Box", fitdf = 3)
