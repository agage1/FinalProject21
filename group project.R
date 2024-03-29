library(fpp3)
library(knitr)

# import data
DATA <- read.csv("credit.csv")
DATA$�..credit_in_millions <- rev(DATA$�..credit_in_millions)


# create into a tsibble
Year <- as.data.frame(rep(1900:1940,each = 12))
Month <- as.data.frame(rep(month.name,41))
test <- data.frame(Year,Month)
names(test) <- c("Year","Month")
test$Year_Month <- paste(test$Year,test$Month)
DATA <- tsibble(Time = yearmonth(test$Year_Month),
                Credit = DATA$�..credit_in_millions,
                index = Time)

# autoplot to see data
autoplot(DATA, Credit)

# decomp data
DATA %>%
  model(
    classical_decomposition(Credit, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot()

# See if transforming helps the data
DATA %>%
  features(DATA$Credit, features = guerrero) %>%
  pull(lambda_guerrero) -> lambda 
DATA %>% 
  autoplot(box_cox(Credit, lambda))
# Does not seem to be helpful

#Model search for entire tsibble
DATA %>%
  stretch_tsibble(.init = 120,.step = 60) %>%
  model(
    "Naive" = NAIVE(Credit),
    "Seasonal Naive" = SNAIVE(Credit),
    "Holts" = ETS(Credit ~ trend()),
    "Auto" = ARIMA(Credit, stepwise = FALSE),
    "Neural" = NNETAR(Credit)
  ) %>%
  forecast(h = 18) %>%
  accuracy(DATA) %>%
  arrange(RMSE)

# Train/Holdout Split
TRAIN <- DATA %>%
  filter_index('1900 Jan' ~ '1938 Jun')
HOLDOUT <- DATA %>%
  filter_index("1938 July" ~ '1940 Dec')

# Cross validate to find the best model
TRAIN %>%
  stretch_tsibble(.init = 120,.step = 60) %>%
  model(
    "Naive" = NAIVE(Credit),
    "Seasonal Naive" = SNAIVE(Credit),
    "Holts" = ETS(Credit ~ trend()),
    "Auto" = ARIMA(Credit, stepwise = FALSE),
    "Neural" = NNETAR(Credit)
  ) %>%
  forecast(h = 18) %>%
  accuracy(DATA) %>%
  arrange(RMSE)

# fit the model on the entire data set and look at residuals
DATA %>% 
  model(
    "Neural" = NNETAR(Credit)
  ) -> MODEL
report(MODEL)
gg_tsresiduals(MODEL)

# visual the forecast of the best model
MODEL %>%
  forecast(h = 30) -> fit
autoplot(HOLDOUT, .vars = Credit) + autolayer(fit)

# make predictions
y_pred <- fit$.mean

# calculate accuracy w/ RMSE & MAPE
rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual - y_pred)^2))
}
mape <- function(y_actual, y_pred) {
  mean(abs(y_actual - y_pred) / y_actual)
}
y_actual <- HOLDOUT$Credit
rmse(y_actual = y_actual, y_pred = y_pred)
mape(y_actual = y_actual, y_pred = y_pred)


