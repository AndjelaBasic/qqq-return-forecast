#==============================================================================#
# SFD Final Project (MCF2024)                                                  #
# Group 9 (Andjela Basic, Luka Basta, Dusan Cukalovic & Ratko Nikolic)         #
#==============================================================================#

# Phase 1: Data collection and preparation ####
# 1.1 Load the necessary libraries #####
library(quantmod)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tseries)

# 1.2 Gathering dependent variable  and potential explanatory variables data ####

# Set the date range
start_date <- as.Date("2021-05-01")
end_date <- as.Date("2024-04-30")

# Get QQQ data
getSymbols("QQQ", src = "yahoo", from = start_date, to = end_date)
qqq_data <- data.frame(date = index(QQQ), coredata(QQQ))

# Get S&P 500 Index data
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)
sp500_data <- data.frame(date = index(GSPC), coredata(GSPC))

# Get NASDAQ Composite Index data
getSymbols("^IXIC", src = "yahoo", from = start_date, to = end_date)
nasdaq_data <- data.frame(date = index(IXIC), coredata(IXIC))

# Get VIX data
getSymbols("^VIX", src = "yahoo", from = start_date, to = end_date)
vix_data <- data.frame(date = index(VIX), coredata(VIX))

# Get Fama-French factors from Kenneth French's data library
ff_factors <- read.csv("F-F_Research_Data_5_Factors_2x3_daily.CSV", skip = 3)

# Format date column
ff_factors$date <- as.Date(as.character(ff_factors$X), format="%Y%m%d")

# Filter date range
ff_factors <- ff_factors %>%
  filter(date >= start_date & date <= end_date)

# Get momentum factor from Kenneth French's data library
momentum <- read.csv("F-F_Momentum_Factor_daily.CSV", skip = 13)

# Format date column
momentum$date <- as.Date(as.character(momentum$X), format="%Y%m%d")

# Filter date range
momentum <- momentum %>%
  filter(date >= start_date & date <= end_date)

# Merge Fama-French factors with momentum factor
ff_factors <- ff_factors %>%
  left_join(momentum %>% select(date, Mom), by = "date")

# Merge all data into a single dataframe
merged_data <- qqq_data %>%
  left_join(sp500_data, by = "date", suffix = c(".QQQ", ".SP500")) %>%
  left_join(nasdaq_data, by = "date", suffix = c("", ".NASDAQ")) %>%
  left_join(vix_data, by = "date", suffix = c("", ".VIX")) %>%
  left_join(ff_factors, by = "date")

# Select relevant columns
relevant_columns <- c("date",
                      "QQQ.Adjusted",
                      "GSPC.Adjusted",
                      "IXIC.Adjusted",
                      "VIX.Adjusted",
                      "Mkt.RF",
                      "SMB",
                      "HML",
                      "RMW",
                      "CMA",
                      "RF",
                      "Mom")

# Keep only the relevant columns
filtered_data <- merged_data %>% select(all_of(relevant_columns))

# Rename columns for easier reference
filtered_data <- filtered_data %>%
  rename(QQQ = QQQ.Adjusted,
         SP500 = GSPC.Adjusted,
         NASDAQ = IXIC.Adjusted,
         VIX = VIX.Adjusted,
         MKTRF = Mkt.RF,
         MOM = Mom)

# Print the first few rows of the filtered data to verify
print(head(filtered_data))

# Calculate the number of missing values for each column
missing_values <- filtered_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# Transpose the result for better readability
missing_values <- as.data.frame(t(missing_values))
colnames(missing_values) <- "Missing Values"

# Print the number of missing values for each column
print(missing_values)

# 1.3 Checking for stationarity and calculating log returns ####

# Calculate log returns for QQQ
filtered_data <- filtered_data %>%
  mutate(QQQ_log_return = log(QQQ) - lag(log(QQQ)))

# Drop NA values
filtered_data <- na.omit(filtered_data)

# Perform ADF test on QQQ Adjusted Closing Price
adf_test_price <- adf.test(filtered_data$QQQ)
print(adf_test_price)

# Perform ADF test on QQQ log returns
adf_test_log_return <- adf.test(filtered_data$QQQ_log_return)
print(adf_test_log_return)

# Plot QQQ Adjusted Closing Price and Log Returns
plot_qqq_price <- ggplot(filtered_data, aes(x = date, y = QQQ)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "QQQ Adjusted Closing Price", x = "Date", y = "Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_qqq_log_return <- ggplot(filtered_data, aes(x = date, y = QQQ_log_return)) +
  geom_line(color = "red", size = 1) +
  labs(title = "QQQ Log Returns", x = "Date", y = "Log Return") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Print plots
grid.arrange(plot_qqq_price, plot_qqq_log_return, ncol = 1)

# 1.4 Plotting QQQ log returns and the gathered explanatory variables ####

plot_sp500 <- ggplot(filtered_data, aes(x = date, y = SP500)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "S&P 500 Adjusted Closing Price", x = "Date", y = "Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_nasdaq <- ggplot(filtered_data, aes(x = date, y = NASDAQ)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "NASDAQ Adjusted Closing Price", x = "Date", y = "Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_vix <- ggplot(filtered_data, aes(x = date, y = VIX)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "VIX Adjusted Closing Price", x = "Date", y = "Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_market <- ggplot(filtered_data, aes(x = date, y = MKTRF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Fama-French Market Factor", x = "Date", y = "Market Risk Premium") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_smb <- ggplot(filtered_data, aes(x = date, y = SMB)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "SMB Factor", x = "Date", y = "SMB") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_hml <- ggplot(filtered_data, aes(x = date, y = HML)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "HML Factor", x = "Date", y = "HML") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_rmw <- ggplot(filtered_data, aes(x = date, y = RMW)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "RMW Factor", x = "Date", y = "RMW") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_cma <- ggplot(filtered_data, aes(x = date, y = CMA)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "CMA Factor", x = "Date", y = "CMA") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

plot_mom <- ggplot(filtered_data, aes(x = date, y = MOM)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Momentum Factor", x = "Date", y = "Momentum") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Arrange plots in a 5x2 grid
grid.arrange(plot_qqq_log_return, plot_sp500, plot_nasdaq,
             plot_vix, plot_market, plot_smb,
             plot_hml, plot_rmw, plot_cma, plot_mom, nrow = 5, ncol = 2)



# 1.5 Save the prepared data ####
write.csv(filtered_data, "sfd_final_project_data.csv", row.names = FALSE)


# Phase 2: Building univariate time series model for forecast ####
# 2.1 Load the necessary libraries ####
library(quantmod)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tseries)
library(forecast)
library(dplyr)

# 2.2 Train / test split ####

data <- read.csv("sfd_final_project_data.csv")
split_ratio <- 0.8
split_index <- floor(split_ratio * nrow(data))
train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

# 2.3 Autocorrelation analysis ####

acf(train_data$QQQ_log_return, main = "ACF of QQQ Log Returns")
pacf(train_data$QQQ_log_return, main = "PACF of QQQ Log Returns")

# Calculate ACF and PACF values
acf_values <- acf(train_data$QQQ_log_return, plot = FALSE)
pacf_values <- pacf(train_data$QQQ_log_return, plot = FALSE)

# Convert ACF and PACF values to dataframes
acf_df <- data.frame(lag = acf_values$lag, acf = acf_values$acf)
pacf_df <- data.frame(lag = pacf_values$lag, pacf = pacf_values$acf)

# Print the dataframes
print("ACF values:")
print(acf_df)

print("PACF values:")
print(pacf_df)

# 2.4 Model fitting #####

log_returns = train_data$QQQ_log_return

# Initialize dataframes to store results
ar_results <- data.frame(Model = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
ma_results <- data.frame(Model = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
arma_results <- data.frame(Model = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)

# Fit AR models (including p up to 10)
for (p in c(1:10)) {
  model <- arima(log_returns, order = c(p, 0, 0))
  ar_results <- rbind(ar_results, data.frame(Model = paste("AR(", p, ")", sep = ""), AIC = AIC(model), BIC = BIC(model)))
}

# Fit MA models (including q up to 10)
for (q in c(1:10)) {
  model <- arima(log_returns, order = c(0, 0, q))
  ma_results <- rbind(ma_results, data.frame(Model = paste("MA(", q, ")", sep = ""), AIC = AIC(model), BIC = BIC(model)))
}

# Fit ARMA models (p and q up to 10)
for (p in 1:10) {
  for (q in 1:10) {
    model <- arima(log_returns, order = c(p, 0, q))
    arma_results <- rbind(arma_results, data.frame(Model = paste("ARMA(", p, ",", q, ")", sep = ""), AIC = AIC(model), BIC = BIC(model)))
  }
}

# Select top 3 models by AIC and BIC for AR
top_ar_aic <- ar_results[order(ar_results$AIC), ][1:3, ]
top_ar_bic <- ar_results[order(ar_results$BIC), ][1:3, ]

# Select top 3 models by AIC and BIC for MA
top_ma_aic <- ma_results[order(ma_results$AIC), ][1:3, ]
top_ma_bic <- ma_results[order(ma_results$BIC), ][1:3, ]

# Select top 3 models by AIC and BIC for ARMA
top_arma_aic <- arma_results[order(arma_results$AIC), ][1:3, ]
top_arma_bic <- arma_results[order(arma_results$BIC), ][1:3, ]

# Combine top models into a single dataframe
top_models <- rbind(
  cbind(top_ar_aic, Metric = "AIC"),
  cbind(top_ar_bic, Metric = "BIC"),
  cbind(top_ma_aic, Metric = "AIC"),
  cbind(top_ma_bic, Metric = "BIC"),
  cbind(top_arma_aic, Metric = "AIC"),
  cbind(top_arma_bic, Metric = "BIC")
)

# Print the top models
print(top_models)

# Models ranked by AIC and by BIC
aic_table <- top_models %>% 
  filter(Metric == "AIC") %>%
  arrange(AIC) %>%
  select(Model, AIC)
print(aic_table)

bic_table <- top_models %>% 
  filter(Metric == "BIC") %>%
  arrange(BIC) %>%
  select(Model, BIC)
print(bic_table)

# 2.5 Model evaluation ####

# Fit the best models: AR(1), MA(1), ARMA(1,1), ARMA(5,8), ARMA(5,9), and ARMA(8,6)
ar1_model <- arima(log_returns, order = c(1, 0, 0))
ma1_model <- arima(log_returns, order = c(0, 0, 1))
arma11_model <- arima(log_returns, order = c(1, 0, 1))
arma58_model <- arima(log_returns, order = c(5, 0, 8))
arma59_model <- arima(log_returns, order = c(5, 0, 9))
arma86_model <- arima(log_returns, order = c(8, 0, 6))


# Function to check coefficient significance (that will work with fixed coefficients)
check_coefficients <- function(model) {
  coefs <- model$coef
  std_err <- sqrt(diag(model$var.coef))
  
  # Identify fixed coefficients
  fixed_coefs <- is.na(model$coef)
  
  # Full length of coefficients including fixed ones
  full_length <- length(coefs) + sum(fixed_coefs)
  
  # Initialize full length vectors
  full_coefs <- rep(NA, full_length)
  full_std_err <- rep(NA, full_length)
  
  # Populate full vectors with estimated coefficients and standard errors
  full_coefs[!fixed_coefs] <- coefs
  full_std_err[!fixed_coefs] <- std_err
  
  # Calculate t-values and p-values for estimated coefficients only
  t_values <- coefs / std_err
  p_values <- 2 * (1 - pnorm(abs(t_values)))
  
  # Initialize full length vectors for t-values and p-values
  full_t_values <- rep(NA, full_length)
  full_p_values <- rep(NA, full_length)
  full_t_values[!fixed_coefs] <- t_values
  full_p_values[!fixed_coefs] <- p_values
  
  # Determine significance levels
  stars <- ifelse(full_p_values <= 0.001, "***", 
                  ifelse(full_p_values <= 0.01, "**", 
                         ifelse(full_p_values <= 0.05, "*", "")))
  
  result <- data.frame(Coefficient = names(model$coef), 
                       Estimate = full_coefs, 
                       Std_Error = full_std_err, 
                       t_value = full_t_values, 
                       p_value = full_p_values, 
                       Significance = stars)
  
  return(result)
}

# Check coefficients for AR(1)
check_coefficients(ar1_model) 
# insignificant: all

# Check coefficients for MA(1)
check_coefficients(ma1_model) 
# insignificant: all

# Check coefficients for ARMA(1,1)
check_coefficients(arma11_model) 
# insignificant: intercept

# Check coefficients for ARMA(5,8)
check_coefficients(arma58_model)
# insignificant: intercept,  ma 3,6,7,  ar 3

# Check coefficients for ARMA(5,9)
check_coefficients(arma59_model)
# insignificant: intercept,  ma 1,3,5,6,7,8,9,  ar 1,3,5

# Check coefficients for ARMA(8,6)
check_coefficients(arma86_model)
# insignificant: intercept, ma 2,3,6,  ar 2,3,6

# proceed with ARMA(1,1), ARMA(5,8), auto arima

# Overfitting checks ARMA(2,1) and ARMA(1,2)
arma21_model <- arima(log_returns,order = c(2, 0, 1))
check_coefficients(arma21_model) # ar 2 insignificant

arma12_model <- arima(log_returns,order = c(1, 0, 2))
check_coefficients(arma12_model) # ma 2 insignificant

# Overfitting checks ARMA(6,8) and ARMA(5,9)
arma68_model <- arima(log_returns,order = c(6, 0, 8))
check_coefficients(arma68_model) # ar 6 insignificant

arma59_model <- arima(log_returns,order = c(5, 0, 9))
check_coefficients(arma59_model) # ma 9 insignificant


# Residual Checking

# Get residuals
arma11_resid <- residuals(arma11_model)
arma58_resid <- residuals(arma58_model)

# Plot residuals and their ACFs
plot(arma11_resid, type='l') 
acf(arma11_resid)
# small visual persistence at lag 9

plot(arma58_resid, type='l') 
acf(arma58_resid)
# but no visual persistence


# Perform Ljung-Box test for each model
Box.test(arma11_resid, lag = 16, type = "Ljung-Box", fitdf = 2) # accept -> no serial correlation
Box.test(arma58_resid, lag = 16, type = "Ljung-Box", fitdf = 13) # accept -> no serial correlation


# Function to calculate forecast accuracy metrics
calculate_forecast_accuracy <- function(forecasts, actuals) {
  mae <- mean(abs(forecasts - actuals))
  rmse <- sqrt(mean((forecasts - actuals)^2))
  return(list(MAE = mae, RMSE = rmse))
}

forecast_errors <- function(forecasts, actuals){
  return (actuals-forecasts)
}

# Function to perform rolling forecasts using arima models
rolling_forecast_arima <- function(train_data, test_data, h, p, q) {
  n <- length(test_data)
  forecasts <- numeric(n-h+1)
  for (j in 1:(n-h+1)) {
    k <- length(train_data)
    print(j)
    model<-arima(train_data[j:k],order=c(p,0,q), method="ML")
    forctemp<-predict(model,n.ahead=h)
    forecasts[j]<-forctemp$pred[h]
    train_data <- rbind(train_data, test_data[j])
  }
  return(forecasts)
}

# Function to perform rolling forecasts using arima models
rolling_forecast_arima_no_intercept <- function(train_data, test_data, h, p, q) {
  n <- length(test_data)
  forecasts <- numeric(n-h+1)
  for (j in 1:(n-h+1)) {
    k <- length(train_data)
    print(j)
    model<-arima(train_data[j:k],order=c(p,0,q), method="ML", include.mean = FALSE)
    forctemp<-predict(model,n.ahead=h)
    forecasts[j]<-forctemp$pred[h]
    train_data <- rbind(train_data, test_data[j])
  }
  return(forecasts)
}

log_returns_train <- train_data$QQQ_log_return
log_returns_test <- test_data$QQQ_log_return

# Evaluate ARMA(1,1) model
arma11_forecasts_1 <- rolling_forecast_arima(log_returns_train, log_returns_test, h=1, p=1, q=1)
arma11_forecasts_3 <- rolling_forecast_arima(log_returns_train, log_returns_test, h=3, p=1, q=1)

arma11_accuracy_1 <- calculate_forecast_accuracy(arma11_forecasts_1, log_returns_test)
arma11_accuracy_3 <- calculate_forecast_accuracy(arma11_forecasts_3, log_returns_test[3:length(log_returns_test)])

arma11_errors_1 <- forecast_errors(arma11_forecasts_1, log_returns_test)
arma11_errors_3 <- forecast_errors(arma11_forecasts_3, log_returns_test[3:length(log_returns_test)])

# Evaluate ARMA(5,8) model
arma58_forecasts_1 <- rolling_forecast_arima(log_returns_train, log_returns_test, h=1, p=5, q=8)
arma58_forecasts_3 <- rolling_forecast_arima(log_returns_train, log_returns_test, h=3, p=5, q=8)

arma58_accuracy_1 <- calculate_forecast_accuracy(arma58_forecasts_1, log_returns_test)
arma_accuracy_3 <- calculate_forecast_accuracy(arma58_forecasts_3, log_returns_test[3:length(log_returns_test)])

arma58_errors_1 <- forecast_errors(arma58_forecasts_1, log_returns_test)
arma58_errors_3 <- forecast_errors(arma58_forecasts_3, log_returns_test[3:length(log_returns_test)])

# Evaluate ARMA(1,1) model without intercept
arma11_forecasts_1_mod <- rolling_forecast_arima_no_intercept(log_returns_train, log_returns_test, h=1, p=1, q=1)
arma11_forecasts_3_mod <- rolling_forecast_arima_no_intercept(log_returns_train, log_returns_test, h=3, p=1, q=1)

arma11_accuracy_1_mod <- calculate_forecast_accuracy(arma11_forecasts_1_mod, log_returns_test)
arma11_accuracy_3_mod <- calculate_forecast_accuracy(arma11_forecasts_3_mod, log_returns_test[3:length(log_returns_test)])

arma11_errors_1_mod <- forecast_errors(arma11_forecasts_1_mod, log_returns_test)
arma11_errors_3_mod <- forecast_errors(arma11_forecasts_3_mod, log_returns_test[3:length(log_returns_test)])

# Save data so that we don't have to re-run lengthy processes

forecasts <- data.frame(
  Date = test_data$date,
  Actual = log_returns_test,
  ARMA11_1 = arma11_forecasts_1,
  ARMA58_1 = arma58_forecasts_1,
  ARMA11_1_mod = arma11_forecasts_1_mod,
  ARMA11_3 = c(NA, NA, arma11_forecasts_3),
  ARMA58_3 = c(NA, NA, arma58_forecasts_3),
  ARMA11_3_mod = c(NA, NA, arma11_forecasts_3_mod)
)
write.csv(forecasts, "phase2_forecasts.csv", row.names = FALSE)


# Combine results into a dataframe
mae_rmse <- data.frame(
  Model = c("ARMA(1,1)", "ARMA(5,8)", "ARMA(1,1)*"),
  MAE_1 = c(arma11_accuracy_1$MAE, arma58_accuracy_1$MAE, arma11_accuracy_1_mod$MAE),
  RMSE_1 = c(arma11_accuracy_1$RMSE, arma58_accuracy_1$RMSE, arma11_accuracy_1_mod$RMSE),
  MAE_3 = c(arma11_accuracy_3$MAE, arma58_accuracy_3$MAE, arma11_accuracy_3_mod$MAE),
  RMSE_3 = c(arma11_accuracy_3$RMSE, arma58_accuracy_3$RMSE, arma11_accuracy_3_mod$RMSE)
)

# Print results
print(mae_rmse)
# save results
write.csv(mae_rmse, "phase2_model_accuracy.csv", row.names = FALSE)

forerr <- data.frame(
  Date = test_data$date,
  Actual = log_returns_test,
  ARMA11_1 = arma11_errors_1,
  ARMA58_1 = arma58_errors_1,
  ARMA11_1_mod = arma11_errors_1_mod,
  ARMA11_3 = c(NA, NA, arma11_errors_3),
  ARMA58_3 = c(NA, NA, arma58_errors_3),
  ARMA11_3_mod = c(NA, NA, arma11_errors_3_mod)
)
# save data
write.csv(forerr, "phase2_forerr.csv", row.names = FALSE)

# Plotting
data_forecasts <- read.csv("phase2_forecasts.csv") 

# Create data frame for plotting
plot_data <- data.frame(
  Date = date(data_forecasts$Date),
  Actual = data_forecasts$Actual,
  ARMA11_1 = data_forecasts$ARMA58_1,
  ARMA58_1 = data_forecasts$ARMA58_1,
  ARMA11_1_mod = data_forecasts$ARMA11_1_mod,
  ARMA11_3 = data_forecasts$ARMA11_3,
  ARMA58_3 = data_forecasts$ARMA58_3,
  ARMA11_3_mod =data_forecasts$ARMA11_3_mod
)

# Plot the one-period-ahead forecasts
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual", group = 1)) +
  geom_line(aes(y = ARMA11_1, color = "ARMA(1,1) Forecast (1-period)", group = 1)) +
  geom_line(aes(y = ARMA58_1, color = "ARMA(5,8) Forecast (1-period)", group = 1)) +
  geom_line(aes(y = ARMA11_1_mod, color = "ARMA(1,1)* Forecast (1-period)", group = 1)) +
  labs(title = "One-Period-Ahead Forecasts", y = "Log Returns", color = "Legend") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()

# Plot the three-periods-ahead forecasts
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual", group = 1)) +
  geom_line(aes(y = ARMA11_3, color = "ARMA(1,1) Forecast (3-periods)", group = 1)) +
  geom_line(aes(y = ARMA58_3, color = "ARMA(5,8) Forecast (3-periods)", group = 1)) +
  geom_line(aes(y = ARMA11_3_mod, color = "ARMA(1,1)* Forecast (3-periods)", group = 1)) +
  labs(title = "Three-Periods-Ahead Forecasts", y = "Log Returns", color = "Legend") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()

# Diebold-Mariano Test

# For 1 period ahead
dm.test(arma11_errors_1, arma58_errors_1, h=1)
# p-value = 0.02668 do not have equal predictive power
dm.test(arma11_errors_1, arma58_errors_1, h=1, alternative = "greater")
# p-value = 0.9867, accept the null that
# expected loss from the second forecast (arma11) is greater

dm.test(arma11_errors_1, arma11_errors_1_mod,h=1)
# p-value = 0.6654 - equal predictive power

# For 3 periods ahead
dm.test(arma11_errors_3,arma58_errors_3,h=3)
# p-value = 0.02693 - do not have equal predictive power
dm.test(arma11_errors_3,arma58_errors_3,h=3, alternative="greater")
# p-value = 0.9865, accept the null that
# expected loss from the arma58 forecast is greater

dm.test(arma11_errors_3, arma11_errors_3_mod, h=3)
# p-value = 0.3707 equal predictive power

# Phase 3: Building multivariate model for forecast ####

# 3.1 Load the necessary libraries ####
library(tidyverse)
library(caret)
library(car)
library(lmtest)
library(urca)

# 3.2 Train / test split ####
data <- read_csv("sfd_final_project_data.csv")
split_ratio <- 0.8
split_index <- floor(split_ratio * nrow(data))
train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

# 3.3 Checking stationarity of the explanatory variables ####

# Stationarity of regressors

tm<-as.Date(train_data$date,format = "%m/%d/%Y")

#SP500
plot(tm,train_data$SP500,type='l',xlab='year',ylab='SP500')
# does not look stationary; looks like random walk with drift
# test
adf_SP500<-ur.df(train_data$SP500,type="drift",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_SP500) 
# null accepted -- nonstationary (accept null of nonstationarity if t stat is greater than critical value)
kpss_SP500<-ur.kpss(train_data$SP500, type = "mu", lags = "short",use.lag = NULL)
summary(kpss_SP500)
# null rejected --  nonstationary (reject null of stationarity is t stat is greater than critical value)
# non-stationary

#NASDAQ
plot(tm,train_data$NASDAQ,type='l',xlab='year',ylab='NASDAQ')
# test
adf_NASDAQ<-ur.df(train_data$NASDAQ,type="drift",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_NASDAQ) 
# null accepted -- nonstationary (accept null of nonstationarity if t stat is greater than critical value)
kpss_NASDAQ<-ur.kpss(train_data$NASDAQ, type = "mu", lags = "short",use.lag = NULL)
summary(kpss_NASDAQ)
# null rejected --  nonstationary (reject null of stationarity is t stat is greater than critical value)
# non-stationary

#VIX
plot(tm,train_data$VIX,type='l',xlab='year',ylab='VIX')
# test
adf_VIX<-ur.df(train_data$VIX,type="drift",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_VIX) 
# null rejected -> stationary
kpss_VIX<-ur.kpss(train_data$VIX, type = "mu", lags = "short",use.lag = NULL)
summary(kpss_VIX)
# null rejected --  nonstationary (reject null of stationarity is t stat is greater than critical value)

#MKTRF
plot(tm,train_data$MKTRF,type='l',xlab='year',ylab='MKTRF')
# test
adf_MKTRF<-ur.df(train_data$MKTRF,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_MKTRF) 
# null rejected -> stationary

#SMB
plot(tm,train_data$SMB,type='l',xlab='year',ylab='SMB')
# test
adf_SMB<-ur.df(train_data$SMB,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_SMB) 
# null rejected -> stationary

#HML
plot(tm,train_data$HML,type='l',xlab='year',ylab='HML')
# test
adf_HML<-ur.df(train_data$HML,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_HML) 
# null rejected -> stationary

#RMW
plot(tm,train_data$RMW,type='l',xlab='year',ylab='RMW')
# test
adf_RMW<-ur.df(train_data$RMW,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_RMW) 
# null rejected -> stationary

#CMA
plot(tm,train_data$CMA,type='l',xlab='year',ylab='CMA')
# test
adf_CMA<-ur.df(train_data$CMA,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_CMA) 
# null rejected -> stationary

#MOM
plot(tm,train_data$MOM,type='l',xlab='year',ylab='MOM')
# test
adf_MOM<-ur.df(train_data$MOM,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_MOM) 
# null rejected -> stationary

# Calculate log returns for non-stationary variables on the train set
train_data <- train_data %>%
  mutate(
    SP500_log_return = log(SP500) - lag(log(SP500)),
    NASDAQ_log_return = log(NASDAQ) - lag(log(NASDAQ)),
    VIX_log_return = log(VIX) - lag(log(VIX))
  ) %>%
  na.omit()  # Drop NA values

# Calculate log returns for non-stationary variables on the test set
test_data <- test_data %>%
  mutate(
    SP500_log_return = log(SP500) - lag(log(SP500)),
    NASDAQ_log_return = log(NASDAQ) - lag(log(NASDAQ)),
    VIX_log_return = log(VIX) - lag(log(VIX))
  ) %>%
  na.omit()  # Drop NA values

# Stationarity test for transformed data

#SP500
plot(tm[1:length(tm)-1],train_data$SP500_log_return,type='l',xlab='year',ylab='SP500')
# looks better than before
# test
adf_SP500<-ur.df(train_data$SP500_log_return,type="none",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_SP500) 
# null rejected -> stationary

#NASDAQ
plot(tm[1:length(tm)-1],train_data$NASDAQ_log_return,type='l',xlab='year',ylab='NASDAQ')
# looks better than before
# test
adf_NASDAQ<-ur.df(train_data$NASDAQ_log_return,type="drift",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_NASDAQ) 
# null rejected -> stationary

#VIX
plot(tm[1:length(tm)-1],train_data$VIX_log_return,type='l',xlab='year',ylab='VIX')
# looks better than before
# test
adf_VIX<-ur.df(train_data$VIX_log_return,type="drift",lags=12,selectlags = "AIC") # there cannot be trend stationarity in EMBI, hence we select drift alternative
summary(adf_VIX) 
# null rejected -> stationary

# 3.4 Model fitting ####

# Selecting regressors - general to specific

# Fit the model using log returns for the non-stationary variables
mv_model <- lm(QQQ_log_return ~ SP500_log_return + NASDAQ_log_return + VIX_log_return + MKTRF + SMB + HML + RMW + CMA + MOM, data=train_data)
summary(mv_model)
# insignificant: HML, CMA, MOM
#Residual standard error: 0.0009639 on 593 degrees of freedom
#Multiple R-squared:  0.9962,	Adjusted R-squared:  0.9962 
#F-statistic: 2.596e+04 on 6 and 593 DF,  p-value: < 2.2e-16

# Checking multicollinearity with VIF
print(vif(mv_model))
# high vif in MKTRF, NASDAQ_log_return, SP500_log_return

# Refitting the model without the MKTRF due to VIF
mv_model1 <- lm(QQQ_log_return ~ SP500_log_return + NASDAQ_log_return + VIX_log_return + SMB + HML + RMW + CMA + MOM, data=train_data)
summary(mv_model1)
print(vif(mv_model1))

# Refitting the model without the MKTRF and NASDAQ_log_return due to VIF
mv_model2 <- lm(QQQ_log_return ~ SP500_log_return + VIX_log_return + SMB + HML + RMW + CMA + MOM, data=train_data)
summary(mv_model2)
# insignificant: intercept, VIX_log_return, RMW, MOM
print(vif(mv_model2))

# Refitting the model without the Intercept, VIX_log_return, RMW and MOM due to significance
mv_model3 <- lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA - 1, data=train_data)
summary(mv_model3)
print(vif(mv_model3))
# all parameters are significant and vif values are below 5

# Adding removed factors separately to see if they become significant
summary(lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA, data=train_data)) # intercept
summary(lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA - 1 + VIX_log_return, data=train_data)) # VIX_log_return
summary(lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA - 1 + RMW, data=train_data)) # RMW
summary(lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA - 1 + MOM, data=train_data)) # MOM

# Stepwise regression
full_model <- lm(QQQ_log_return ~ SP500_log_return + SMB + HML + CMA - 1, data = train_data)

# Function to fit the model and calculate F-statistic
fit_and_evaluate <- function(formula, data) {
  model <- lm(formula, data)
  summary(model)
}

# Exclude each term and evaluate
models <- list(
  "Without SP500_log_return" = QQQ_log_return ~ SMB + HML + CMA - 1,
  "Without SMB" = QQQ_log_return ~ SP500_log_return + HML + CMA - 1,
  "Without HML" = QQQ_log_return ~ SP500_log_return + SMB + CMA - 1,
  "Without CMA" = QQQ_log_return ~ SP500_log_return + SMB + HML - 1
)

# Fit and evaluate each model
results <- lapply(models, fit_and_evaluate, data = train_data)

# Print summaries for comparison
for (name in names(results)) {
  cat("\n\nModel:", name, "\n")
  print(results[[name]])
}

# Wald test
beta <- coef(mv_model3)
VCV <- vcov(mv_model3)
# Define the matrix R which imposes restrictions 
# We have 4 coefficients and no intercept, R should select the coefficients (not the intercept)
R <- diag(4)
# Compute R times beta
Rb <- R %*% beta
# Compute the Wald test statistic
Wald <- t(Rb) %*% solve(R %*% VCV %*% t(R)) %*% Rb
# Degrees of freedom for the test is the number of restrictions
df <- nrow(R)
# Calculate the p-value using the chi-squared distribution
pval_Wald <- 1 - pchisq(drop(Wald), df)
# Output the Wald statistic and p-value
cat("Wald statistic:", drop(Wald), "\n")
cat("p-value for the Wald statistic:", pval_Wald, "\n")

# p-value is 0, Wald stat is 15114.52
#we can reject the hypothesis that all coefficients are jointly 0

# Check Residuals for Normality using Q-Q plot
qqnorm(resid(mv_model3))
qqline(resid(mv_model3))

# Homoscedasticity Check
plot(fitted(mv_model3), resid(mv_model3), 
     main="Residuals vs Fitted",
     xlab="Fitted values",
     ylab="Residuals")
abline(h=0, col="red")

# Breusch-Pagan test for homoscedasticity
bptest(mv_model3)
# p-value is 0.7362 -> accept the null of homoscedasticity

# White's test
white_test <- bptest(mv_model3, ~ fitted.values(mv_model3) + I(fitted.values(mv_model3)^2), data = train_data)
print(white_test)
# p-value = 0.7937 -> accept the null of homoscedasticity

# Serial correlation

plot(mv_model3$residuals, type='l')
# appears random enough
acf(mv_model3$residuals) 
# no visual signs of autocoreelation

# Breusch-Godfrey (LM) test for lags up to 4
bgtest(mv_model3,order=4,type="Chisq")
# p-value = 0.6511 -> accept the null of no serial correlation up to lag 4

# Ljung-Box Portamanteu test for lags up to 16
Box.test(mv_model3$residuals, lag = 16, type = "Ljung-Box", fitdf = 4)
# p-value = 0.5548 -> accpet the null of no serial correlation up to lag 16

# 3.5 Model Evaluation ####

# FF 5 factor model
ff_model<- lm(QQQ_log_return ~ MKTRF + SMB + HML + RMW + CMA, data=train_data)
summary(ff_model)

# Benchmark model 1: FF 5 factor without intercept
benchmark_model1<- lm(QQQ_log_return ~ MKTRF + SMB + HML + RMW + CMA - 1, data=train_data)
summary(benchmark_model1)

# add january dummy train
train_data <- train_data %>%
  mutate(
    jan_dummy = case_when(substr(date, 6, 7) ==  "01" ~ 1, .default = 0)
  )

# add january dummy test
test_data <- test_data %>%
  mutate(
    jan_dummy = case_when(substr(date, 6, 7) ==  "01" ~ 1, .default = 0)
  )

# Benchmark model 2: FF 5 factor + jan_dummy
benchmark_model2<- lm(QQQ_log_return ~ MKTRF + SMB + HML  + CMA + jan_dummy -1, data=train_data)
summary(benchmark_model2)


# Function to perform rolling forecasts using arima models
rolling_forecast_lm <- function(model, train_data, test_data, h) {
  n <- nrow(test_data)
  forecasts <- numeric(n-h+1)
  for (j in 1:(n-h+1)) {
    print(j)
    k <- nrow(train_data)
    model<-lm(formula(model), data=train_data[j: k,])
    forctemp<-predict(model, newdata = test_data[j:(j+h-1), ])
    forecasts[j]<-mean(forctemp)
    train_data <- rbind(train_data, test_data[j, ])
  }
  return(forecasts)
}

# MV_model3
mv_model3_forecast_1 <- rolling_forecast_lm(mv_model3, train_data, test_data, h = 1)
mv_model3_forecast_3 <- rolling_forecast_lm(mv_model3, train_data, test_data, h = 3)

mv_model3_accuracy_1 <- calculate_forecast_accuracy(mv_model3_forecast_1, test_data$QQQ_log_return[1:length(mv_model3_forecast_1)])
mv_model3_accuracy_3 <- calculate_forecast_accuracy(mv_model3_forecast_3, test_data$QQQ_log_return[3:length(mv_model3_forecast_1)])

mv_model3_errors_1 <- forecast_errors(mv_model3_forecast_1, test_data$QQQ_log_return[1:length(mv_model3_forecast_1)])
mv_model3_errors_3 <- forecast_errors(mv_model3_forecast_3, test_data$QQQ_log_return[3:length(mv_model3_forecast_1)])

# benchmark 1
b1_forecast_1 <- rolling_forecast_lm(benchmark_model1, train_data, test_data, h = 1)
b1_forecast_3 <- rolling_forecast_lm(benchmark_model1, train_data, test_data, h = 3)

b1_accuracy_1 <- calculate_forecast_accuracy(b1_forecast_1, test_data$QQQ_log_return[1:length(b1_forecast_1)])
b1_accuracy_3 <- calculate_forecast_accuracy(b1_forecast_3, test_data$QQQ_log_return[3:length(b1_forecast_1)])

b1_errors_1 <- forecast_errors(b1_forecast_1, test_data$QQQ_log_return[1:length(b1_forecast_1)])
b1_errors_3 <- forecast_errors(b1_forecast_3, test_data$QQQ_log_return[3:length(b1_forecast_1)])

# benchmark 2

b2_forecast_1 <- rolling_forecast_lm(benchmark_model2, train_data, test_data, h = 1)
b2_forecast_3 <- rolling_forecast_lm(benchmark_model2, train_data, test_data, h = 3)

b2_accuracy_1 <- calculate_forecast_accuracy(b2_forecast_1, test_data$QQQ_log_return[1:length(b2_forecast_1)])
b2_accuracy_3 <- calculate_forecast_accuracy(b2_forecast_3, test_data$QQQ_log_return[3:length(b2_forecast_1)])

b2_errors_1 <- forecast_errors(b2_forecast_1, test_data$QQQ_log_return[1:length(b2_forecast_1)])
b2_errors_3 <- forecast_errors(b2_forecast_3, test_data$QQQ_log_return[3:length(b2_forecast_1)])


# Print the accuracy metrics
print("One-Period-Ahead Forecast Accuracy:")
f1 = data.frame(Model = c("mv_model3", "benchmark_model1", "benchmark_model2"),
                MAE = c(mv_model3_accuracy_1$MAE, b1_accuracy_1$MAE, b2_accuracy_1$MAE),
                RMSE = c(mv_model3_accuracy_1$RMSE, b1_accuracy_1$RMSE, b2_accuracy_1$RMSE))
print(f1)
# save data
write.csv(f1, "phase3_accuracy1.csv", row.names = FALSE)

print("Three-Periods-Ahead Forecast Accuracy:")

f2 = data.frame(Model = c("mv_model3", "benchmark_model1", "benchmark_model2"),
                MAE = c(mv_model3_accuracy_3$MAE, b1_accuracy_3$MAE, b2_accuracy_3$MAE),
                RMSE = c(mv_model3_accuracy_3$RMSE, b1_accuracy_3$RMSE, b2_accuracy_3$RMSE))
print(f2)
write.csv(f2, "phase3_accuracy2.csv", row.names = FALSE)

# Make a plot df containing ARMA(1,1) and MV3 model forecasts
plot_forecasts_df = plot_data[, c('Date', 'Actual', 'ARMA11_1_mod', 'ARMA11_3_mod')]
plot_forecasts_df$MV_1 = mv_model3_forecast_1
plot_forecasts_df$MV_3 = c(NA, NA, mv_model3_forecast_3)

plot_forecasts_df$Date = date(plot_forecasts_df$Date)

# Plot the one-period-ahead forecasts
ggplot(plot_forecasts_df, aes(x = date(Date)), group = 1) +
  geom_line(aes(y = Actual, color = "Actual"), group = 1) +
  geom_line(aes(y = ARMA11_1_mod, color = "ARMA(1,1)* Forecast (1-period)"), group = 1) +
  geom_line(aes(y = MV_1, color = "MV Forecast (1-period)"), group = 1) +
  labs(title = "One-Period-Ahead Forecasts", y = "Log Returns", color = "Legend") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()

# Plot the three-periods-ahead forecasts
ggplot(plot_forecasts_df, aes(x = date(Date)), group = 1) +
  geom_line(aes(y = Actual, color = "Actual"), group = 1) +
  geom_line(aes(y = ARMA11_3_mod, color = "ARMA(1,1)* Forecast (3-period)"), group = 1) +
  geom_line(aes(y = MV_3, color = "MV Forecast (3-period)"), group = 1) +
  labs(title = "Three-Period-Ahead Forecasts", y = "Log Returns", color = "Legend") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()

# Diebold-Mariano Test

# For 1 period ahead
dm.test(mv_model3_errors_1,b1_errors_1,h=1)
# equal predictive power
dm.test(mv_model3_errors_1,b2_errors_1,h=1)
# equal predictive power

# For 3 periods ahead
dm.test(mv_model3_errors_3,b1_errors_3,h=3)
# equal predictive power
dm.test(mv_model3_errors_3,b2_errors_3,h=3)
# equal predictive power

dm.test(mv_model3_errors_1,arma11_errors_1_mod,h=1, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# mv_model3 has greater predictive power

dm.test(mv_model3_errors_3,arma11_errors_3_mod,h=3, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# mv_model3 has greater predictive power


dm.test(b1_errors_1,arma11_errors_1_mod,h=1, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# b1_model3 has greater predictive power

dm.test(b1_errors_3,arma11_errors_3_mod,h=3, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# b1_model3 has greater predictive power

dm.test(b2_errors_1,arma11_errors_1_mod,h=1, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# b2_model3 has greater predictive power

dm.test(b2_errors_3,arma11_errors_3_mod,h=3, alternative="greater")
# p-value = 1, expected loss from the second forecast (arma11) is greater
# b2_model3 has greater predictive power

# Phase 4: Building volatility model ####

# 4.1 Load the necessary libraries ####
library(quantmod)
library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(FinTS)
library(fGarch)

# 4.2 Data and train / test split ####
data <- read_csv("sfd_final_project_data.csv")
split_ratio <- 0.8
split_index <- floor(split_ratio * nrow(data))
train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

# Calculate log returns for non-stationary variables on the train set
train_data <- train_data %>%
  mutate(
    SP500_log_return = log(SP500) - lag(log(SP500)),
    NASDAQ_log_return = log(NASDAQ) - lag(log(NASDAQ)),
    VIX_log_return = log(VIX) - lag(log(VIX))
  ) %>%
  na.omit()  # Drop NA values

# Calculate log returns for non-stationary variables on the test set
test_data <- test_data %>%
  mutate(
    SP500_log_return = log(SP500) - lag(log(SP500)),
    NASDAQ_log_return = log(NASDAQ) - lag(log(NASDAQ)),
    VIX_log_return = log(VIX) - lag(log(VIX))
  ) %>%
  na.omit()  # Drop NA values
split_ratio <- 0.8
split_index <- floor(split_ratio * nrow(data))
train_data <- data[1:split_index, ]
test_data <- data[(split_index + 1):nrow(data), ]

# 4.3 Autocorrelation testing ####

# Fit ARMA(1,1) model without intercept
log_returns_train <- train_data$QQQ_log_return
arma11_model <- arima(log_returns_train, order = c(1, 0, 1), include.mean = FALSE)

# Residual Analysis
residuals_arma11 <- residuals(arma11_model)
acf(residuals_arma11, main = "ACF of ARMA(1,1) residuals")
pacf(residuals_arma11, main = "PACF of ARMA(1,1) residuals")
Box.test(residuals_arma11, lag = 25, type = "Ljung-Box", fitdf = 2)
# p-value = 0.1591 -- accept the null of no serial correlation

# 4.4 ARCH effect testing ####

# Check for ARCH effects
acf(residuals_arma11^2, main="ACF of ARMA(1,1) sqared residuals")
pacf(residuals_arma11^2, main="PACF of ARMA(1,1) sqared residuals")
Box.test(residuals_arma11^2, lag = 25, type = "Ljung-Box")
ArchTest(residuals_arma11)


# 4.5 Volatility model selection ####

# Initialize a data frame to store the information criteria
ic_vol_norm <- data.frame(matrix(nrow=0, ncol=4))
colnames(ic_vol_norm) <- c('AIC', 'BIC', 'SIC', 'HQIC')

# Fit GARCH models with normal distribution
for (p in 1:5) {
  for (q in 0:5) {
    model <- garchFit(substitute(~garch(p, q), list(p=p, q=q)),
                      data=train_data$QQQ_log_return,
                      trace=FALSE)
    model_name <- paste0("GARCH(", p, ',', q, ')-norm')
    ic_vol_norm[model_name,] <- model@fit$ics
  }
}

# Fit ARMA(1,1) + GARCH models with normal distribution
for (p in 1:5) {
  for (q in 0:5) {
    model <- garchFit(substitute(~arma(1,1)+garch(p, q), list(p=p, q=q)),
                      data=train_data$QQQ_log_return,
                      include.mean=FALSE,  # No intercept
                      trace=FALSE)
    model_name <- paste0("ARMA(1,1)+GARCH(", p, ',', q, ')-norm')
    ic_vol_norm[model_name,] <- model@fit$ics
  }
}

# Display the top 10 models by AIC and BIC
round(ic_vol_norm[order(ic_vol_norm$AIC),][1:10,], 4)
round(ic_vol_norm[order(ic_vol_norm$BIC),][1:10,], 4)

# Fit selected models with normal distribution
garch11_norm <- garchFit(~garch(1, 1), data=train_data$QQQ_log_return, trace=FALSE)
garch21_norm <- garchFit(~garch(2, 1), data=train_data$QQQ_log_return, trace=FALSE)
garch31_norm <- garchFit(~garch(3, 1), data=train_data$QQQ_log_return, trace=FALSE)

# Fit models with Student's t-distribution
garch11_std <- garchFit(~garch(1, 1), data=train_data$QQQ_log_return, cond.dist="std", trace=FALSE)
garch21_std <- garchFit(~garch(2, 1), data=train_data$QQQ_log_return, cond.dist="std", trace=FALSE)
garch31_std <- garchFit(~garch(3, 1), data=train_data$QQQ_log_return, cond.dist="std", trace=FALSE)

# Fit models with Generalized Error Distribution (GED)
garch11_gde <- garchFit(~garch(1, 1), data=train_data$QQQ_log_return, cond.dist="ged", trace=FALSE)
garch21_gde <- garchFit(~garch(2, 1), data=train_data$QQQ_log_return, cond.dist="ged", trace=FALSE)
garch31_gde <- garchFit(~garch(3, 1), data=train_data$QQQ_log_return, cond.dist="ged", trace=FALSE)

# Summary of the selected models
summary(garch11_norm)
summary(garch21_norm)
summary(garch31_norm)
summary(garch11_std)
summary(garch21_std)
summary(garch31_std)
summary(garch11_gde)
summary(garch21_gde)
summary(garch31_gde)

# Create a function to extract model information
extract_model_info <- function(model, model_name) {
  return(data.frame(
    Model = model_name,
    AIC = model@fit$ics["AIC"],
    BIC = model@fit$ics["BIC"],
    LogLikelihood = model@fit$llh
  ))
}

# Extract information from all models
model_info <- rbind(
  extract_model_info(garch11_norm, "GARCH(1,1)-norm"),
  extract_model_info(garch21_norm, "GARCH(2,1)-norm"),
  extract_model_info(garch31_norm, "GARCH(3,1)-norm"),
  extract_model_info(garch11_std, "GARCH(1,1)-std"),
  extract_model_info(garch21_std, "GARCH(2,1)-std"),
  extract_model_info(garch31_std, "GARCH(3,1)-std"),
  extract_model_info(garch11_gde, "GARCH(1,1)-ged"),
  extract_model_info(garch21_gde, "GARCH(2,1)-ged"),
  extract_model_info(garch31_gde, "GARCH(3,1)-ged")
)

# Rank models by AIC, BIC, and Log Likelihood
ranked_models <- model_info %>%
  arrange(AIC) %>%
  mutate(AIC_Rank = row_number()) %>%
  arrange(BIC) %>%
  mutate(BIC_Rank = row_number()) %>%
  arrange(desc(LogLikelihood)) %>%
  mutate(LogLikelihood_Rank = row_number())

# Display the ranked table
rm = ranked_models %>%
  select(Model, AIC, AIC_Rank, BIC, BIC_Rank, LogLikelihood, LogLikelihood_Rank) %>%
  arrange(AIC_Rank)

print(rm)
write.csv(rm, "garchrankedtable.csv", row.names = FALSE)

# 4.6 Volatility model forecast ####

# Function to calculate accuracy metrics
calculate_accuracy_metrics <- function(actual, forecast) {
  mae <- mean(abs(actual - forecast))
  rmse <- sqrt(mean((actual - forecast)^2))
  return(data.frame(MAE = mae, RMSE = rmse))
}

# Function to generate rolling forecasts with volatility and confidence intervals
rolling_forecast_garch <- function(train_data, test_data, garch_formula, cond_dist = "norm", sig_level = 0.05) {
  n <- length(test_data)
  forecasts <- numeric(n)
  volatilities <- numeric(n)
  lows <- numeric(n)
  highs <- numeric(n)
  
  for (i in 1:n) {
    train_subset <- c(train_data, test_data[1:(i-1)])
    model <- garchFit(garch_formula, data = train_subset, cond.dist = cond_dist, trace = FALSE)
    
    # Empirical quantiles
    quantiles <- quantile(residuals(model, standardize = TRUE), c(sig_level / 2, 1 - (sig_level / 2)))
    
    prediction <- predict(model, n.ahead = 1)
    
    vol <- prediction$standardDeviation
    mean <- prediction$meanForecast
    forecasts[i] <- mean
    volatilities[i] <- vol
    lows[i] <- mean + quantiles[1] * vol
    highs[i] <- mean + quantiles[2] * vol
  }
  
  data.frame(
    Time = 1:n,
    Actual = test_data,
    Forecast = forecasts,
    Volatility = volatilities,
    Low = lows,
    High = highs
  )
}

# Generate rolling forecasts for each GARCH(1,1) model
forecast_garch11_norm <- rolling_forecast_garch(train_data$QQQ_log_return, test_data$QQQ_log_return, ~garch(1, 1), cond_dist = "norm")
forecast_garch11_std <- rolling_forecast_garch(train_data$QQQ_log_return, test_data$QQQ_log_return, ~garch(1, 1), cond_dist = "std")
forecast_garch11_ged <- rolling_forecast_garch(train_data$QQQ_log_return, test_data$QQQ_log_return, ~garch(1, 1), cond_dist = "ged")

# Calculate accuracy metrics for each model
accuracy_garch11_norm <- calculate_accuracy_metrics(forecast_garch11_norm$Actual, forecast_garch11_norm$Forecast)
accuracy_garch11_std <- calculate_accuracy_metrics(forecast_garch11_std$Actual, forecast_garch11_std$Forecast)
accuracy_garch11_ged <- calculate_accuracy_metrics(forecast_garch11_ged$Actual, forecast_garch11_ged$Forecast)

# Combine the accuracy metrics into one data frame
accuracy_metrics <- data.frame(
  Model = c("GARCH(1,1)-norm", "GARCH(1,1)-std", "GARCH(1,1)-ged"),
  MAE = c(accuracy_garch11_norm$MAE, accuracy_garch11_std$MAE, accuracy_garch11_ged$MAE),
  RMSE = c(accuracy_garch11_norm$RMSE, accuracy_garch11_std$RMSE, accuracy_garch11_ged$RMSE)
)

# Sort the accuracy metrics by RMSE and then by MAE
accuracy_metrics <- accuracy_metrics %>%
  arrange(RMSE, MAE)

# Display the sorted accuracy metrics
print(accuracy_metrics)

write.csv(accuracy_metrics, "garchaccuracy.csv", row.names = FALSE)

# Diebold-Mariano Test

garch11_norm_errors <- forecast_errors(forecast_garch11_norm$Forecast, forecast_garch11_norm$Actual)
garch11_std_errors <- forecast_errors(forecast_garch11_std$Forecast, forecast_garch11_std$Actual)
garch11_ged_errors <- forecast_errors(forecast_garch11_ged$Forecast, forecast_garch11_ged$Actual)

dm.test(garch11_norm_errors, garch11_std_errors, h=1)
# p-value = 0.7244 - equal predictive power
dm.test(garch11_norm_errors, garch11_ged_errors, h=1)
# p-value = 0.7095 - equal predictive power
dm.test(garch11_std_errors, garch11_ged_errors, h=1)
# p-value = 0.7352 - equal predictive power


# Function to plot the forecast intervals
plot_forecast_intervals <- function(forecast_df, model_name) {
  ggplot(forecast_df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Forecast, color = "Forecast")) +
    geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
    labs(title = paste("Forecast Intervals for", model_name),
         y = "Log Return") +
    scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
    theme_minimal()
}

# Function to evaluate hits
evaluate_hits <- function(forecast_df, sig_level = 0.05) {
  hits <- with(forecast_df, ifelse(Actual >= Low & Actual <= High, 1, 0))
  hit_rate <- mean(hits)
  return(data.frame(HitRate = hit_rate, Threshold = 1 - sig_level))
}

# Generate plots for each model
plot_garch11_norm <- plot_forecast_intervals(forecast_garch11_norm, "GARCH(1,1)-norm")
plot_garch11_std <- plot_forecast_intervals(forecast_garch11_std, "GARCH(1,1)-std")
plot_garch11_ged <- plot_forecast_intervals(forecast_garch11_ged, "GARCH(1,1)-ged")

# Display the plots
print(plot_garch11_norm)
print(plot_garch11_std)
print(plot_garch11_ged)

# Evaluate hits for each model
hits_garch11_norm <- evaluate_hits(forecast_garch11_norm)
hits_garch11_std <- evaluate_hits(forecast_garch11_std)
hits_garch11_ged <- evaluate_hits(forecast_garch11_ged)

# Combine the hit rates into one data frame
hit_rates <- data.frame(
  Model = c("GARCH(1,1)-norm", "GARCH(1,1)-std", "GARCH(1,1)-ged"),
  HitRate = c(hits_garch11_norm$HitRate, hits_garch11_std$HitRate, hits_garch11_ged$HitRate),
  Threshold = c(hits_garch11_norm$Threshold, hits_garch11_std$Threshold, hits_garch11_ged$Threshold)
)

# Display the hit rates
print(hit_rates)
