library(dplyr)
library(tidyr)
library("corrplot")
library(readxl)
library(ggplot2)
library(mgcv)


# Load the data
data <- read_excel("data weekly.xlsx")
# Preview the data
head(data)
# View column names
colnames(data)



# Rename the columns in the data frame
data <- data %>%
  rename(
    WNV_2022_CumCounts = `2022 Arboviral diseases, West Nile virus disease; Cumulative year-to-date counts.`,
    WNV_2021_CumCounts = `2021 Arboviral diseases, West Nile virus disease;  †Cumulative year-to-date counts.`,
    WNV_2020_CumCounts = `2020 Arboviral diseases, West Nile virus disease; †Cumulative year-to-date counts.`,
    WNV_2019_CumCounts = `2019 Arboviral diseases, West Nile virus disease; Cumulative year-to-date counts.`,
    WNV_2018_CumCounts = `2018 Arboviral diseases, West Nile virus disease; Cumulative year-to-date counts.`,
    WNV_2017_CumCounts = `2017 Arboviral diseases, West Nile virus disease`
  )

# Check the renamed column names

colnames(data)
# Load necessary packages
library(ggplot2)
library(forecast)

# Assuming your data is in a data frame called `data`
# View the structure of the dataset
str(data)
data$Week <- as.numeric(data$Week)
# Extract relevant columns
data_subset <- data %>%
  select(Week, WNV_2022_CumCounts) %>%
  mutate(Week )   

data_subset2021 <- data %>%
  select(Week, WNV_2021_CumCounts) %>%
  mutate(Week )   

data_subset2020 <- data %>%
  select(Week, WNV_2020_CumCounts) %>%
  mutate(Week )  

data_subset2019 <- data %>%
  select(Week, WNV_2019_CumCounts) %>%
  mutate(Week )  

data_subset2018 <- data %>%
  select(Week, WNV_2018_CumCounts) %>%
  mutate(Week )  

data_subset2017 <- data %>%
  select(Week, WNV_2017_CumCounts) %>%
  mutate(Week ) 
# Function to calculate Poisson EWMA
poisson_ewma <- function(counts, lambda) {
  n <- length(counts)
  ewma <- numeric(n)
  ewma[1] <- counts[1]  # Initialize with the first count
  for (t in 2:n) {
    ewma[t] <- lambda * counts[t] + (1 - lambda) * ewma[t - 1]
  }
  return(ewma)
}

# Set lambda and calculate EWMA
lambda <- 0.2
data_subset$ewma <- poisson_ewma(data_subset$WNV_2022_CumCounts, lambda)
data_subset2021$ewma <- poisson_ewma(data_subset2021$WNV_2021_CumCounts, lambda)
data_subset2020$ewma <- poisson_ewma(data_subset2020$WNV_2020_CumCounts, lambda)
data_subset2019$ewma <- poisson_ewma(data_subset2019$WNV_2019_CumCounts, lambda)
data_subset2018$ewma <- poisson_ewma(data_subset2018$WNV_2018_CumCounts, lambda)
data_subset2017$ewma <- poisson_ewma(data_subset2017$WNV_2017_CumCounts, lambda)


# Calculate residuals
data_subset$residuals <- data_subset$WNV_2022_CumCounts - data_subset$ewma
data_subset2021$residuals <- data_subset2021$WNV_2021_CumCounts - data_subset2021$ewma
data_subset2020$residuals <- data_subset2020$WNV_2020_CumCounts - data_subset2020$ewma
data_subset2019$residuals <- data_subset2019$WNV_2019_CumCounts - data_subset2019$ewma
data_subset2018$residuals <- data_subset2018$WNV_2018_CumCounts - data_subset2018$ewma
data_subset2017$residuals <- data_subset2017$WNV_2017_CumCounts - data_subset2017$ewma


# Plot residuals
png('Residuals of Poisson EWMA for Weekly WNV 2022.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2022",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()
 
png('Residuals of Poisson EWMA for Weekly WNV 2021.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset2021, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2021",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()

png('Residuals of Poisson EWMA for Weekly WNV 2020.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset2020, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2020",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()

png('Residuals of Poisson EWMA for Weekly WNV 2019.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset2019, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2019",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()

png('Residuals of Poisson EWMA for Weekly WNV 2018.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset2018, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2018",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()

png('Residuals of Poisson EWMA for Weekly WNV 2017.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_subset2017, aes(x = Week, y = residuals)) +
  geom_line(color = "darkgreen") +
  labs(title = "Residuals of Poisson EWMA for Weekly WNV 2017",
       x = "Week",
       y = "Residuals") +
  theme_minimal()
dev.off()
# ACF plot of residuals
png('2022 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset$residuals, main = "2022 Weekly ACF of Residuals- Poisson EWMA")
dev.off()




# ACF plot of residuals
png('2021 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset2021$residuals, main = "2021 Weekly ACF of Residuals- Poisson EWMA")
dev.off()

png('2020 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset2020$residuals, main = "2020 Weekly ACF of Residuals- Poisson EWMA")
dev.off()


png('2019 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset2019$residuals, main = "2019 Weekly ACF of Residuals- Poisson EWMA")
dev.off()


png('2018 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset2018$residuals, main = "2018 Weekly ACF of Residuals- Poisson EWMA")
dev.off()

png('2017 Weekly ACF plot of residuals- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(data_subset2017$residuals, main = "2017 Weekly ACF of Residuals- Poisson EWMA")
dev.off()

png('Plot 2022 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset$residuals, main = "PACF of Weekly WNV 2022 Cumulative Counts- Poisson EWMA")
dev.off()

png('Plot 2021 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset2021$residuals, main = "PACF of Weekly WNV 2021 Cumulative Counts- Poisson EWMA")
dev.off()

png('Plot 2020 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset2020$residuals, main = "PACF of Weekly WNV 2020 Cumulative Counts- Poisson EWMA")
dev.off()

png('Plot 2019 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset2019$residuals, main = "PACF of Weekly WNV 2019 Cumulative Counts - Poisson EWMA")
dev.off()

png('Plot 2018 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset2018$residuals, main = "PACF of Weekly WNV 2018 Cumulative Counts-Poisson EWMA")
dev.off()

png('Plot 2017 Weekly PACF- Poisson EWMA.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(data_subset2017$residuals, main = "PACF of Weekly WNV 2017 Cumulative Counts-Poisson EWMA")
dev.off()

# QQ plot
png('2022 QQ plot.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
qqnorm(data_subset$residuals)
qqline(data_subset$residuals, col = "red")
dev.off()
# Forecast function
forecast_poisson_ewma <- function(last_ewma, lambda, n_forecast) {
  forecast_values <- numeric(n_forecast)
  forecast_values[1] <- last_ewma  # Start with the last EWMA value
  
  for (t in 2:n_forecast) {
    forecast_values[t] <- lambda * forecast_values[t - 1] + (1 - lambda) * last_ewma
  }
  
  return(forecast_values)
}

# Forecast for the next 52 weeks
n_forecast <- 52
last_ewma <- tail(data_subset$ewma, 1)
forecasted_values <- forecast_poisson_ewma(last_ewma, lambda, n_forecast)

last_ewma2021 <- tail(data_subset2021$ewma, 1)
forecasted_values2021 <- forecast_poisson_ewma(last_ewma2021, lambda, n_forecast)

last_ewma2020 <- tail(data_subset2020$ewma, 1)
forecasted_values2020 <- forecast_poisson_ewma(last_ewma2020, lambda, n_forecast)

last_ewma2019 <- tail(data_subset2019$ewma, 1)
forecasted_values2019 <- forecast_poisson_ewma(last_ewma2019, lambda, n_forecast)

last_ewma2018 <- tail(data_subset2018$ewma, 1)
forecasted_values2018 <- forecast_poisson_ewma(last_ewma2018, lambda, n_forecast)

last_ewma2017 <- tail(data_subset2017$ewma, 1)
forecasted_values2017 <- forecast_poisson_ewma(last_ewma2017, lambda, n_forecast)
# Prepare data for plotting the forecast
forecast_Week <- seq(max(data_subset$Week) + 52, by = 1, length.out = n_forecast)
forecast_data <- data.frame(Week = forecast_Week, WNV_2022_CumCounts = forecasted_values)

forecast_Week2021 <- seq(max(data_subset2021$Week) + 52, by = 1, length.out = n_forecast)
forecast_data2021 <- data.frame(Week = forecast_Week2021, WNV_2021_CumCounts = forecasted_values)

forecast_Week2020 <- seq(max(data_subset2020$Week) + 52, by = 1, length.out = n_forecast)
forecast_data2020 <- data.frame(Week = forecast_Week2020, WNV_2020_CumCounts = forecasted_values)

forecast_Week2019 <- seq(max(data_subset2019$Week) + 52, by = 1, length.out = n_forecast)
forecast_data2019 <- data.frame(Week = forecast_Week2019, WNV_2019_CumCounts = forecasted_values)


forecast_Week2018 <- seq(max(data_subset2018$Week) + 52, by = 1, length.out = n_forecast)
forecast_data2018 <- data.frame(Week = forecast_Week2018, WNV_2018_CumCounts = forecasted_values)


forecast_Week2017 <- seq(max(data_subset2017$Week) + 52, by = 1, length.out = n_forecast)
forecast_data2017 <- data.frame(Week = forecast_Week2017, WNV_2017_CumCounts = forecasted_values)
# Combine original and forecasted data for plotting
full_data <- rbind(data_subset[,-c(3,4)], forecast_data)
full_data2021 <- rbind(data_subset2021[,-c(3,4)], forecast_data2021)
full_data2020 <- rbind(data_subset2020[,-c(3,4)], forecast_data2020)
full_data2019 <- rbind(data_subset2019[,-c(3,4)], forecast_data2019)
full_data2018 <- rbind(data_subset2018[,-c(3,4)], forecast_data2018)
full_data2017 <- rbind(data_subset2017[,-c(3,4)], forecast_data2017)

# Plot original data and forecast
png('Plot 2022 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data, aes(x = Week)) +
  geom_line(aes(y = WNV_2022_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data, aes(y = WNV_2022_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2022",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()


png('Plot 2021 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data2021, aes(x = Week)) +
  geom_line(aes(y = WNV_2021_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data2021, aes(y = WNV_2021_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2021",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()


png('Plot 2020 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data2020, aes(x = Week)) +
  geom_line(aes(y = WNV_2020_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data2020, aes(y = WNV_2020_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2020",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()


png('Plot 2019 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data2019, aes(x = Week)) +
  geom_line(aes(y = WNV_2019_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data2019, aes(y = WNV_2019_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2019",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()


png('Plot 2018 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data2018, aes(x = Week)) +
  geom_line(aes(y = WNV_2018_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data2018, aes(y = WNV_2018_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2018",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()


png('Plot 2017 Weekly original data and forecast.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
ggplot(full_data2017, aes(x = Week)) +
  geom_line(aes(y = WNV_2017_CumCounts), color = "blue", size = 1) +
  geom_line(data = forecast_data2017, aes(y = WNV_2017_CumCounts), color = "red", size = 1) +
  labs(title = "Poisson EWMA Forecast for Weekly WNV 2017",
       x = "Week",
       y = "Cumulative Counts") +
  theme_minimal()
dev.off()



############ SARIMA model##########33
# 加载必要的包
library(forecast)

# 将数据转换为时间序列对象
ts_data <- ts(data_subset$WNV_2022_CumCounts, start = c(2022, 1), frequency = 52)
ts_data2021 <- ts(data_subset2021$WNV_2021_CumCounts, start = c(2021, 1), frequency = 52)
ts_data2020 <- ts(data_subset2020$WNV_2020_CumCounts, start = c(2020, 1), frequency = 52)
ts_data2019 <- ts(data_subset2019$WNV_2019_CumCounts, start = c(2019, 1), frequency = 52)
ts_data2018 <- ts(data_subset2018$WNV_2018_CumCounts, start = c(2018, 1), frequency = 52)
ts_data2017 <- ts(data_subset2017$WNV_2017_CumCounts, start = c(2017, 1), frequency = 52)

 

# 绘制时间序列图
png('Plot 2022 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data, main = "WNV 2022 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()

png('Plot 2021 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data2021, main = "WNV 2021 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()

png('Plot 2020 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data2020, main = "WNV 2020 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()

png('Plot 2019 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data2019, main = "WNV 2019 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()

png('Plot 2018 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data2018, main = "WNV 2018 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()

png('Plot 2017 ts data Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(ts_data2017, main = "WNV 2017 Cumulative Counts Weekly", xlab = "Week", ylab = "Cumulative Counts")
dev.off()
# ACF 和 PACF 图
png('Plot 2022 Weekly SARIMA ACF .png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data, main = "SARIMA ACF of 2022 Weekly WNV Cumulative Counts")
dev.off()

png('Plot 2022 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data, main = "SARIMA PACF of 2022 Weekly WNV Cumulative Counts")
dev.off()


# ACF 和 PACF 图
png('Plot 2021 Weekly SARIMA ACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data2021, main = "SARIMA ACF of Weekly WNV 2021 Cumulative Counts")
dev.off()

png('Plot 2021 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data2021, main = "SARIMA PACF of Weekly WNV 2021 Cumulative Counts")
dev.off()

png('Plot 2020 Weekly SARIMA ACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data2020, main = "SARIMA ACF of Weekly WNV 2020 Cumulative Counts")
dev.off()

png('Plot 2020 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data2020, main = "SARIMA PACF of Weekly WNV 2020 Cumulative Counts")
dev.off()
png('Plot 2019 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data2019, main = "SARIMA PACF of Weekly WNV 2019 Cumulative Counts")
dev.off()

png('Plot 2019 Weekly SARIMA ACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data2019, main = "SARIMA ACF of Weekly WNV 2019 Cumulative Counts")
dev.off()

png('Plot 2018 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data2018, main = "SARIMA PACF of Weekly WNV 2018 Cumulative Counts")
dev.off()

png('Plot 2018 Weekly SARIMA ACF .png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data2018, main = "SARIMA ACF of 2018 Weekly WNV Cumulative Counts")
dev.off()

png('Plot 2017 Weekly SARIMA ACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Acf(ts_data2017, main = "SARIMA ACF of Weekly WNV 2017 Cumulative Counts")
dev.off()

png('Plot 2017 Weekly SARIMA PACF.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
Pacf(ts_data2017, main = "SARIMA PACF of Weekly WNV 2017 Cumulative Counts")
dev.off()

# 拟合 SARIMA 模型
model <- auto.arima(ts_data, seasonal = TRUE)
model2021 <- auto.arima(ts_data2021, seasonal = TRUE)
model2020 <- auto.arima(ts_data2020, seasonal = TRUE)
model2019 <- auto.arima(ts_data2019, seasonal = TRUE)
model2018 <- auto.arima(ts_data2018, seasonal = TRUE)
model2017 <- auto.arima(ts_data2017, seasonal = TRUE)


# 查看模型摘要
summary(model)
# 进行预测
n_forecast <- 52  # 预测未来 
forecasted_values <- forecast(model, h = n_forecast)
forecasted_values2021 <- forecast(model2021, h = n_forecast)
forecasted_values2020 <- forecast(model2020, h = n_forecast)
forecasted_values2019 <- forecast(model2019, h = n_forecast)
forecasted_values2018 <- forecast(model2018, h = n_forecast)
forecasted_values2017 <- forecast(model2017, h = n_forecast)

# 绘制预测结果
png('Plot prediction result-2022 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()

png('Plot prediction result-2021 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()

png('Plot prediction result-2020 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()

png('Plot prediction result-2019 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()

png('Plot prediction result-2018 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()

png('Plot prediction result-2017 Weekly.png',
    height = 15,
    width = 25,
    units = 'cm',
    res = 300)
plot.new()
plot(forecasted_values)
dev.off()
# 残差分析
checkresiduals(model)



