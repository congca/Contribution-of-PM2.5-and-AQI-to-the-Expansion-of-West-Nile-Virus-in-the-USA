# Load necessary packages
library(tidyverse)
library(mgcv)
library(tensorflow)
library(keras)
library(readxl)

# Set working directory
setwd("C:/Users/congc/Desktop/WNV")

# Read data from Excel
excel_file <- "WNV.xlsx"
data <- read_excel(excel_file, sheet = 1)

# Rename columns
colnames(data) <- c("State", "Neuroinvasive_2016", "Non_neuroinvasive_2016", "Neuroinvasive_2017", 
                    "Non_neuroinvasive_2017", "Neuroinvasive_2018", "Non_neuroinvasive_2018", 
                    "Neuroinvasive_2019", "Non_neuroinvasive_2019", "Neuroinvasive_2020", 
                    "Non_neuroinvasive_2020", "Neuroinvasive_2021", "Non_neuroinvasive_2021", 
                    "AQI_2016", "PM2.5_2016", "AQI_2017", "PM2.5_2017", "AQI_2018", "PM2.5_2018", 
                    "AQI_2019", "PM2.5_2019", "AQI_2020", "PM2.5_2020", "AQI_2021", "PM2.5_2021")
R.version.string
# Display data structure
print(data)
dim(data)
str(data)
stat(data)
data$Neuroinvasive_2016 <- as.numeric(as.factor(data$Neuroinvasive_2016))
data$Neuroinvasive_2017 <- as.numeric(as.factor(data$Neuroinvasive_2017))
data$Neuroinvasive_2018 <- as.numeric(as.factor(data$Neuroinvasive_2018))
data$Neuroinvasive_2019 <- as.numeric(as.factor(data$Neuroinvasive_2019))
data$Neuroinvasive_2020 <- as.numeric(as.factor(data$Neuroinvasive_2020))
data$Neuroinvasive_2021 <- as.numeric(as.factor(data$Neuroinvasive_2021))
data$Non_neuroinvasive_2016 <- as.numeric(as.factor(data$Non_neuroinvasive_2016))
data$Non_neuroinvasive_2017 <- as.numeric(as.factor(data$Non_neuroinvasive_2017))
data$Non_neuroinvasive_2018 <- as.numeric(as.factor(data$Non_neuroinvasive_2018))
data$Non_neuroinvasive_2019 <- as.numeric(as.factor(data$Non_neuroinvasive_2019))
data$Non_neuroinvasive_2020 <- as.numeric(as.factor(data$Non_neuroinvasive_2020))
data$Non_Neuroinvasive_2021 <- as.numeric(as.factor(data$Non_Neuroinvasive_2021))
summary(data)
# Convert data to long format
data_long <- data %>%
  pivot_longer(cols = starts_with("Neuroinvasive") | starts_with("Non_neuroinvasive") | starts_with("AQI") | starts_with("PM2.5"),
               names_to = c(".value", "year"),
               names_pattern = "(.*)_(\\d+)")

# Convert year to numeric
data_long$year <- as.numeric(data_long$year)

# Ensure variables are numeric
data_long$Neuroinvasive <- as.numeric(data_long$Neuroinvasive)
data_long$Non_neuroinvasive <- as.numeric(data_long$Non_neuroinvasive)
data_long$AQI <- as.numeric(data_long$AQI)
data_long$PM2.5 <- as.numeric(data_long$PM2.5)

# Handle missing values
data_long <- na.omit(data_long)



png('Neuroinvasive Cases Over Years by State.png',
    height = 35,
    width = 55,
    units = 'cm',
    res = 300)
plot.new()

# Visualize data
ggplot(data_long, aes(x = year, y = Neuroinvasive, group = State, color = State)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal() +
  labs(title = "Neuroinvasive Cases Over Years by State", x = "Year", y = "Neuroinvasive Cases")
dev.off()

png('Non-Neuroinvasive Cases Over Years by State.png',
    height = 35,
    width = 55,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_long, aes(x = year, y = Non_neuroinvasive, group = State, color = State)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal() +
  labs(title = "Non-neuroinvasive Cases Over Years by State", x = "Year", y = "Non-neuroinvasive Cases")
dev.off()

png('PM2.5 Over Years by State.png',
    height = 35,
    width = 55,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_long, aes(x = year, y = PM2.5, group = State, color = State)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal() +
  labs(title = "PM2.5 Over Years by State", x = "Year", y = "PM2.5")
dev.off()

png('AQI Over Years by State.png',
    height = 35,
    width = 55,
    units = 'cm',
    res = 300)
plot.new()
ggplot(data_long, aes(x = year, y = AQI, group = State, color = State)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal() +
  labs(title = "AQI Over Years by State", x = "Year", y = "AQI")
dev.off()
# Counterfactual policy analysis using linear model
summary(data_long$Neuroinvasive)

# Assume 10% reduction in PM2.5
data_long$PM2.5_policy <- data_long$PM2.5 * 0.9

# Policy impact analysis
policy_model <- glm(Neuroinvasive ~ PM2.5_policy + AQI + factor(year), data = data_long, family = gaussian())
summary(policy_model)

# Predict policy effect
data_long$Neuroinvasive_policy_pred <- predict(policy_model, type = "response")

# Compare actual vs predicted values
policy_effect <- data_long %>%
  select(State, year, Neuroinvasive, Neuroinvasive_policy_pred) %>%
  pivot_longer(cols = c(Neuroinvasive, Neuroinvasive_policy_pred),
               names_to = "Type", values_to = "Count")

png('Actual vs Predicted Neuroinvasive Cases Under Policy.png',
    height = 35,
    width = 55,
    units = 'cm',
    res = 300)
plot.new()
ggplot(policy_effect, aes(x = year, y = Count, group = Type, color = Type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ State, scales = "free_y") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Neuroinvasive Cases Under Policy", x = "Year", y = "Count")
dev.off()
# GAM model for causal relationship
gam_model <- gam(Neuroinvasive ~ s(PM2.5) + s(AQI) + factor(year), data = data_long, family = gaussian())
summary(gam_model)

# Visualize GAM model
png('Visualize GAM model.png',
    height = 15,
    width = 30,
    units = 'cm',
    res = 300)
plot.new()
par(mfrow = c(2, 2))
plot(gam_model, rug = TRUE)
dev.off()


# 解释结果
# s(PM2.5): PM2.5的平滑项，如果该项的edf（有效自由度）显著大于1，说明存在非线性关系。
# s(AQI): AQI的平滑项，同样判断其非线性关系。
# factor(year): 年份的因子变量，用于控制不同年份间的差异。
# 解释结果
# s(PM2.5): PM2.5的平滑项，如果该项的edf（有效自由度）显著大于1，说明存在非线性关系。
# s(AQI): AQI的平滑项，同样判断其非线性关系。
# factor(year): 年份的因子变量，用于控制不同年份间的差异。
# Causal deep learning
# Convert data to matrix format for deep learning
x <- as.matrix(data_long %>% select(-Neuroinvasive, -Non_neuroinvasive, -State, -year))
y <- as.matrix(data_long$Neuroinvasive)


