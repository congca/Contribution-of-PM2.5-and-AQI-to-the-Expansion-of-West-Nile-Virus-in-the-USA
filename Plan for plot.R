# Load necessary packages
library(tidyverse)
library(sf)  # For spatial operations

library(tidyverse)
library(ggplot2)
library(mapdata)
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
# 合并地图数据
map_data <- map_data("state")
# 转换数据到长格式

state = as.character(state) # 显式转换 State 列为字符型

# 合并地图数据
map_data <- map_data("state")

# 检查地图数据的列名
colnames(map_data)

# 将地图数据的州名列名改为 State
colnames(map_data)[colnames(map_data) == "region"] <- "State"



# 将州名与地图数据合并
map_data <- left_join(map_data, data_long, by = "State")

# 绘制地图
ggplot() +
  geom_map(data = map_data, map = map_data,
           aes(x = long, y = lat, map_id = State, fill = Cases),
           color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Neuroinvasive Cases", labels = scales::comma) +
  labs(title = "Neuroinvasive Cases by State") +
  theme_void()
