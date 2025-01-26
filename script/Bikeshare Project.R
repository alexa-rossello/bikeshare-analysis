# ---- Bikeshare Analysis Script ----
# Purpose: Analyze bikeshare and weather data to assess usage patterns.
# Includes data loading, transformation, statistical tests, and modeling.
# Author: Alexa Rossello

# ---- Load Libraries ----
library(tidyverse)
library(dplyr)
library(e1071)
library(MASS)
library(car)
library(FSA)
library(randomForest)
library(ggplot2)
library(here)

# Load and Transform Data ----

# Read in bikshare data --

data_path <- here("data")

# Reference the data folder
file_names <- list.files(
  path = here("data"),  # Directly reference the data folder
  pattern = "capitalbikeshare-tripdata.*\\.csv$",
  full.names = TRUE
)

# Read and combine the files into one data frame
bike_data <- lapply(file_names, read.csv) %>%
  bind_rows()

# Check the first few rows of the combined dataset
head(bike_data)

# Investigate rideable type - what is the difference between classic, docked, and electric?
unique(bike_data$rideable_type)
table(bike_data$rideable_type)

# Plot the distribution of the three types
p1 <- ggplot(bike_data, aes(x = rideable_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Rideable Types", x = "Rideable Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the plot
p1

# Save the plot
ggsave(
  filename = here("report", "rideable_types.png"), 
  plot = p1, 
  width = 8, height = 6, dpi = 300)


# Plot the distribution of member and casual
p13 <- ggplot(bike_data, aes(x = member_casual)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Rider Types", x = "Rider Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the plot
p13

# Save the plot
ggsave(
  filename = here("report", "rider_types.png"), 
  plot = p13, 
  width = 8, height = 6, dpi = 300)


# Bikeshare data transformation --

# Convert started_at and ended_at to datetime format
bike_data$started_at <- as.POSIXct(bike_data$started_at, format = "%Y-%m-%d %H:%M:%S")
bike_data$ended_at <- as.POSIXct(bike_data$ended_at, format = "%Y-%m-%d %H:%M:%S")

# Create a new column for ride duration (in minutes)
bike_data$ride_duration <- as.numeric(difftime(bike_data$ended_at, bike_data$started_at, units = "mins"))

# Create a new column for date component of 'started_at'
bike_data$Date <- as.Date(bike_data$started_at)

# Check the first few rows after conversion
head(bike_data)

# Check for missing values
summary(bike_data)

# Check for missing data in specific columns
summary(bike_data[, c("start_station_id", "end_station_id", "end_lat", "end_lng")])

# There is a large number of missing data points regarding station name and ID. 
# This is fine since the analysis will focus on weather impacts by rider type.


# Read in weather data --

# File name
weather_data <- read.csv("data/washington dc 2023-01-01 to 2023-12-31.csv")

# Examine the first few rows of data
head(weather_data)

# Check data structure
str(weather_data)

# Convert datetime to a Date variable
weather_data$datetime <- as.Date(weather_data$datetime)


# Combine bike and weather data --
combined_data <- merge(bike_data, weather_data, by.x = "Date", by.y = "datetime", all.x = TRUE)

# Check the first few rows of the combined data
head(combined_data)

# Create the daily_count column
daily_rides <- combined_data %>%
  group_by(Date) %>%
  summarise(daily_count = n(), .groups = "drop")

# Merge daily_rides back into combined_data based on Date
combined_data <- merge(combined_data, daily_rides, by = "Date", all.x = TRUE)

# Check the first few rows to ensure daily_count is added
head(combined_data)


# Distribution Visualization Functions ----

# Plot histogram of daily ride counts
p2 <- ggplot(combined_data, aes(x = daily_count)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Daily Ride Counts", x = "Daily Ride Count", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
p2

# Save the plot
ggsave(
  filename = here("report","daily_rides_hist.png"),
  plot = p2, 
  width = 8, height = 6, dpi = 300)

# Plot density of daily ride counts
p3 <- ggplot(combined_data, aes(x = daily_count)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Daily Ride Counts", x = "Daily Ride Count", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
p3

# Save the plot
ggsave(
  filename = here("report", "daily_rides_density.png"), 
  plot = p3, 
  width = 8, height = 6, dpi = 300)


# Log Transformation ----

# Calculate skewness of daily ride counts
skewness_value <- skewness(combined_data$daily_count, na.rm = TRUE)
skewness_value

# Log transform the daily count and add it to the dataset
combined_data$log_daily_count <- log1p(combined_data$daily_count)  # log1p handles zero values

# Plot density of log-transformed daily ride counts
p4 <- ggplot(combined_data, aes(x = log_daily_count)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Log-Transformed Daily Ride Counts", x = "Log(Daily Ride Count + 1)", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
p4

# Save the plot
ggsave(
  filename = here("report", "log_xform_rides.png"), 
  plot = p4, 
  width = 8, height = 6, dpi = 300)


# Box-Cox Transformation ----

# Apply Box-Cox transformation
box_cox_result <- boxcox(daily_count ~ 1, data = combined_data, lambda = seq(-2, 2, 0.1))

# Get the optimal lambda value
optimal_lambda <- box_cox_result$x[which.max(box_cox_result$y)]
optimal_lambda

# Apply the Box-Cox transformation to daily_count using the optimal lambda
if (optimal_lambda == 0) {
  combined_data$daily_count_boxcox <- log(combined_data$daily_count)
} else {
  combined_data$daily_count_boxcox <- (combined_data$daily_count^optimal_lambda - 1) / optimal_lambda
}

# Plot the density of the transformed data to evaluate the distribution
p5 <- ggplot(combined_data, aes(x = daily_count_boxcox)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Box-Cox Transformed Daily Ride Counts", x = "Box-Cox Transformed Daily Ride Count", y = "Density") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Display the plot
p5

# Save the plot
ggsave(
  filename = here("report", "boxcox_xform_rides.png"), 
  plot = p5, 
  width = 8, height = 6, dpi = 300)



# Create categorical variables for temp and rain ----

# Categorize temperature into three groups: cold, moderate, and hot
combined_data$temp_category <- cut(combined_data$temp, 
                                   breaks = c(-Inf, 50, 70, Inf), 
                                   labels = c("Cold", "Moderate", "Hot"))

# Plot the distribution of daily rides by temperature category
p6 <- ggplot(combined_data, aes(x = temp_category, y = daily_count, fill = temp_category)) +
  geom_boxplot() +
  labs(title = "Daily Bike Rides by Temperature Category", x = "Temperature Category", y = "Daily Ride Count") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Display the plot
p6

# Save the plot
ggsave(
  filename = here("report", "boxplot_rides_temp.png"), 
  plot = p6, 
  width = 8, height = 6, dpi = 300)

# Create a new categorical variable for rain intensity
combined_data$rain_category <- cut(combined_data$precip, 
                                   breaks = c(-Inf, 0, 0.1, Inf),  # Defining categories
                                   labels = c("No Rain", "Light Rain", "Heavy Rain"))

# Check the distribution of the new variable
table(combined_data$rain_category)

# Plot the distribution of daily rides by rain category
ggplot(combined_data, aes(x = rain_category, y = daily_count, fill = rain_category)) +
  geom_boxplot() +
  labs(title = "Daily Bike Rides by Rain Category", x = "Rain Category", y = "Daily Ride Count") +
  theme_minimal()


# Create a new combined category for temperature and rain
combined_data$temp_rain_category <- paste(combined_data$temp_category, combined_data$rain_category, sep = " & ")

# Check the distribution of the new combined category
table(combined_data$temp_rain_category)


# Reorder levels to specify the desired order
combined_data <- combined_data %>%
  mutate(temp_rain_category = factor(temp_rain_category,
                                     levels = c(
                                       "Cold & No Rain", "Cold & Light Rain", "Cold & Heavy Rain",
                                       "Moderate & No Rain", "Moderate & Light Rain", "Moderate & Heavy Rain",
                                       "Hot & No Rain", "Hot & Light Rain", "Hot & Heavy Rain"
                                     )))

# Plot the distribution of daily rides by combined rain and temperature category
p7 <- ggplot(combined_data, aes(x = temp_rain_category, y = daily_count, fill = temp_rain_category)) +
  geom_boxplot() +
  labs(title = "Daily Bike Rides by Temperature/Rain Category", 
       x = "Temperature/Rain Category", 
       y = "Daily Ride Count") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "purple") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the plot
p7

# Save the plot
ggsave(
  filename = here("report", "boxplot_rides_temp.png"), 
  plot = p7, 
  width = 8, height = 6, dpi = 300)


# Statistical Tests -----

# Subsample for Shapiro-Wilk normality test
set.seed(123) # For reproducibility
normality_temp <- combined_data %>%
  group_by(temp_category) %>%
  summarise(
    p_value = {
      sample_data <- sample(daily_count, min(5000, n())) # Subsample if >5000
      shapiro.test(sample_data)$p.value
    }
  )

print(normality_temp)

normality_rain <- combined_data %>%
  group_by(rain_category) %>%
  summarise(
    p_value = {
      sample_data <- sample(daily_count, min(5000, n()))
      shapiro.test(sample_data)$p.value
    }
  )

print(normality_rain)

normality_combined <- combined_data %>%
  group_by(temp_rain_category) %>%
  summarise(
    p_value = {
      sample_data <- sample(daily_count, min(5000, n()))
      shapiro.test(sample_data)$p.value
    }
  )

print(normality_combined)


# Mann-Whitney U Test for Temperature by Rider Type
wilcox_temp_test <- wilcox.test(temp ~ member_casual, data = combined_data)
print(wilcox_temp_test)

# Mann-Whitney U Test for Precipitation by Rider Type
wilcox_precip_test <- wilcox.test(precip ~ member_casual, data = combined_data)
print(wilcox_precip_test)


# Perform Kruskal-Wallis Test for Temperature Categories
kruskal_temp <- kruskal.test(daily_count ~ temp_category, data = combined_data)

# Display the results
print(kruskal_temp)

# Perform Kruskal-Wallis Test for Rain Categories
kruskal_rain <- kruskal.test(daily_count ~ rain_category, data = combined_data)

# Display the results
print(kruskal_rain)

# Perform Kruskal-Wallis Test for Combined Temperature and Rain Categories
kruskal_combined <- kruskal.test(daily_count ~ temp_rain_category, data = combined_data)

# Display the results
print(kruskal_combined)



# Visually check temp category and daily rides for normality ----

# Create density plots for daily ride counts by temperature category
p8 <- ggplot(combined_data, aes(x = daily_count, fill = temp_category)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density Plot of Daily Rides by Temperature Category",
    x = "Daily Ride Count",
    y = "Density",
    fill = "Temperature"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Display the plot
p8

# Save the plot
ggsave(
  filename = here("report", "rides_by_temp.png"), 
  plot = p8, 
  width = 8, height = 6, dpi = 300)


# Create density plots for daily ride counts by rain category
p9 <- ggplot(combined_data, aes(x = daily_count, fill = rain_category)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density Plot of Daily Rides by Rain Category",
    x = "Daily Ride Count",
    y = "Density",
    fill = "Rain"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Display the plot
p9

# Save the plot
ggsave(
  filename = here("report", "rides_by_precip.png"), 
  plot = p9, 
  width = 8, height = 6, dpi = 300)


# Create density plots for daily ride counts by combined temperature and rain categories
p10 <- ggplot(combined_data, aes(x = daily_count, fill = temp_rain_category)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density Plot of Daily Rides by Combined Temperature and Rain Categories",
    x = "Daily Ride Count",
    y = "Density",
    fill = "Temp & Rain"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Display the plot
p10

# Save the plot
ggsave(
  filename = here("report", "rides_by_tempprecip.png"), 
  plot = p10, 
  width = 8, height = 6, dpi = 300)


# Modeling ----

# Negative Binomial Regression --
mean_counts <- mean(combined_data$daily_count, na.rm = TRUE)
var_counts<- var(combined_data$daily_count, na.rm = TRUE)
overdispersion <- var_counts/mean_counts # result = 554.99, confirming overdispersion

# Construct the model
nb_model_interaction <- glm.nb(
  daily_count ~ temp + precip + member_casual + 
    temp:member_casual + precip:member_casual,
  data = combined_data
)

summary(nb_model_interaction)

# Generate new data for prediction
new_data <- expand.grid(
  temp = seq(min(combined_data$temp), max(combined_data$temp), length.out = 100),
  precip = c(0, 0.5, 1.0), # Representative precipitation levels
  member_casual = c("casual", "member")
)

# Add predictions
new_data$predicted_count <- predict(nb_model_interaction, newdata = new_data, type = "response")

# Plot predictions
p11 <- ggplot(new_data, aes(x = temp, y = predicted_count, color = member_casual, linetype = as.factor(precip))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Effect of Temperature and Precipitation on Ridership by Rider Type",
    x = "Temperature (°F)",
    y = "Predicted Daily Count",
    color = "Rider Type",
    linetype = "Precipitation Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Display the plot
p11

# Save the plot
ggsave(
  filename = here("report", "reg_predictions.png"), 
  plot = p11, 
  width = 8, height = 6, dpi = 300)


# Random Forest --

# Prepare data: Select relevant variables
rf_data <- combined_data %>%
  dplyr::select(daily_count, temp, precip, member_casual) %>% # Explicit dplyr::select
  na.omit() # Remove missing values

# Convert categorical variable to factor
rf_data$member_casual <- as.factor(rf_data$member_casual)

# Fit Random Forest model
set.seed(123)
rf_data_sample <- rf_data %>%
  group_by(member_casual) %>%
  sample_n(50000) %>% # Adjust sample size per group
  ungroup()

# Fit the model
rf_model <- randomForest(
  daily_count ~ temp + precip + member_casual, 
  data = rf_data_sample,
  ntree = 500, 
  mtry = 2, 
  importance = TRUE
)

# Print model summary
print(rf_model)

# Assess feature importance
importance <- importance(rf_model)

# Display varImpPlot in the plot viewer
varImpPlot(rf_model, main = "Variable Importance in Random Forest Model")

# Save varImpPlot as a PNG
png(filename = here("report", "varImpPlot.png"))

# Generate the importance plot
varImpPlot(rf_model, main = "Variable Importance in Random Forest Model")

# Close the graphics device
dev.off()

# Generate new data for prediction
new_data_rf <- expand.grid(
  temp = seq(min(rf_data$temp), max(rf_data$temp), length.out = 100),
  precip = c(0, 0.5, 1.0), # Example precipitation levels
  member_casual = c("casual", "member")
)

# Add predictions
new_data_rf$predicted_count <- predict(rf_model, newdata = new_data_rf)

# Plot predictions
p12 <- ggplot(new_data_rf, aes(x = temp, y = predicted_count, color = member_casual, linetype = as.factor(precip))) +
  geom_line(linewidth = 1) +
  labs(
    title = "Random Forest: Predicted Daily Count by Temperature and Rider Type",
    x = "Temperature (°F)",
    y = "Predicted Daily Count",
    color = "Rider Type",
    linetype = "Precipitation Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
p12

# Save the plot
ggsave(
  filename = here("report", "rf_predictions.png"), 
  plot = p12, 
  width = 8, height = 6, dpi = 300)

