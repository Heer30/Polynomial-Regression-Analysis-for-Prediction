# Linear Models

data <- read.csv("data1.csv", header = TRUE)
head(data)

names(data)

#Cleaning Data - removing tonnes from the regression
df_raw <- data
df_model <- df_raw
class(df_model)
df_model$solar_metric_tons <- NULL
df_model$wind_metric_tons  <- NULL
names(df_model)

library(dplyr)

df_log <- df_model %>%
  filter(
    total_resource_kg > 0,
    tot_kg > 0,
    mms_refineries_kg > 0,
    mms_ammonia_kg > 0,
    mms_biofuels_kg > 0
  )
#Linear Model 1
linear_model1 <- lm(
  total_resource_kg ~ 
    solar_kg + 
    wind_kg + 
    a_sqkm,
  data = df_model
)

summary(linear_model1)

#GG Plot
library(ggplot2)

ggplot(df_model, aes(x = solar_kg, y = total_resource_kg)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Solar Resource vs Hydrogen Energy Production",
    x = "Solar Resource (kg)",
    y = "Hydrogen Energy Production (kg)"
  ) +
  theme_minimal()

#Linear Model 1 just shows how the total energy produced is from solar and wind they are in the sum of the total energy 

#Linear Model 2 - Given location and industrial context, what affects hydrogen production potential
linear_model2 <- lm(
  total_resource_kg ~ 
    a_sqkm +
    mms_refineries_kg +
    mms_ammonia_kg,
  data = df_model
)

summary(linear_model2)

ggplot(df_model, aes(x = a_sqkm, y = total_resource_kg)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Land Area vs Hydrogen Production Potential",
    x = "Area (sq km)",
    y = "Total Hydrogen Production Potential (kg)"
  ) +
  theme_minimal()
#Log of Model 2
log_model2 <- lm(
  log(total_resource_kg) ~ 
    a_sqkm +
    log(mms_refineries_kg) +
    log(mms_ammonia_kg),
  data = df_log
)

summary(log_model2)

ggplot(df_model, aes(x = a_sqkm, y = total_resource_kg)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Land Area vs Hydrogen Production Potential (Log–Log Scale)",
    x = "Area (sq km, log scale)",
    y = "Hydrogen Production Potential (kg, log scale)"
  ) +
  theme_minimal()


#Predicting Demand
linear_model3 <- lm(
  tot_kg ~ 
    mms_refineries_kg +
    mms_ammonia_kg +
    mms_biofuels_kg,
  data = df_model
)

summary(linear_model3)

#Refineries vs Demand
ggplot(df_model, aes(x = mms_refineries_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Refinery Activity vs Hydrogen Demand",
    x = "Refinery Demand (kg)",
    y = "Total Hydrogen Demand (kg)"
  ) +
  theme_minimal()
#Log of Model 3:
log_model3 <- lm(
  log(tot_kg) ~ 
    log(mms_refineries_kg) +
    log(mms_ammonia_kg) +
    log(mms_biofuels_kg),
  data = df_log
)

summary(log_model3)

#Log scale of refineries vs demand
ggplot(df_model, aes(x = mms_refineries_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Refinery Activity vs Hydrogen Demand (Log-Log Scale)",
    x = "Refinery Demand (kg, log scale)",
    y = "Total Hydrogen Demand (kg, log scale)"
  ) +
  theme_minimal()

#Ammonia vs Demand
ggplot(df_model, aes(x = mms_ammonia_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Ammonia Production vs Hydrogen Demand",
    x = "Ammonia Demand (kg)",
    y = "Total Hydrogen Demand (kg)"
  ) +
  theme_minimal()

#Log scale

ggplot(df_model, aes(x = mms_ammonia_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Ammonia Production vs Hydrogen Demand (Log-Log Scale)",
    x = "Ammonia Demand (kg, log scale)",
    y = "Total Hydrogen Demand (kg, log scale)"
  ) +
  theme_minimal()

#Biofuels vs Demand
ggplot(df_model, aes(x = mms_biofuels_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Biofuels Production vs Hydrogen Demand",
    x = "Biofuels Demand (kg)",
    y = "Total Hydrogen Demand (kg)"
  ) +
  theme_minimal()

#Log scale
ggplot(df_model, aes(x = mms_biofuels_kg, y = tot_kg)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Biofuels Production vs Hydrogen Demand (Log-Log Scale)",
    x = "Biofuels Demand (kg, log scale)",
    y = "Total Hydrogen Demand (kg, log scale)"
  ) +
  theme_minimal()