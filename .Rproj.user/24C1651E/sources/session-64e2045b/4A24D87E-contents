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

#Linear Model
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
  theme_minimal

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

poly_1 <- lm(
  total_resource_kg ~ poly(a_sqkm, 2, raw = TRUE),
  data = df_model
)

summary(poly_1)
AIC(linear_model2, poly_1)

poly_2 <- lm(
  total_resource_kg ~ poly(a_sqkm, 3, raw = TRUE),
  data = df_model
)

AIC(linear_model2, poly_1, poly_2)

ggplot(df_model, aes(x = a_sqkm, y = total_resource_kg)) +
  geom_point(alpha = 0.25, color = "steelblue") +
  stat_smooth(
    method = "lm",
    formula = y ~ poly(x, 2, raw = TRUE),
    color = "red",
    se = FALSE,
    linewidth = 1
  ) +
  labs(
    title = "Quadratic Polynomial Fit: Area vs Hydrogen Production",
    x = "Area (sq km)",
    y = "Hydrogen Production Potential (kg)"
  ) +
  theme_minimal()

poly_log <- lm(
  log(total_resource_kg) ~ poly(log(a_sqkm), 2, raw = TRUE),
  data = df_model
)
summary(poly_log)
AIC(poly_log)
ggplot(df_model, aes(x = a_sqkm, y = total_resource_kg)) +
  geom_point(alpha = 0.25) +
  stat_smooth(
    method = "lm",
    formula = y ~ poly(log(x), 2, raw = TRUE),
    se = FALSE,
    color = "red"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

sum(df_model$total_resource_kg <= 0, na.rm = TRUE)

df_model$log_total_resource_kg <- log(df_model$total_resource_kg + 1)
df_model$log_a_sqkm <- log(df_model$a_sqkm + 1)

poly_log <- lm(
  log_total_resource_kg ~ poly(log_a_sqkm, 2, raw = TRUE),
  data = df_model
)

summary(poly_log)
AIC(poly_log)

ggplot(df_model, aes(x = a_sqkm, y = total_resource_kg)) +
  geom_point(alpha = 0.3) +
  stat_smooth(
    aes(y = exp(predict(poly_log)) - 1),
    color = "red",
    size = 1
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Polynomial Regression on Log-Transformed Variables",
    x = "Area (sq km, log scale)",
    y = "Hydrogen Production Potential (kg, log scale)"
  ) +
  theme_minimal()
