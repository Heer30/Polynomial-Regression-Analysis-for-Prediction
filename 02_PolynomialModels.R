#Polynomial Models
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

summary(poly_2)

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

df_poly <- df_model %>%
  filter(
    total_resource_kg > 0,
    a_sqkm > 0
  ) %>%
  mutate(
    log_total_resource_kg = log(total_resource_kg),
    log_a_sqkm = log(a_sqkm)
  )

poly_log <- lm(
  log_total_resource_kg ~ poly(log_a_sqkm, 2, raw = TRUE),
  data = df_poly
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

