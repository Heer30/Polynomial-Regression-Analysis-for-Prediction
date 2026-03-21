
data <- read.csv("data.csv", header = TRUE)
head(data)

names(data)

linear_model1 <- lm(Hydrogen_Production_kg.day ~ Latitude + Longitude + Temperature_C + Electrolyzer_Efficiency_. + Desalination_Power_kW + System_Efficiency_., data = data)

#Summary of the full model
summary(linear_model1)

library(ggplot2)
ggplot(data, aes(x = Temperature_C, y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Temperature vs Hydrogen Production",
       x = "Temperature (°C)",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()

ggplot(data, aes(x = Latitude, y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Latitude vs Hydrogen Production",
       x = "Latitude",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()

ggplot(data, aes(x = Longitude, y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Temperature vs Hydrogen Production",
       x = "Temperature (°C)",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()


ggplot(data, aes(x = Electrolyzer_Efficiency_., y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Temperature vs Hydrogen Production",
       x = "Temperature (°C)",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()


ggplot(data, aes(x = Desalination_Power_kW, y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Temperature vs Hydrogen Production",
       x = "Temperature (°C)",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()


ggplot(data, aes(x = System_Efficiency_., y = Hydrogen_Production_kg.day)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Temperature vs Hydrogen Production",
       x = "Temperature (°C)",
       y = "Hydrogen Production (kg/day)") +
  theme_minimal()

linear_model2 <- lm(Hydrogen_Production_kg.day ~ Latitude + Longitude + Temperature_C + Electrolyzer_Efficiency_. + Desalination_Power_kW + System_Efficiency_.+PV_Power_kW + Wind_Speed_m.s + Wind_Power_kW + Solar_Irradiance_kWh.mÂ..day + System_Efficiency_., data = data)

#Summary of the full model
summary(linear_model2)
library(car)
car::vif(linear_model2)

#Polynomial Full Model Fit
poly_model1 <- lm(
  Hydrogen_Production_kg.day ~ 
    poly(Latitude, 2, raw = TRUE) +
    poly(Longitude, 2, raw = TRUE) +
    poly(Temperature_C, 2, raw = TRUE) +
    poly(Electrolyzer_Efficiency_., 2, raw = TRUE) +
    poly(Desalination_Power_kW, 2, raw = TRUE) +
    poly(System_Efficiency_., 2, raw = TRUE) +
    poly(PV_Power_kW, 2, raw = TRUE) +
    poly(Wind_Speed_m.s, 2, raw = TRUE) +
    poly(Wind_Power_kW, 2, raw = TRUE) +
    poly(Solar_Irradiance_kWh.mÂ..day, 2, raw = TRUE),
  data = data
)
coef(poly_model1)
summary(poly_model1)
AIC(linear_model2, poly_model1)

vars <- c("Latitude", "Longitude", "Temperature_C", 
          "Electrolyzer_Efficiency_.", "Desalination_Power_kW",
          "System_Efficiency_.", "PV_Power_kW",
          "Wind_Speed_m.s", "Wind_Power_kW",
          "Solar_Irradiance_kWh.m..day")

for (v in vars) {
  if (any(data[[v]] == 0, na.rm = TRUE)) {
    message(paste("Variable", v, "contains zero values. Applying +0.0001 shift."))
    data[[v]] <- data[[v]] + 0.0001
  }
}
poly_model_inverse <- lm(
  Hydrogen_Production_kg.day ~ 
    Latitude + I(Latitude^2) + I(1/Latitude) + I(1/(Latitude^2)) +
    Longitude + I(Longitude^2) + I(1/Longitude) + I(1/(Longitude^2)) +
    Temperature_C + I(Temperature_C^2) + I(1/Temperature_C) + I(1/(Temperature_C^2)) +
    Electrolyzer_Efficiency_. + I(Electrolyzer_Efficiency_.^2) +
    I(1/Electrolyzer_Efficiency_.) + I(1/(Electrolyzer_Efficiency_.^2)) +
    Desalination_Power_kW + I(Desalination_Power_kW^2) +
    I(1/Desalination_Power_kW) + I(1/(Desalination_Power_kW^2)) +
    System_Efficiency_. + I(System_Efficiency_.^2) +
    I(1/System_Efficiency_.) + I(1/(System_Efficiency_.^2)) +
    PV_Power_kW + I(PV_Power_kW^2) + I(1/PV_Power_kW) + I(1/(PV_Power_kW^2)) +
    Wind_Speed_m.s + I(Wind_Speed_m.s^2) +
    I(1/Wind_Speed_m.s) + I(1/(Wind_Speed_m.s^2)) +
    Wind_Power_kW + I(Wind_Power_kW^2) +
    I(1/Wind_Power_kW) + I(1/(Wind_Power_kW^2)) +
    Solar_Irradiance_kWh.mÂ..day + I(Solar_Irradiance_kWh.mÂ..day^2) +
    I(1/Solar_Irradiance_kWh.mÂ..day) + I(1/(Solar_Irradiance_kWh.mÂ..day^2)),
  data = data
)
summary(poly_model_inverse)


AIC(linear_model2, poly_model1, poly_model_inverse)