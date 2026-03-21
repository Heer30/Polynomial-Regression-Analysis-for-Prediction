# ====================================================
# Polynomial Regression for Hydrogen Demand Prediction
# Purpose: Coursework analysis (RStudio)
# Data source: NREL county-level dataset
# ====================================================
# STEP 1: Load packages and data
getwd()
data <- read.csv("data1.csv")
str(data)
summary(data)
names(data)
# STEP 2: Inspect and understand the data
# Response

#tot_kg: total hydrogen serviceable consumption potential (county-level)

#Predictors

#Industrial: mms_ammonia_kg, mms_refineries_kg, mms_metals_kg
ammonia_subset <- model_data[model_data$mms_ammonia_kg > 0, 
                             c("mms_ammonia_kg", "tot_kg")]
ammonia_subset <- head(ammonia_subset, 10)
ammonia_subset
mean(ammonia_subset$mms_ammonia_kg)
mean(ammonia_subset$tot_kg)
x <- ammonia_subset$mms_ammonia_kg
y <- ammonia_subset$tot_kg

x_bar <- mean(x)
y_bar <- mean(y)

beta1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
beta1

beta0 <- y_bar - beta1 * x_bar
beta0
y_hat_example <- beta0 + beta1 * x
residuals_example <- y - y_hat_example

RSS <- sum((y - y_hat_example)^2)
TSS <- sum((y - mean(y))^2)
R2 <- 1 - (RSS/TSS)
adj_R2 <- 1 - ((1 - R2) * (10 - 1)) / (10 - 1 - 1)
sigma2 <- RSS / (10 - 1 - 1)

cat("RSS:", RSS, "\n")
cat("TSS:", TSS, "\n")
cat("R2:", R2, "\n")
cat("Adj R2:", adj_R2, "\n")
cat("Sigma squared:", sigma2, "\n")

data.frame(
  y = y,
  y_hat = round(y_hat_example),
  epsilon = round(residuals_example)
)
#Transport: mms_ld_fcev_kg, mms_mhd_fcev_kg

#Energy/storage: mms_season_enrgy_strg_kg



# STEP 3: Prepare variables for modeling

response <- "tot_kg" 

model_data <- data[,c( "tot_kg",
                       "mms_ammonia_kg",
                       "mms_refineries_kg",
                       "mms_metals_kg",
                       "mms_ld_fcev_kg",
                       "mms_mhd_fcev_kg",
                       "mms_season_enrgy_strg_kg",
                       "a_sqkm")]
summary(model_data)
#Control: a_sqkm
colSums(is.na(model_data))

mean(model_data$mms_ammonia_kg == 0)
# STEP 4: Baseline linear regression model
# --- Baseline linear model (raw scale) ---
# Purpose: Provide interpretable benchmark
# Issue: Poor fit due to zero inflation and skewness

model_linear <- lm(
  tot_kg ~ mms_ammonia_kg +
    mms_refineries_kg +
    mms_metals_kg +
    mms_ld_fcev_kg +
    mms_mhd_fcev_kg +
    mms_season_enrgy_strg_kg +
    a_sqkm,
  data = model_data
)

summary(model_linear)

colMeans(model_data == 0) # check structural zeros
hist(model_data$mms_ammonia_kg, breaks = 50)

# Predicted values from the linear model
predicted <- fitted(model_linear)

# Observed values
observed <- model_data$tot_kg

# Scatter plot
plot(predicted, observed,
     xlab = "Predicted Total Hydrogen Demand",
     ylab = "Observed Total Hydrogen Demand",
     main = "Predicted vs Observed Hydrogen Demand",
     pch = 19, col = rgb(0,0,1,0.3)) # semi-transparent blue points

# Add a 45-degree reference line (perfect prediction)
abline(a = 0, b = 1, col = "red", lwd = 2)

png("Rplot1.png", width=800, height=600)
plot(predicted, observed,
     xlab = "Predicted Total Hydrogen Demand",
     ylab = "Observed Total Hydrogen Demand",
     main = "Predicted vs Observed Hydrogen Demand",
     pch = 19, col = rgb(0,0,1,0.3))
abline(a=0,b=1,col="red",lwd=2)
dev.off()


png("residuals_linear.png", width=800, height=600)
plot(fitted(model_linear), residuals(model_linear),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Linear Model",
     pch = 19, col = rgb(0,0,1,0.3))
abline(h = 0, col = "red", lwd = 2)
dev.off()
# STEP 4.2: Log Transformed Linear Regression Model
model_data_log <- transform(
  model_data,
  log_tot_kg = log(tot_kg),
  log_ammonia = log1p(mms_ammonia_kg),
  log_refineries = log1p(mms_refineries_kg),
  log_metals = log1p(mms_metals_kg),
  log_ld_fcev = log1p(mms_ld_fcev_kg),
  log_mhd_fcev = log1p(mms_mhd_fcev_kg),
  log_seasonal = log1p(mms_season_enrgy_strg_kg),
  log_area = log(a_sqkm)
)
summary(model_data_log)

#STEP 4.2 log-linear model
model_linear_log <- lm(
  log_tot_kg ~ log_ammonia +
    log_refineries +
    log_metals +
    log_ld_fcev +
    log_mhd_fcev +
    log_seasonal +
    log_area,
  data = model_data_log
)

summary(model_linear_log)

AIC(model_linear, model_linear_log)

# STEP 5: Polynomial regression model
# Adding squared terms to allow nonlinear scaling of sectoral hydrogen demand

model_poly <- lm(
  log_tot_kg ~ 
    log_ammonia + I(log_ammonia^2) +
    log_refineries + I(log_refineries^2) +
    log_metals + I(log_metals^2) +
    log_ld_fcev + I(log_ld_fcev^2) +
    log_mhd_fcev + I(log_mhd_fcev^2) +
    log_seasonal + I(log_seasonal^2) +
    log_area,
  data = model_data_log
)

summary(model_poly)
plot(model_poly$residuals)
hist(model_poly$residuals, breaks=50)
summary(model_poly)$adj.r.squared
AIC(model_linear_log, model_poly)

model_poly_cubic <- lm(
  log_tot_kg ~ 
    log_ammonia + I(log_ammonia^2) + I(log_ammonia^3) +
    log_refineries + I(log_refineries^2) + I(log_refineries^3) +
    log_metals + I(log_metals^2) + I(log_metals^3) +
    log_ld_fcev + I(log_ld_fcev^2) + I(log_ld_fcev^3) +
    log_mhd_fcev + I(log_mhd_fcev^2) + I(log_mhd_fcev^3) +
    log_seasonal + I(log_seasonal^2) + I(log_seasonal^3) +
    log_area,
  data = model_data_log
)

summary(model_poly_cubic)
AIC(model_linear_log, model_poly, model_poly_cubic)
# STEP 6: Model comparison (prediction performance)
# STEP 6.1: FITTED VS OBSERVED PLOT
# 1. Compute fitted values
fitted_values <- model_poly$fitted.values

# 2. Actual observed values
observed_values <- model_data_log$log_tot_kg

# 3. Basic scatter plot
plot(observed_values, fitted_values,
     xlab = "Observed log(total hydrogen demand)",
     ylab = "Fitted log(total hydrogen demand)",
     main = "Fitted vs Observed Values - Log-Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5)) # semi-transparent points

# 4. Add 45-degree reference line
abline(a = 0, b = 1, col = "red", lwd = 2)

#STEP 6.2 RESIDUALS VS PREDICTORS
# 1. Compute residuals from the log-polynomial model
residuals_poly <- model_poly$residuals

# 2. Predictor to check
predictor <- model_data_log$log_ld_fcev

# 3. Plot residuals vs predictor
plot(predictor, residuals_poly,
     xlab = "log(Light-duty FCEV hydrogen demand)",
     ylab = "Residuals",
     main = "Residuals vs log_ld_fcev - Log-Polynomial Model",
     pch = 16, col = rgb(1,0,0,0.5)) # semi-transparent red points

# 4. Add horizontal line at 0
abline(h = 0, col = "blue", lwd = 2)
# STEP 7: Interpretation and notes

#Examples from Project 
#1 Linear model, OLS and Residuals
data_xy <- data[, c("solar_kg", "tot_kg")]
data_xy <- na.omit(data_xy)
subset_10 <- data_xy[1:10, ]
subset_10

write.csv(subset_10, "example_pairs1.csv", row.names = FALSE)
model <- lm(tot_kg ~ solar_kg, data = subset_10)
summary(model)

subset_10$fitted <- fitted(model)
subset_10$residuals <- residuals(model)

subset_10

plot(subset_10$fitted, subset_10$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0)

plot(subset_10$fitted / 1e6,
     subset_10$residuals / 1e6,
     xlab = "Fitted values (millions)",
     ylab = "Residuals (millions)",
     main = "Residuals vs Fitted (scaled)")
abline(h = 0)

plot(subset_10$solar_kg / 1e9, subset_10$tot_kg / 1e6,
     xlab = "Solar energy (billions)",
     ylab = "Total kg (millions)",
     main = "OLS With Residual Lines (scaled axes)")

abline(lm(I(tot_kg / 1e6) ~ I(solar_kg / 1e9), data = subset_10),
       col = "blue", lwd = 2)

segments(subset_10$solar_kg / 1e9,
         subset_10$tot_kg / 1e6,
         subset_10$solar_kg / 1e9,
         subset_10$fitted / 1e6,
         col = "red")

# Fitted vs Observed
png("fitted_vs_observed_poly.png", width=800, height=600)
plot(observed_values, fitted_values,
     xlab = "Observed log(total hydrogen demand)",
     ylab = "Fitted log(total hydrogen demand)",
     main = "Fitted vs Observed - Log-Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5))
abline(a = 0, b = 1, col = "red", lwd = 2)
dev.off()

# Residuals plot
png("residuals_poly.png", width=800, height=600)
plot(fitted(model_poly), residuals(model_poly),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5))
abline(h = 0, col = "red", lwd = 2)
dev.off()


#Machine Learning - tree based method
names(data)
head(data[, c("name", "state_name", 
              "total_resource_minus_total_demand_kg")])
install.packages("rpart")
install.packages("randomForest")
library(rpart)
library(randomForest)

# Set up features and target
features <- c("mms_refineries_kg", "mms_metals_kg", "mms_ammonia_kg",
              "mms_biofuels_kg", "mms_synfuels_kg", "mms_ngas_kg",
              "mms_ld_fcev_kg", "mms_mhd_fcev_kg", 
              "mms_season_enrgy_strg_kg", "solar_kg", "wind_kg",
              "a_sqkm")

target <- "total_resource_minus_total_demand_kg"

# Prepare data
ml_data <- data[, c(features, target, "name", "state_name")]

# Check for any issues
cat("Rows:", nrow(ml_data), "\n")
cat("Missing values:", sum(is.na(ml_data)), "\n")
#Step 1: Fractional Polynomial Model
library(mfp)
# Fractional Polynomial Model
fp_model <- mfp(
  log_tot_kg ~ 
    fp(log_ammonia) +
    fp(log_refineries) +
    fp(log_metals) +
    fp(log_ld_fcev) +
    fp(log_mhd_fcev) +
    fp(log_seasonal) +
    log_area,
  data = model_data_log,
  family = gaussian,
  verbose = TRUE
)
#Step 1: Fractional
# Model summary
summary(fp_model)

# AIC comparison with previous models
AIC(model_linear_log, model_poly, model_poly_cubic, fp_model)

# Fitted vs observed plot
plot(model_data_log$log_tot_kg, fp_model$fitted.values,
     xlab = "Observed log(tot_kg)",
     ylab = "Fitted log(tot_kg)",
     main = "FP Model: Fitted vs Observed",
     pch = 16, col = rgb(0, 0, 1, 0.4))
abline(0, 1, col = "red", lwd = 2)

# Residual plot
plot(fp_model$fitted.values, fp_model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "FP Model: Residuals vs Fitted",
     pch = 16, col = rgb(0, 0, 1, 0.4))
abline(h = 0, col = "red", lwd = 2)
summary(fp_model)


# Step 1: Decision Tree
set.seed(42)

tree_model <- rpart(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = ml_data,
  method = "anova",
  control = rpart.control(maxdepth = 4, minsplit = 20)
)

# Print summary of tree
printcp(tree_model)

# Plot the tree
png("decision_tree.png", width=1200, height=800)
plot(tree_model, uniform=TRUE, 
     main="Decision Tree - Hydrogen Production Suitability")
text(tree_model, use.n=TRUE, cex=0.7)
dev.off()

cat("Tree saved!\n")

# Step 2: Random Forest
set.seed(42)

rf_model <- randomForest(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = ml_data,
  ntree = 200,
  importance = TRUE
)

# Print summary
print(rf_model)

# Feature importance
importance(rf_model)

# Save feature importance plot
png("feature_importance.png", width=800, height=600)
varImpPlot(rf_model, 
           main="Feature Importance - Random Forest",
           type=1)
dev.off()

# Step 3: Rankings
# Add predictions back to data
ml_data$predicted_surplus <- predict(rf_model, ml_data)

# Top 20 counties
top20_counties <- head(ml_data[order(-ml_data$predicted_surplus), 
                               c("name", "state_name", 
                                 "predicted_surplus")], 20)
print(top20_counties)

# State level rankings - average predicted surplus per state
state_rankings <- aggregate(predicted_surplus ~ state_name, 
                            data = ml_data, FUN = mean)
state_rankings <- state_rankings[order(-state_rankings$predicted_surplus), ]
print(head(state_rankings, 20))

# Top 20 counties plot
png("top20_counties.png", width=900, height=700)
par(mar=c(5,12,4,2))
barplot(rev(top20_counties$predicted_surplus/1e9),
        names.arg=rev(paste(top20_counties$name, 
                            top20_counties$state_name, sep=", ")),
        horiz=TRUE, las=1, cex.names=0.7,
        xlab="Predicted Surplus (billion kg)",
        main="Top 20 Counties by Hydrogen Production Suitability",
        col="#2E86AB")
dev.off()

# Top 20 states plot
top20_states <- head(state_rankings, 20)
png("top20_states.png", width=900, height=700)
par(mar=c(5,10,4,2))
barplot(rev(top20_states$predicted_surplus/1e9),
        names.arg=rev(top20_states$state_name),
        horiz=TRUE, las=1, cex.names=0.8,
        xlab="Mean Predicted Surplus (billion kg)",
        main="Top 20 States by Hydrogen Production Suitability",
        col="#A23B72")
dev.off()

cat("Plots saved!\n")

# Get R squared for random forest
rf_r2 <- 1 - sum((ml_data$total_resource_minus_total_demand_kg - 
                    ml_data$predicted_surplus)^2) /
  sum((ml_data$total_resource_minus_total_demand_kg - 
         mean(ml_data$total_resource_minus_total_demand_kg))^2)

cat("Random Forest R squared:", round(rf_r2, 4), "\n")
cat("Polynomial R squared: 0.7656\n")
cat("Log-Linear R squared: 0.7083\n")

# Train test split 80/20
set.seed(42)
train_idx <- sample(1:nrow(ml_data), 0.8 * nrow(ml_data))
train_data <- ml_data[train_idx, ]
test_data  <- ml_data[-train_idx, ]

# Retrain on training data
rf_model_test <- randomForest(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = train_data,
  ntree = 200,
  importance = TRUE
)

# Test R squared
test_preds <- predict(rf_model_test, test_data)
test_r2 <- 1 - sum((test_data$total_resource_minus_total_demand_kg - 
                      test_preds)^2) /
  sum((test_data$total_resource_minus_total_demand_kg - 
         mean(test_data$total_resource_minus_total_demand_kg))^2)

cat("Test R squared:", round(test_r2, 4), "\n")

poly_subset <- ammonia_subset[1:6, c("mms_ammonia_kg", "tot_kg")]
poly_subset

model_poly_small <- lm(tot_kg ~ mms_ammonia_kg + 
                         I(mms_ammonia_kg^2), 
                       data = poly_subset)
summary(model_poly_small)
residuals(model_poly_small)
data.frame(
  mms_ammonia_kg = poly_subset$mms_ammonia_kg,
  tot_kg = poly_subset$tot_kg,
  fitted = round(fitted(model_poly_small)),
  residual = round(residuals(model_poly_small))
)

library(ggplot2)

library(corrplot)
library(maps)

# ── 1. DISTRIBUTION PLOTS ─────────────────────────────────────
# Response variable
ggplot(model_data, aes(x = tot_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Total Hydrogen Demand",
       x = "Total Hydrogen Demand (kg)", y = "Frequency")
ggsave("hist_tot_kg.png", width = 8, height = 5)

# Log-transformed response
ggplot(model_data_log, aes(x = log_tot_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of log(Total Hydrogen Demand)",
       x = "log(Total Hydrogen Demand)", y = "Frequency")
ggsave("hist_log_tot_kg.png", width = 8, height = 5)

# All predictors on log scale
predictors_log <- c("log_ammonia", "log_refineries", "log_metals",
                    "log_ld_fcev", "log_mhd_fcev", "log_seasonal",
                    "log_area")

for (var in predictors_log) {
  p <- ggplot(model_data_log, aes_string(x = var)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", var), 
         x = var, y = "Frequency")
  ggsave(paste0("hist_", var, ".png"), p, width = 8, height = 5)
}

# ── 2. ZERO INFLATION ANALYSIS ────────────────────────────────
zero_props <- colMeans(model_data == 0) * 100
zero_df <- data.frame(
  variable = names(zero_props),
  pct_zero = zero_props
)
zero_df <- zero_df[zero_df$pct_zero > 0, ]

ggplot(zero_df, aes(x = reorder(variable, pct_zero), y = pct_zero)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage of Zero Values by Variable",
       x = "Variable", y = "Percentage of Zeros (%)") +
  geom_text(aes(label = paste0(round(pct_zero, 1), "%")), 
            hjust = -0.1, size = 3.5)
ggsave("zero_inflation.png", width = 8, height = 5)

# ── 3. CORRELATION ANALYSIS ───────────────────────────────────
cor_matrix <- cor(model_data_log[, c("log_tot_kg",
                                     predictors_log)],
                  use = "complete.obs")

png("correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         title = "Correlation Matrix of Log-Transformed Variables",
         mar = c(0, 0, 2, 0))
dev.off()

# ── 4. SCATTER PLOTS OF KEY RELATIONSHIPS ─────────────────────
# Plot each predictor against response
for (var in predictors_log) {
  p <- ggplot(model_data_log, aes_string(x = var, y = "log_tot_kg")) +
    geom_point(alpha = 0.2, color = "steelblue", size = 0.8) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste("log_tot_kg vs", var),
         x = var, y = "log(Total Hydrogen Demand)")
  ggsave(paste0("scatter_", var, ".png"), p, width = 8, height = 5)
}

# ── 5. SPATIAL ANALYSIS ───────────────────────────────────────
# Merge with county map data
county_map <- map_data("county")

# You need fips codes to merge properly
# Add state and county name columns to model_data first
spatial_data <- data.frame(
  fips     = data$fips,
  tot_kg   = data$tot_kg,
  state    = tolower(data$state_name),
  county   = tolower(data$name)
)

us_map <- map_data("state")

# State-level aggregation
state_data <- aggregate(tot_kg ~ state, 
                        data = spatial_data, 
                        FUN = mean)

map_merged <- merge(us_map, state_data, 
                    by.x = "region", 
                    by.y = "state")
map_merged <- map_merged[order(map_merged$order), ]

ggplot(map_merged, aes(x = long, y = lat, 
                       group = group, fill = tot_kg / 1e6)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      name = "Mean Demand\n(millions kg)") +
  coord_fixed(1.3) +
  labs(title = "Mean Total Hydrogen Demand by State",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("spatial_map.png", width = 10, height = 6)

# ── ZERO INFLATION BAR CHART ──────────────────────────────────
zero_props <- colMeans(model_data == 0) * 100
zero_df <- data.frame(
  variable = names(zero_props),
  pct_zero = zero_props
)
zero_df <- zero_df[zero_df$pct_zero > 0, ]

ggplot(zero_df, aes(x = reorder(variable, pct_zero), y = pct_zero)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage of Zero Values by Variable",
       x = "Variable", y = "Percentage of Zeros (%)") +
  geom_text(aes(label = paste0(round(pct_zero, 1), "%")),
            hjust = -0.1, size = 3.5) +
  ylim(0, 105)
ggsave("zero_inflation.png", width = 8, height = 5)

# ── CORRELATION MATRIX ────────────────────────────────────────
cor_vars <- model_data_log[, c("log_tot_kg", "log_ammonia",
                               "log_refineries", "log_metals",
                               "log_ld_fcev", "log_mhd_fcev",
                               "log_seasonal", "log_area")]
cor_matrix <- cor(cor_vars, use = "complete.obs")

png("correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         mar = c(0, 0, 2, 0))
dev.off()

# ── SCATTER PLOTS (key relationships only) ────────────────────
key_vars <- c("log_ld_fcev", "log_mhd_fcev", 
              "log_seasonal", "log_area")

for (var in key_vars) {
  p <- ggplot(model_data_log, aes_string(x = var, 
                                         y = "log_tot_kg")) +
    geom_point(alpha = 0.2, color = "steelblue", size = 0.8) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste("log_tot_kg vs", var),
         x = var, y = "log(Total Hydrogen Demand)")
  ggsave(paste0("scatter_", var, ".png"), p, 
         width = 8, height = 5)
}

# ── SPATIAL MAP ───────────────────────────────────────────────
spatial_data <- data.frame(
  state  = tolower(data$state_name),
  tot_kg = data$tot_kg
)

state_data <- aggregate(tot_kg ~ state, 
                        data = spatial_data, 
                        FUN = mean)

us_map <- map_data("state")
map_merged <- merge(us_map, state_data,
                    by.x = "region",
                    by.y = "state")
map_merged <- map_merged[order(map_merged$order), ]

ggplot(map_merged, aes(x = long, y = lat,
                       group = group, 
                       fill = tot_kg / 1e6)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "lightyellow", 
                      high = "darkred",
                      name = "Mean Demand\n(millions kg)") +
  coord_fixed(1.3) +
  labs(title = "Mean Total Hydrogen Demand by State",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank())
ggsave("spatial_map.png", width = 10, height = 6)