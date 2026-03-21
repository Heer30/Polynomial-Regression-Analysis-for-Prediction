data <- read.csv("data.csv", header = TRUE)
head(data)

linear_model <- lm(Hydrogen_Production_kg.day ~ . - Feasibility_Score, data = data)

# Summary of the full model
summary(linear_model)

 
# Stepwise model selection to remove unneeded predictors (AIC-based)
#final_model <- step(linear_model, k = 2)
#summary(final_model)
#Looking at your earlier output:

#Your System_Efficiency_. and Hydrogen_Production_kg.day columns are deterministically related to the other predictors — there’s no noise.

#So linear regression achieves a “perfect” fit, which breaks model selection functions like step().

# Check for assumption violations
#par(mfrow = c(2, 2))  # multiple plots on one window
#plot(final_model)      # residuals, QQ plot, etc.

# Reset plotting layout
#par(mfrow = c(1, 1))


