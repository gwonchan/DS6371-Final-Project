#Analysis 1
library(ggplot2)

options(repr.plot.width = 12, repr.plot.height = 8)

Train = read.csv("train.csv", header = T)

Test = read.csv("test.csv", header = T)

filtered_data <- Train[Train$Neighborhood %in% c("NAmes", "Edwards", "BrkSide"), ]

head(filtered_data)


# Make sure to replace the column names with the actual names in your dataset
model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars, data = filtered_data)

# Summary of the linear regression model
summary(model)

# Load necessary packages
library(ggplot2)
library(car)

# Assuming you have already fit the linear regression model (replace 'model' with your actual model)
# Example: model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars, data = Train)

# Residual plots
par(mfrow=c(2,2))
plot(model, pch = 16)
# Assuming you have already fit the linear regression model (replace 'model' with your actual model)
# Example: model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars, data = Train)

# Calculate Cook's Distance directly on the model
cooksd <- cooks.distance(model)

# Plot Cook's Distance
plot(cooksd, pch = 16, main = "Cook's Distance Plot", ylab = "Cook's Distance", xlab = "Observation")
abline(h = 4/(length(cooksd) - length(coefficients(model))), col = "red", lty = 2)

# Identify influential points based on Cook's D
influential_points <- which(cooksd > 4/(length(cooksd) - length(coefficients(model))))
cat("Influential Points (based on Cook's D):", influential_points, "\n")

# Plot Leverage vs. Residuals squared
residuals_squared <- residuals(model)^2
plot(hatvalues(model), residuals_squared, pch = 16, main = "Leverage vs. Residuals Squared", 
     ylab = "Residuals Squared", xlab = "Leverage")

# Identify high leverage points
high_leverage_points <- which(hatvalues(model) > 2 * mean(hatvalues(model)))
cat("High Leverage Points:", high_leverage_points, "\n")
