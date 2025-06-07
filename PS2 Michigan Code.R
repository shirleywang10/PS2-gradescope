# Step 1: Install and load required packages
install.packages("readxl")     # for reading Excel files
install.packages("stargazer")  # for formatted regression output
install.packages("sandwich")   # for robust standard errors
install.packages("lmtest")     # for hypothesis testing with robust SEs

library(readxl)
library(stargazer)
library(sandwich)
library(lmtest)

# Step 2: Set working directory (adjust to where your file is located)
setwd("~/Desktop")

# Step 3: Load the data
data <- read_excel("michigan_scores.xlsx")  # adjust filename if needed

# Step 4: Run the regression
model <- lm(math4 ~ log(exppp) + log(enroll), data = data)

# Step 5: Display robust standard errors using coeftest
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# Step 6: Output regression table using stargazer with robust SEs
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
stargazer(model, type = "text", title = "Regression Results with Robust SEs",
          se = list(robust_se), align = TRUE, digits = 3)


# Load required libraries
library(sandwich)
library(lmtest)
library(stargazer)

# Original model
model1 <- lm(math4 ~ log(exppp) + log(enroll), data = data)

# New model with lunch added
model2 <- lm(math4 ~ log(exppp) + log(enroll) + lunch, data = data)

# Compute robust standard errors
robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC1")))
robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC1")))

# Compare both models with robust SEs using stargazer
stargazer(model1, model2,
          se = list(robust_se1, robust_se2),
          type = "text",
          title = "Math Score Regressions with Robust Standard Errors",
          column.labels = c("Model 1", "Model 2"),
          dep.var.labels = "math4",
          align = TRUE, digits = 3)


# Load required libraries
library(readxl)
library(lmtest)
library(sandwich)
library(stargazer)

# Set working directory and load data
setwd("~/Desktop")
data <- read_excel("michigan_scores.xlsx")

# Model 1: Basic
model1 <- lm(math4 ~ log(exppp) + log(enroll), data = data)

# Model 2: Add lunch
model2 <- lm(math4 ~ log(exppp) + log(enroll) + lunch, data = data)

# Model 3: Add read4
model3 <- lm(math4 ~ log(exppp) + log(enroll) + lunch + read4, data = data)

# Robust standard errors
robust_se1 <- sqrt(diag(vcovHC(model1, type = "HC1")))
robust_se2 <- sqrt(diag(vcovHC(model2, type = "HC1")))
robust_se3 <- sqrt(diag(vcovHC(model3, type = "HC1")))

# Stargazer output with robust SEs
stargazer(model1, model2, model3,
          se = list(robust_se1, robust_se2, robust_se3),
          type = "text",
          title = "Math Score Regressions with Robust Standard Errors",
          column.labels = c("Model 1", "Model 2", "Model 3"),
          dep.var.labels = "math4",
          align = TRUE, digits = 3)

# Plot
plot(lunch_used, residuals_model2,
     main = "Residuals vs. Lunch (Model 2)",
     xlab = "Percentage Eligible for Lunch",
     ylab = "Residuals from Model 2",
     pch = 19, col = "blue")

# Add horizontal line at 0
abline(h = 0, col = "red", lwd = 2)
 



