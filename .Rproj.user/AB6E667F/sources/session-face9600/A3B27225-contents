# Load necessary packages
install.packages("readxl")  # If not already installed
library(readxl)

# Read the Excel file (adjust the file path as needed)
data <- read_excel("california_scores.xlsx")

# Create the new variable "score" as the average of math and reading scores
data$score <- (data$math + data$read) / 2

# View the first few rows to confirm
head(data)

# Show math, read, and score side by side for the first 6 rows
data[, c("math", "read", "score")]

# Load the writexl package (install it first if needed)
install.packages("writexl")  # Only run once
library(writexl)

# Save the updated data to a new Excel file
write_xlsx(data, "updated_california_scores.xlsx")

# Run the linear regression
model <- lm(score ~ income, data = data)

# Show the regression summary
summary(model)

install.packages("sandwich")
install.packages("lmtest")

library(sandwich)
library(lmtest)

# Re-run the model (if needed)
model <- lm(score ~ income, data = data)

# Step 1: Install (only needed once)
install.packages("stargazer")

# Step 2: Load the package
library(stargazer)

# Display regression table with robust SEs
stargazer(model, type = "text", se = list(robust_se),
          title = "Regression of Score on Income (Robust SE)",
          dep.var.labels = "Score", covariate.labels = "Income",
          digits = 3)

# Base R plot
plot(data$income, data$score,
     xlab = "Income (in $1,000)",
     ylab = "Average Score",
     main = "Score vs. Income with Regression Line",
     pch = 19, col = "blue")

# Add regression line
abline(model, col = "red", lwd = 2)

# Load necessary packages
library(sandwich)
library(lmtest)
library(stargazer)

# Models
model_income <- lm(score ~ income, data = data)
model_log_income <- lm(score ~ log(income), data = data)

# Get robust standard errors
robust_se1 <- sqrt(diag(vcovHC(model_income, type = "HC1")))
robust_se2 <- sqrt(diag(vcovHC(model_log_income, type = "HC1")))

# Stargazer with robust SEs
stargazer(model_income, model_log_income,
          type = "text",
          se = list(robust_se1, robust_se2),
          title = "Regression of Score on Income vs. Log(Income) (Robust SE)",
          column.labels = c("Income", "Log(Income)"),
          dep.var.labels = "Score",
          covariate.labels = c("Income", "Log(Income)"),
          digits = 3)

# Scatter plot
plot(data$income, data$score,
     xlab = "Income (in $1,000)",
     ylab = "Average Score",
     main = "Score vs. Income with Lin-Log Regression Fit",
     pch = 19, col = "lightblue")

# Add the regression line from the lin-log model
# Create a sequence of income values to evaluate predicted scores
income_seq <- seq(min(data$income), max(data$income), length.out = 100)
predicted_scores <- predict(model_log, 
                            newdata = data.frame(log_income = log(income_seq)))

# Add the regression line
lines(income_seq, predicted_scores, col = "red", lwd = 2)




