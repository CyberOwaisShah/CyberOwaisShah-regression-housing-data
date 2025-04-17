# Step 1: Install and load necessary packages----
install.packages(c("haven", "lmtest", "sandwich", "car"))
library(haven)      # for reading .dta files
library(lmtest)     # for Breusch-Pagan test
library(sandwich)   # for robust standard errors
library(car)        # for White test

# Step 2: Load your hprice1.dta data
data <- read_dta("path/to/hprice1.dta")  # Change "path/to/" to your correct folder
attach(data)
# Step 3: Run baseline OLS regression
model <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
summary(model)
# Step 4: Graphical Analysis of Heteroscedasticity
residuals_model <- resid(model)
fitted_model <- fitted(model)

plot(fitted_model, residuals_model,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# Step 5: Heteroscedasticity Tests

# 5.1 Breusch-Pagan Test
bptest(model)

# 5.2 White Test (Full Interaction Version)
bptest(model, ~ lotsize + sqrft + bdrms + I(lotsize^2) + I(sqrft^2) + I(bdrms^2) +
         I(lotsize*sqrft) + I(lotsize*bdrms) + I(sqrft*bdrms), data = hprice1)

# 5.3 Alternative White Test (Manually create squared terms)
data$lotsize_sq <- data$lotsize^2
data$sqrft_sq <- data$sqrft^2
data$bdrms_sq <- data$bdrms^2

model_alt <- lm(price ~ lotsize + sqrft + bdrms + lotsize_sq + sqrft_sq + bdrms_sq, data =hprice1)
bptest(model_alt)

# Step 6: Robust Standard Errors
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# Step 7: Weighted Least Squares (WLS)

# Create WLS weights
weights_wls <- 1 / (residuals_model^2)

# WLS regression
model_wls <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1, weights = weights_wls)
summary(model_wls)

# Step 8: Feasible Generalized Least Squares (FGLS)

# Auxiliary regression: uhat^2 on regressors
uhat2 <- residuals_model^2
auxiliary_model <- lm(uhat2 ~ lotsize + sqrft + bdrms, data =hprice1)

# Predicted variances (hhat)
hhat <- fitted(auxiliary_model)

# Create FGLS weights
weights_fgls <- 1 / hhat

# FGLS regression
model_fgls <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1, weights = weights_fgls)
summary(model_fgls)

detach(data)





# Auxiliary regression: square residuals on regressors----
uhat2 <- residuals(model)^2
auxiliary_model <- lm(uhat2 ~ lotsize + sqrft + bdrms, data =hprice1)

# Predicted variances (hhat)
hhat <- fitted(auxiliary_model)

# Very important: Replace any tiny/negative/zero hhat values
hhat[hhat <= 0] <- min(hhat[hhat > 0])  # Replace non-positive values with smallest positive one

# FGLS regression
weights_fgls <- 1 / hhat
model_fgls <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1, weights = weights_fgls)
summary(model_fgls)


# Step 9: Compare Models
# Compare the coefficients of the original OLS model and the WLS model
summary(model)
summary(model_wls)
summary(model_fgls)



# FIRST estimate OLS model----
model_ols <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)

# THEN estimate FGLS model (which you already did)
uhat2 <- residuals(model_ols)^2
auxiliary_model <- lm(uhat2 ~ lotsize + sqrft + bdrms, data = hprice1)
hhat <- fitted(auxiliary_model)
hhat[hhat <= 0] <- min(hhat[hhat > 0])
weights_fgls <- 1 / hhat

model_fgls <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1, weights = weights_fgls)


# Compare old and final model
par(mfrow = c(1, 2))

# OLS
plot(hprice1$price, fitted(model_ols),
     xlab = "Actual Price",
     ylab = "Fitted Price",
     main = "OLS: Actual vs Fitted",
     pch = 19, col = "red")
abline(0, 1, col = "black", lwd = 2)

# FGLS
plot(hprice1$price, fitted(model_fgls),
     xlab = "Actual Price",
     ylab = "Fitted Price",
     main = "FGLS: Actual vs Fitted",
     pch = 19, col = "blue")
abline(0, 1, col = "black", lwd = 2)

par(mfrow = c(1, 1))




install.packages("stargazer")  # only if not installed
library(stargazer)

# Create comparison table
stargazer(model_ols, model_fgls,
          type = "text",
          title = "Comparison of OLS and FGLS Models",
          align = TRUE,
          column.labels = c("OLS", "FGLS"),
          dep.var.labels = "House Price",
          covariate.labels = c("Lot Size", "Square Footage", "Bedrooms"),
          no.space = TRUE,
          omit.stat = c("f", "ser"), # optional: hide extra statistics
          digits = 3)

