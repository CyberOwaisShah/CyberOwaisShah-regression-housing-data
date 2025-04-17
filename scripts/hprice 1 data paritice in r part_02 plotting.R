# plots using ggplot2 & plotly & broom
# load libraries
library(ggplot2)
library(plotly)
# install.packesges("broom")
library(broom)

# # Load libraries
# library(ggplot2)
# library(plotly)
# library(broom)

# OLS model
model_ols <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)

# FGLS model
uhat2 <- residuals(model_ols)^2
auxiliary_model <- lm(uhat2 ~ lotsize + sqrft + bdrms, data = hprice1)
hhat <- fitted(auxiliary_model)
hhat[hhat <= 0] <- min(hhat[hhat > 0])
weights_fgls <- 1 / hhat
model_fgls <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1, weights = weights_fgls)

# Combine fitted values
hprice1$fitted_ols <- fitted(model_ols)
hprice1$fitted_fgls <- fitted(model_fgls)

# Plot: Actual vs Fitted for OLS
p1 <- ggplot(hprice1, aes(x = price, y = fitted_ols)) +
  geom_point(color = "red", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "OLS: Actual vs Fitted Prices",
       x = "Actual Price",
       y = "Fitted Price") +
  theme_minimal()

# Plot: Actual vs Fitted for FGLS
p2 <- ggplot(hprice1, aes(x = price, y = fitted_fgls)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "FGLS: Actual vs Fitted Prices",
       x = "Actual Price",
       y = "Fitted Price") +
  theme_minimal()

# Make both plots interactive
p1_interactive <- ggplotly(p1)
p2_interactive <- ggplotly(p2)

# Show plots
p1_interactive
p2_interactive


htmlwidgets::saveWidget(p1_interactive, "OLS_plot.html")
htmlwidgets::saveWidget(p2_interactive, "FGLS_plot.html")
