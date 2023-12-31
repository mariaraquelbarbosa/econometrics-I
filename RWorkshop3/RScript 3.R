setwd("C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/RWorkshop3")

# Task 1. Open the data set in R.
summary(crime) # get summary descriptive statistics for dataset crime

# Task 2. Run a simple regression model of officers on crimes and
# discuss the statistical significance of the intercept, the coefficient
# of crimes and of R-squared (R2).
officersfit <- lm(crime$officers~crime$crimes,data=crime)
officersfit

# Task 3. Using the regression results from above, check the model’s
# prediction for the number of police officers based on the number of
# crimes committed within a city.
crime$prediction <- predict(officersfit, newdata=data.frame(crimes=crime$crimes))

# Task 4. Calculate the residuals uˆi for each observation to identify
# how far off the predicted values are away from the actual values.
crime$residuals <- crime$officers-crime$prediction

# Task 5. Construct the histogram of the residuals.
hist(crime$residuals, main = "Histogram of model residuals",
     xlab = "Residuals")

# Task 6. Calculate the arithmetic mean of the residuals.
mean(crime$residuals)

# Task 7. Add popdens to the regression model, run the model and
# comment on the regression results. Identify differences between the
# results of the simple regression model and the multiple regression
# model:
officersfit2 <- lm(officers~crimes+popdens,data=crime)
summary(officersfit2)

options(scipen=4) # Set scipen = 0 to get back to default
summary(officersfit2)

# Task 8. Comment on the R-squared (R2) values across both the
# simple regression model and the multiple regression model.

# The R-squared value is higher for the multiple regression model than for the simple regression model. Adding
# more variables to a model will always increase the explanatory power of a model.
# The R-squared for the multiple regression model reveals that 86 % of the variation in officers can be explained
# by the variations in the independent variables. The explanatory power of the estimated model is high.
