summary(crime)

officersfit <- lm(officers~crimes, data=crime)
officersfit

summary(officersfit)
# Very small value (smaller than significance level) --> reject NULL --> crimes is statistically significant, so we need to include it in our model

crime$officershat <- predict(officersfit, newdata = data.frame(crimes=crime$crimes))
# The difference between the predicted value and the actual value is residual

crime$residuals <- crime$officers - crime$officershat

hist(crime$residuals, main = "Histogram of model residuals", xlab = "Residuals")

mean(crime$residuals)
# The mean residual is around zero --> great model

officersfit2 <- lm(officers~crimes+popdens,data=crime)
summary(officersfit2)

# Getting rid of scientific notation
options(scipen=0) # Set scipen = 0 to get back to default
summary(officersfit2)

# After adding more variables --> R squared will increase

library(haven)
ceosal <- read_dta("ceosal.dta")
View(ceosal)

summary(ceosal$salary)
summary(ceosal$sales)
summary(ceosal$mktval)

salaryfit <- lm(salary~sales, data=ceosal)
salaryfit

ceosal$salary000 <- ceosal$salary / 1000
