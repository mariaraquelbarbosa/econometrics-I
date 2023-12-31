summary(crime)
apply(crime,2,sd)

# Summary descriptive statistics for the variables
summary(crime$crimes) # get summary descriptive statistics for variable crimes
sd(crime$crimes) # calculate standard deviation for variable crimes
summary(crime$officers) # get summary descriptive statistics for variable officers
sd(crime$officers) # calculate standard deviation for variable officers

# Generate a scatter plot
plot(crime$crimes ~ crime$officers , main = "Relationship between number of police officers and crime")

# Calculate the Covariance and Correlation Coefficient
cov(crime$officers,crime$crimes) #covariance
cor(crime$officers,crime$crimes) #correlation

# Regress the number of police officers on crimes
officersfit <- lm(crime$officers~crime$crimes,data=crime)
officersfit

# Goodness of fits of the estimated model
summary(officersfit)

# Add a regression line to the scatter plot
plot(crime$officers~crime$crimes, main = "Relationship between number of police officers and crime")
abline(officersfit)

