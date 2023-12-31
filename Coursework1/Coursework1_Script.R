library(readxl)

# Reading file
X2011lifeexpectancy <- read_xls("C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework1/2011lifeexpectancy.xls")

# Task 1. Generate a descriptive statistics table to summarize your sample.
summary(X2011lifeexpectancy[, 3:7])
apply(X2011lifeexpectancy[, 3:7],2,sd)
prop.table(table(X2011lifeexpectancy$Status))

# Task 2. Generate histograms for Life Expectancy, Alcohol and Schooling.
par(mfrow = c(1, 3))
hist(X2011lifeexpectancy$Life_Expectancy, 
     main = "Histogram of Life Expectancy",
     xlab = "Life expectancy of each country (in age)",
     col = "mistyrose")
hist(X2011lifeexpectancy$Alcohol,
     main = "Histogram of Alcohol consumption",
     xlab = "Alcohol consumption (in litres of pure alcohol)",
     col = "lightyellow")
hist(X2011lifeexpectancy$Schooling,
     main = "Histogram of  Number of years of Schooling",
     xlab = "Number of years of schooling",
     col = "lightgreen")
par(mfrow = c(1, 1))

# Task 3. Plot the relationship between Life Expectancy and Alcohol as well as Life Expectancy and Schooling
par(mfrow = c(1, 2))

alcoholfit <- lm(X2011lifeexpectancy$Life_Expectancy ~ X2011lifeexpectancy$Alcohol, data=X2011lifeexpectancy)
schoolingfit <- lm(X2011lifeexpectancy$Life_Expectancy ~ X2011lifeexpectancy$Schooling, data=X2011lifeexpectancy)

plot(X2011lifeexpectancy$Life_Expectancy ~ X2011lifeexpectancy$Alcohol,
     main = "Relationship between Life Expectancy and Alcohol",
     ylab = "Life expectancy (in age)",
     xlab = "Alcohol consumption (in litres)")
abline(alcoholfit)

plot(X2011lifeexpectancy$Life_Expectancy ~ X2011lifeexpectancy$Schooling,
     main = "Relationship between Life Expectancy and Schooling",
     ylab = "Life expectancy (in age)",
     xlab = "Number of years of schooling")
abline(schoolingfit)

par(mfrow = c(1, 1))

# Task 4. Calculate the correlation coefficient between (i) Life Expectancy and Alcohol, and (ii) Life Expectancy and Schooling.
cor(X2011lifeexpectancy$Alcohol, X2011lifeexpectancy$Life_Expectancy)
cor(X2011lifeexpectancy$Schooling, X2011lifeexpectancy$Life_Expectancy)

# Extra: Boxplot to analyse outliers
par(mfrow = c(1, 3))
boxplot(X2011lifeexpectancy$Life_Expectancy, main = "Life Expectancy", col = "mistyrose")
boxplot(X2011lifeexpectancy$Alcohol, main = "Alcohol", col = "lightyellow")
boxplot(X2011lifeexpectancy$Schooling, main = "Schooling", col = "lightgreen")
par(mfrow = c(1, 1))