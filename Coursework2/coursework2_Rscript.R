# Preparing workspace
setwd("C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2")
install.packages("xtable")

# Loading libraries
library(readxl)
library(stargazer)
library(xtable)

# Importing the dataset
data_2011 <- read_excel("2011lifeexpectancy.xls")


# Model 1.	Simple Linear Regression Model
# Regressing life expectancy on alcohol consumption
model1 <- lm(data_2011$Life_Expectancy~data_2011$Alcohol,data=data_2011)
model1

# Goodness of fits of the estimated model
summary(model1)

# Formatting the table of results
stargazer(model1, 
          type = "html", 
          title="Table 1: Simple Linear Model Results", 
          summary = TRUE,
          align=TRUE,
          no.space=TRUE,
          out = "C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2/Model1.htm",
          report=("vcstp"))

# Creating a scatter plot with a regression line
plot(data_2011$Life_Expectancy~data_2011$Alcohol, 
     main = "Relationship between Life Expectancy and Alcohol",
     xlab = "Life expectancy (in years)",
     ylab = "Alcohol (in litres of pure alcohol)")
abline(model1, col = "red")

# Obtaining ANOVA table
anova_table1 <- anova(model1)

# Creating a table from the ANOVA table
xtable_anova1 <- xtable(anova_table1)

# Saving it to a file
print(xtable_anova1, type = "latex")


# Model 2.	Multiple linear regression model I
# Adding Schooling and BMI to the regression model
model2 <- lm(Life_Expectancy~Alcohol+Schooling+BMI,data=data_2011)
summary(model2)

# Formatting the table of results
stargazer(model2, 
          type = "html", 
          title="Table 2: Multiple Linear Regression Model I Results", 
          summary = TRUE,
          align=TRUE,
          no.space=TRUE,
          report=("vcstp"),
          out = "C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2/Model2.htm")

# Obtaining ANOVA table
anova_table2 <- anova(model2)

# Creating a table from the ANOVA table
xtable_anova2 <- xtable(anova_table2)

# Saving it to a file
print(xtable_anova2, type = "latex")

# Model 3.	Multiple linear regression model II
# Including the quadratic term of the Schooling variable
model3 <- lm(Life_Expectancy ~ Alcohol + Schooling + I(Schooling^2) + BMI, data = data_2011)
summary(model3)

# Formatting the table of results
stargazer(model3, 
          type = "html", 
          title="Table 3: Multiple Linear Regression Model II Results", 
          summary = TRUE,
          align=TRUE,
          no.space=TRUE,
          report=("vcstp"),
          out = "C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2/Model3.htm")

# Model 4. Logarithmic Model
# Making the log transformation
data_2011$lnLife_Expectancy <- log(data_2011$Life_Expectancy)
data_2011$lnSchooling <- log(data_2011$Schooling)

# Building the new model
model4 <- lm(lnLife_Expectancy ~ Alcohol + lnSchooling + BMI, data = data_2011)
summary(model4)

# Formatting the table of results
stargazer(model4, 
          type = "html", 
          title="Table 4: Logarithmic Model Results", 
          summary = TRUE,
          align=TRUE,
          no.space=TRUE,
          report=("vcstp"),
          out = "C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2/Model4.htm")

# Comparing all models
stargazer(model1, model2, model3, model4, 
          type = "html", 
          title="Table 5: Comparison of Models Results", 
          summary = TRUE,
          align=TRUE,
          no.space=TRUE,
          report=("vcstp"),
          out = "C:/Users/maria/OneDrive/Documentos/3. Econometrics 1/BS2280/Coursework2/All_Models.htm")

# Predicting life expectancy data with each model
data_2011$prediction1 <- predict(model1, newdata=data.frame(Alcohol=data_2011$Alcohol))
data_2011$prediction2 <- predict(model2, newdata=data.frame(Alcohol = data_2011$Alcohol,
                                                            Schooling = data_2011$Schooling,
                                                            BMI = data_2011$BMI))
data_2011$prediction3 <- predict(model3, newdata=data.frame(Alcohol = data_2011$Alcohol,
                                                            Schooling = data_2011$Schooling,
                                                            Schooling_squared = data_2011$Schooling^2,
                                                            BMI = data_2011$BMI))
data_2011$prediction4 <- predict(model4, newdata=data.frame(Alcohol = data_2011$Alcohol,
                                                            lnSchooling = data_2011$lnSchooling,
                                                            BMI = data_2011$BMI))

# Calculating the residuals
data_2011$residuals1 <- data_2011$Life_Expectancy-data_2011$prediction1
data_2011$residuals2 <- data_2011$Life_Expectancy-data_2011$prediction2
data_2011$residuals3 <- data_2011$Life_Expectancy-data_2011$prediction3
data_2011$residuals4 <- data_2011$lnLife_Expectancy-data_2011$prediction4

# Plotting the histograms of residuals
par(mfrow = c(2, 2))
hist(data_2011$residuals1, main = "Histogram of Model 1 Residuals",
     xlab = "Residuals", col = "#A6CEE3")
hist(data_2011$residuals2, main = "Histogram of Model 2 Residuals",
     xlab = "Residuals", col = "#B2DF8A")
hist(data_2011$residuals3, main = "Histogram of Model 3 Residuals",
     xlab = "Residuals", col = "#FFFF99")
hist(data_2011$residuals4, main = "Histogram of Model 4 Residuals",
     xlab = "Residuals", col = "#FF9A98")
par(mfrow = c(1, 1))

# Finding the t-critical value
alpha <- 0.05
df <- 169
t_critical <- qt(1 - alpha/2, df)
print(t_critical)

# Finding f-critical value
df1 <- 2
df2 <- 167
significance_level <- 0.05
f_critical <- qf(1 - significance_level, df1, df2)
print(f_critical)