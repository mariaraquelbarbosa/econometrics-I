# Task 1. Open the data set in R.
wages <- read.csv("wages.csv")

# Task 2. Describe the distribution of wages by plotting a histogram.
hist(wages$wage,
     main = "Histogram of average hourly wages ($)",
     xlab = "Wages")
## The histogram reveals that the variable wage is right skewed

# Task 3. Produce a bar chart that compares the average wages
# for men and women. Compare this chart with the bar chart that
# compared average level of education for men and women.
barplot(mean(wages$wage),
     main = "Bar plot of average hourly wages ($)",
     ylab = "Wages", 
     ylim = c(0,7))

## Before we can produce the mean values for men and women, we have to save them first in a vector. The
## command tapply() allows us to generate statistics for variables of different groups, e.g. gender.
av.wage.gender <- tapply(wages$wage, INDEX = wages$gender, FUN = mean)
av.wage.gender

barplot(av.wage.gender, 
        main = "Average wage difference between men and women",
        ylab = "Wages", 
        ylim = c(0,7))
## There is a big difference between averages wages of men and women.
## We can use regression analysis to get further information on why there is such a big wage gap.

# Task 4. Consider the following model:
# wagei = β1 + β2educi + β3experi + β4tenurei + ui

model1 <- lm(wage~educ+exper+tenure,data=wages)
summary(model1)

## Add the gender dummy variable to the above model and rerun the model
model2 <- lm(wage~educ+exper+tenure+gender,data=wages)
summary(model2)

## The coefficient of gender dummy is 1.81, which is positive and highly 
## statistically significant (p_value < 1%).

## A man with the same years of education, the same work experience, 
## the same number of years of work tenure will earn USD 1.8 more an hour than women.

# Task 5. Show the frequency for how many people work in a specific sector and location. 
# (Note that sector and location are categorical variables with more than two outcomes.)
table(wages$sector)
table(wages$location)

# Task 6. Construct bar charts showing differences in wage in different
# sectors and the differences in wages in different locations.
av.wage.sector <- tapply(wages$wage, 
                         INDEX = wages$sector, 
                         FUN = mean)
barplot(av.wage.sector, 
        main = "Average wage difference in different sector",
        ylab = "Wages", 
        ylim = c(0,7))

av.wage.location <- tapply(wages$wage, 
                           INDEX = wages$location, 
                           FUN = mean)
barplot(av.wage.location, 
        main = "Average wage difference in different location",
        ylab = "Wages", 
        ylim = c(0,7))

# Task 7. Use the regression Model 1 from task 4 and add location
# and sector dummy variables and re-estimate the model. Beware of
# the Dummy Variable trap!
model3 <- lm(wage~educ+exper+tenure+location+sector,data=wages)
summary(model3)

## For variable location, eastern is dropped so eastern is base category/comparison group for all remaining locations.
## For variable sector, construction is dropped so eastern is base category/comparison group for all remaining sectors.
## R drops the first alphabetically sorted category by default.

# Task 8. Change the base category/comparison group for dummy variables.
## The most frequent category of location is “south” and The most frequent category of sector is “service”.
## So we prefer to use “south” and “service” as base category for location and sector dummies, respectively.
class(wages$sector) #check the type of variable sector in data set wages
class(wages$location) #check the type of variable location in data set wages

wages$sectorfactor <- as.factor(wages$sector) #convert character variable sector to a factor variable sectorfactor
wages$locationfactor <- as.factor(wages$location) #convert character variable location to a factor variable locationfactor

levels(wages$sectorfactor) #show the categories of factor variable sectorfactor
levels(wages$locationfactor) #show the categories of factor variable locationfactor

wages$sectorneworder <- relevel(wages$sectorfactor, ref="services") #Change the base category as service and give it to a new variable sectorneworder

wages$locationneworder <- relevel(wages$locationfactor, ref="south") #Change the base category as south and give it to a new variable locationneworder

levels(wages$sectorneworder) #show the categories of new factor variable sectorneworder
levels(wages$locationneworder) #show the categories of new factor variable locationneworder

# Task 9. Run the regression Model 4 with changed base category dummy variables location and sector
model4 <- lm(wage~educ+exper+tenure+locationfactor+sectorneworder,data=wages)
summary(model4)

# Task 10. Use stargazer() to get the regression output
install.packages("stargazer")
library(stargazer)

stargazer(model1, type = "text")
stargazer(model1, model2, model3, type = "text")