# R Workshop 0
## Using R as a calculator
5+2
5*2
5/2
sqrt(5)

## Save as a workspace image
save.image("MyRsession.Rdata")

## Assigning objects
x <- 5*2
x

X <- 3*2
X

y <- x^2
y

## Installing and Loading R Packages
install.packages("readxl")
library("readxl")
update.packages
?sqrt()

# R Workshop 1
## Import the Excel dataset
crime <- read_excel("crime.xls")
View(crime)

## Label the variables
install.packages("expss")
library("expss")

crime <- apply_labels(crime,
                      pop = "actual population in number",
                      crimes = "total number of crimes",
                      unem = "unemployment rate (%)",
                      officers = "number of police officers",
                      pcinc = "per capita income, $",
                      area = "land area, square miles",
                      lawexpc = "law enforcement expenditure per capita, $")

## Create a new variable
crime$popdens <- crime$pop / crime$area
rank <- order(crime$popdens)
crime.popens1 <- crime[rank,] # order from the smallest to the largest value
crime.popens2 <- crime[order(crime$popdens, decreasing = TRUE),] # order from the largest to the smallest number
View(crime.popens2)
