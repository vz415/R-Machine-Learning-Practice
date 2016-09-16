# Wine data - load it up.
wine <- read.csv("wine.csv")
test_wine <- read.csv("wine_test.csv")
# Get a feel for the data
str(wine)
summary(wine)
# Creating logistic regression model of the wine based off the price and average growing season temperature
model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
SSE <- sum(model1$residuals^2)
SSE
# Another one.
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE2 <- sum(model2$residuals^2)
SSE2
# last one with a bunch of variables.
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE3 <- sum(model3$residuals^2)
SSE3