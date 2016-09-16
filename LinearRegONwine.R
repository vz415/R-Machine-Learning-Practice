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
# To understand which variables help the model predict wine price, look at the summary of the data, and see which variables have a ***, **, *, ., or ' ' by their name. Features that help predict the output will have asterisks
# However, when you eliminate unnecessary features, there is a possibility that correlations come out of nowhere. Check with cor() function.
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
# Because of multicollinearity, only want to remove one feature at a time to make sure you don't completely screw up the model
