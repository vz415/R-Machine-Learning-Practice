# Election predictions
polling <- read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling) # Check out where data are missing
# To replace missing values, we'll replace with similar values
# install.packages("mice")
library(mice)
simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed) #shows no more missing values!
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)
# No more missing values, apparently
# Create training and testing data. Train will come from 2004 and 2008 data.
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
table(Train$Republican)
# CReate new baseline to fix republican bias by using sign function, returns 1 if pos number, -1 if negative, and 0 for 0
table(sign(Train$Rasmussen))
# Who actually won the state? Specificity and sensitivity calculations
table(Train$Republican, sign(Train$Rasmussen))
# Account for multilinearities
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
mod1 <- glm(Republican~PropR, data = Train, family= "binomial")
summary(mod1)
pred1 <- predict(mod1, type = "response")
table(Train$Republican, pred1 >= 0.5)
# We want to filter out any variables that are complimentary. Two good variables make one bad prediction. Just choose the best one.
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")
pred2 <- predict(mod2, type = "response")
table(Train$Republican, pred2 >= 0.5)
# Made one less mistake than the baseline
summary(mod2)
# Want to look at smart baseline model from Rassmussen polls
table(Test$Republican, sign(Test$Rasmussen))
TestPrediction <- predict(mod2, newdata = Test, type = "response")
table(Test$Republican, TestPrediction >= 0.5)
subset(Test, TestPrediction >= 0.5 & Republican == 0)
# Model didn't predict Obama winning FLA in 2012 just indicates that this model, while nice, isn't conclusive.
