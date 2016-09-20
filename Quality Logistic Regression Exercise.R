# Load up the data
quality <- read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
# Load up caTools package
library("caTools")
set.seed(88) # To standardize the number at which random number generator starts
split <- sample.split(quality$PoorCare, SplitRatio = 0.75) 
# ^This randomly splits the data at 3/4 in order to create test and training data. So cool.
qualityTrain <- subset(quality, split == T)
qualityTest <- subset(quality, split == F)
nrow(qualityTrain)
nrow(qualityTest)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial) # binomial indicates log reg model
summary(QualityLog)
predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
table(qualityTrain$PoorCare, predictTrain > 0.5)
# ROC curves to determine where you should put a cutoff of the threshold value (last value in table^)
# install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = T) # Colors thresholds
plot(ROCRperf, colorize = T, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
# Cool thing is that the area under the ROC curve is the probability of predicting the true positive and etc. Cool stuff.