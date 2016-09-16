# NBA Predictions
NBA_train <- read.csv("NBA_train.csv")
NBA_test <- read.csv("NBA_test.csv")
# explore
str(NBA_train)
summary(NBA_train)
# Tables for wins and playoffs
table(NBA_train$W, NBA_train$Playoffs)
NBA_train$PTSdiff <- NBA_train$PTS - NBA_train$oppPTS
plot(NBA_train$PTSdiff, NBA_train$W)
WinsReg <- lm(W~PTSdiff, data = NBA_train)
