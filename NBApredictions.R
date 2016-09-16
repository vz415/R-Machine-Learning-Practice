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
summary(WinsReg)
# Points scored and wins
PointsReg <- lm(PTS~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA_train)
summary(PointsReg)
SSE <- sum(PointsReg$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(NBA_train))
RMSE
# Regression without turnovers
PointsReg2 <- lm(PTS~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA_train)
summary(PointsReg2)
# Regression without DRB
PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA_train)
summary(PointsReg3)
# Regression without blocks
PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA_train)
summary(PointsReg4)
SSE4 <- sum(PointsReg4$residuals^2)
SSE4
RMSE4 <- sqrt(SSE4/nrow(NBA_train))
RMSE4

# Predictions of the 12-13 season, using the NBA_test data
PointsPrediction <- predict(PointsReg4, newdata = NBA_test)
SSE_test <- sum((PointsPrediction - NBA_test$PTS)^2)
SST_test <- sum((mean(NBA_train$PTS) - NBA_test$PTS)^2)
R2 <- 1 - SSE/SST
R2
RMSE_test <- sqrt(SSE/nrow(NBA_test))
RMSE_test
