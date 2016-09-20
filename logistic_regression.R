## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("logistic_regression/dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
               data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

# Get rid of NA values
str(NH11$r_maritl) 
levels(NH11$r_maritl) 
# check for missing values
sapply(NH11[,c(11,12,13)], function(x) sum(is.na(x))) # None for age and marriage, a lot for work.

# Need to make binary - married or not married? Married is anything other than 1,2,3, and 8
MarriageTest <- model.matrix(~ r_maritl - 1, data = NH11)
# Logic matrix that indicates 1 where true. Remove all rows that are not marriage then make one column
Marriage <- MarriageTest[,2] + MarriageTest[,3] + MarriageTest[,4] + MarriageTest[,9]
NH11$r_maritl <- ifelse(Marriage == 1, "1 Married", "2 Not Married")
# Assumption is that living with a partner is equivalent to being married, could go more in-depth here to find a difference...
# Also assuming unknown marital status is not married

# collapse all missing values of everwrk to NA... Then remove NAs. Create matrix of shortened factors.
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))
# find rows with NA by using complete.cases() function
NONnas <- complete.cases(NH11$everwrk)
LogRegMat <- NH11[NONnas == T, c(11,12,13)] # Using this as y for regression model

# run our regression model
PredictionEverwrk <- glm(everwrk ~ age_p + r_maritl,
               data=LogRegMat, family="binomial")
coef(summary(PredictionEverwrk)) #Prints a list of probs for each age and if married or not

# Create a dataset with predictors set at desired levels
predMarried <- with(LogRegMat,
                expand.grid(r_maritl = c("1 Married","2 Not Married"), # All marriage values, now
                age_p = c(18, 20, 30, 40, 50, 60))) # Nice range of age values

##  Predict the probability of working for each level of marital status
Predictions <- cbind(predMarried, predict(PredictionEverwrk, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predMarried))
plot(Predictions) # Apparent that older people are more employed. And possibly that married are less employed.