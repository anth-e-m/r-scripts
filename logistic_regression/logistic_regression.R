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

install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.
##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

str(NH11$everwrk) # check stucture of everwrk
levels(NH11$everwrk) # check levels of everwrk
# collapse all missing values to NA
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

str(NH11$age_p) # check stucture of age_p
# this is a numerical variable, should be ok to use as-is

str(NH11$r_maritl) # check stucture of r_maritl
levels(NH11$r_maritl) # check levels of r_maritl
# collapse all unknown or irrelevant values to NA
NH11$r_maritl <- factor(NH11$r_maritl, levels=c("1 Married - spouse in household", 
                                            "2 Married - spouse not in household",
                                            "4 Widowed", "5 Divorced", "6 Separated",
                                            "7 Never married", "8 Living with partner"))

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

# run the regression model
pred.everwrk <- glm(everwrk~age_p+r_maritl,
               data=NH11, family="binomial")
coef(summary(pred.everwrk))

##   2. Predict the probability of working for each level of marital
##      status.

# predict work probability based on marital status

# create new regression model 
pred.everwrk.mar <- glm(everwrk~r_maritl,
                    data=NH11, family="binomial")
coef(summary(pred.everwrk.mar))

# transform the coefficients for interpretation
pred.everwrk.mar.tab <- coef(summary(pred.everwrk.mar))
pred.everwrk.mar.tab[, "Estimate"] <- exp(coef(pred.everwrk.mar))
pred.everwrk.mar.tab

# plot the data to view effects
plot(allEffects(pred.everwrk.mar))

# Based on the model it appears that divorced people have the highest
# probability of having worked (around 94%) and never married people
# have the lowest probability of having worked (around 75%).
