## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("linear_regression/dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

## Start by examining the data to check for problems

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships, etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   What would the association between expense and SAT scores be
##   if there were no difference among the states in the percentage of
##   students taking the SAT?
##   

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to

##   1. Examine/plot the data before fitting the model

# summary of metro and energy columns, all rows
sts.metro.energy <- subset(states.data, select = c("metro", "energy"))
summary(sts.metro.energy)
# correlation between metro and energy, na values removed
sts.metro.energy <- subset(na.omit(states.data), select = c("metro", "energy"))
cor(sts.metro.energy)
# scatter plot of metro vs energy
plot(sts.metro.energy)

##   2. Print and interpret the model `summary'

# Fit the regression model
energy.mod <- lm(energy ~ metro, # regression formula
              data=na.omit(states.data)) # data set
# Summarize and print the results
summary(energy.mod) # show regression coefficients table
# This model shows that the percentage of residents living in 
# metropolitan areas is a significant predictor of energy consumption
# per capita at a 5% significance level. However, the R-squared and
# adjusted R-squared values are only 0.09714 and 0.07751 respectively,
# which indicates that this single factor is not a strong predictor.

##   3. `plot' the model to look for deviations from modeling assumptions

# Plot the regression model
plot(energy.mod, which = c(1, 2))
# The standardized residuals appear to have a linear distribution with
# a reasonable homogeneity of variance.

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# Reexamine the data labels
states.info

# Fit the new regression model with solid waste and greenhouse gas added 
energy.mod.new <- lm(energy ~ metro + waste + green, # regression formula
                 data=na.omit(states.data)) # data set

# Summarize and print the results
summary(energy.mod.new) # show regression coefficients table
# This new model shows that out of metro area population, per capita
# solid waste, and per capita greenhouse gas, only the greenhouse gas
# factor is significant, and it is highly significant. Multiple R-squared
# is 0.5962 and adjusted R-squared is 0.5687, which indicates that this
# model is a much better predictor than the original model.

# Plot the new regression model
plot(energy.mod.new, which = c(1, 2))
# The standardized residuals again appear to have a linear distribution with
# a reasonable homogeneity of variance.

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us to assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

#Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

# Adjust the regression equation to include the interaction of metro
# area population and per capita greenhouse gas
energy.mod.int <- lm(energy ~ metro*green, # regression formula
                     data=na.omit(states.data)) # data set
#Show the results
coef(summary(energy.mod.int)) # show regression coefficients table

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

#Add region to the model
energy.region <- lm(energy ~ region, #regression formula
                 data=states.data) # data set
#Show the results
coef(summary(energy.region)) # show regression coefficients table
anova(energy.region) # show ANOVA table

# The coefficient summary does seem to indicate significant differences
# across regions. The ANOVA table indicates that region is significant
# at the 10% probability level.
