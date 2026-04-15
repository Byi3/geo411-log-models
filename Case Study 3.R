#MTCARS REGRESSION ANALYSIS
#Analyzes the relationship between car weight and fuel efficiency

#Built-in dataset. Load the built-in mtcars dataset (Motor Trend Car Road Tests, 1974)
data(mtcars)

#Quick look
head(mtcars)

#Create a scatterplot to visually inspect the relationship
#Between car weight (wt) and miles per gallon (mpg)
plot(mtcars$wt, mtcars$mpg,
     main = "Fuel Efficiency vs car weight",
     xlab = "Weight (1000lbs)",
     ylab = "Miles per Gallon (MPG)")

#Fit a simple linear regression: mpg predicted by weight
m1 <- lm(mpg ~ wt, data = mtcars) #mpg ~ wt reads as: "mpg is modeled by wt" i.e.,y is modeled by x
summary(m1)

#Plot fitted values vs residuals to check model assumptions:
# - Random scatter around 0 = good fit
# - Patterns or curves = model may be a missing something
plot(m1$fitted.values, resid(m1),
     main = "Residuals: Linear Model",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = 'red')

#Apply a log transformation to mpg to address non-linearity or heteroscedasticity (unequal spread)
m2 <- lm(log(mpg) ~ wt, data = mtcars)
summary(m2)

#Check if the log transformation improved residual behavior
plot(m2$fitted.values, resid(m2),
     main = "Residuals: Log Model",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = 'red')

#Returns the intercept and slope (B0, B1) from the log model
#Interpretation: At 0%, residuals are at 3.8319136. For every 1% increase in weight, there is a -0.2717847% decrease in MPG.
coef(m2)

#R^2 for the linear model (predicting raw mpg) and transformed (predicting log-transformed mpg)
summary(m1)$r.squared
summary(m2)$r.squared

