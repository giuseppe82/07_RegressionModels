# Regression Model. Quiz 3 

# 1. 
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number
# cylinders as a factor variable and weight as cofounder. Give the adjusted estimate for 
# the expected change in mpg comparing 8 cylinders to 4. 

y <- mtcars$mpg
x1 <- factor(mtcars$cyl)
x2 <- mtcars$wt

fit <- lm(y~x1+x2)
summary(fit)$coefficients

# 2. 
fit_adjusted <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit_adjusted)$coefficients

fit_unadjusted <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit_unadjusted)$coefficients

# 3. 
library(lmtest)
y <- mtcars$mpg
x1 <- factor(mtcars$cyl)
x2 <- mtcars$wt

fit_interaction <- lm(y ~ x1 + x2 + x1:x2)
summary(fit_interaction)

fit_noninteraction <- lm(y ~ x1 + x2)
summary(fit_noninteraction)

# likelihood ration test 
lrtest(fit_interaction, fit_noninteraction)

# 4. 
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# 5.
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

model <- lm(y~x)
max(lm.influence(model)$hat)  # we thake the maximum value

# 6. 
mod <- lm.influence(model)$hat
dfbetas(model)
