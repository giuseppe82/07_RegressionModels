# Regression Models - Quiz 4 

library(MASS)
data("shuttle")
shuttle$use<-as.numeric(shuttle$use=='auto')  # convert 1 or zero, auto or not auto
logistic_fit <- glm (use ~ factor(wind) - 1, family = binomial, data = shuttle)
summary(logistic_fit)
exp(coef(logistic_fit))
exp(coef(logistic_fit))[1]/exp(coef(logistic_fit))[2]


logistic_fit2 <- glm (use ~ factor(wind) + factor(magn) - 1, family = binomial, data = shuttle)
summary(logistic_fit2)
exp(coef(logistic_fit2))
exp(coef(logistic_fit2))[1]/exp(coef(logistic_fit2))[2]


data("InsectSprays")
InsectSprays$spray<-as.factor(InsectSprays$spray)  
fit <- glm(count ~ factor(spray) - 1, family = poisson, data = InsectSprays)
summary(fit)
exp(coef(fit))
exp(coef(fit))[1]/exp(coef(fit))[2]
