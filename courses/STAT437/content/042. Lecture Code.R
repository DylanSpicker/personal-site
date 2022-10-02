library(survival)

churn <- read.csv("data/Customer Churn/customer_churn.csv")

# Create an Event Indicator Instead
churn$event <- 1 - churn$censored

churn <- churn[,-3] # This drops the censored indicator

# We want to relevel some of the factors we have so that the model uses the 
# correct baseline levels.
churn$phone_service <- relevel(churn$phone_service, ref = "No")
churn$internet_service <- relevel(churn$internet_service, ref = "No")

# Continuing from where we were last time
# AFT models can be fit using survreg, covariates can be included in the formula
# The specification of the distribution implies a particular survival model.

# The [, -c(1,3)] removes ID and total charges
# Why might we remove the total charges?
model1.exp <- survreg(Surv(time, event) ~ . , data = churn[,-c(1,3)], dist = "exp") 
model1.wei <- survreg(Surv(time, event) ~ . , data = churn[,-c(1,3)], dist = "wei")
model1.logn <- survreg(Surv(time, event) ~ . , data = churn[,-c(1,3)], dist = "logn")
model1.logl <- survreg(Surv(time, event) ~ . , data = churn[,-c(1,3)], dist = "logl")

summary(model1.exp)
summary(model1.wei)
summary(model1.logn)
summary(model1.logl)

## Model Selection?
## Can use AIC
AIC(model1.exp, model1.wei, model1.logn, model1.logl) 

# Appears that lognormal is preferable

# Can formally test exponential vs. Weibull, since exponential is Weibull with parameter restriction
anova(model1.exp, model1.wei) # p < 0.05 => reject H0

# Can also look at the residuals
# We know that residuals should (roughly) follow a normal
# and could try to assess those. 

my_resid <- log(churn$time) - model1.logn$linear.predictors
qqnorm(my_resid)
qqline(my_resid)

# Certainly not perfect.. But probably fine for us for right now.


# Look at the model output
summary(model1.logn)  # Why the NAs?
# The NAs come because of strange mixtures in the factor levels. 
# multiple_lines$no_phone_service is perfectly correlated with no_phone_service
# This is not intrinsically a problem, we can just ignore those that are NA
# we could, instead, refactor to collapse categories, or more carefully
# specify the model to drop those factors

### Questions
# 1. How does survival time compare between an individual with paperless billing, compared to those who receive paper?
coef(model1.logn)[8]

# Recall it's T0*exp(bj)
exp(coef(model1.logn)[8])  # Therefore, paperless bill customers churn about 14.7% faster than those receiving paper bills

# 2. What is a 95% CI for the impact of whether the individual has a partner or not
exp(coef(model1.logn)[11]) # 1.725633
se <- sqrt(diag(model1.logn$var))[11]

exp(coef(model1.logn)[11]) - qnorm(0.975)*se
exp(coef(model1.logn)[11]) + qnorm(0.975)*se

# (1.604172, 1.847095)

# 3. Does streaming behaviour impact the rate at which a customer churns?
model2 <- update(model1.logn, data = churn[,-c(1,3,19,20)])
anova(model2, model1.logn) # p = 0.9993 - no impact at all!
-2*(model2$loglik[2]-model1.logn$loglik[2]) # Test statistic directly

