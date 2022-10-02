library(survival)

churn <- read.csv("data/Customer Churn/customer_churn.csv")

# Create an Event Indicator Instead
churn$event <- 1 - churn$censored

# The survival package is built around 'Surv' objects
# The package handles understanding the event indicators, and the times,
# based on a call to 'Surv()'
survival_object <- Surv(churn$time, churn$event)
plot(survival_object, xlab="Months", ylab="Survival Probability") 

# This provides a nonparametric estimate for survival probabilities
# based on the survival object.
# Generally, not going to work directly with it.
# Instead, it becomes useful as the outcome in survival regressions.

?survival::survreg

# This provides, essentially, an `lm()` for the types of survival regression
# models based on location-scale families that we discussed. 
# We will explore these more completely in the coming lectures.
# For now, let's see how we can use this for likelihood purposes.
model1 <- survreg(Surv(time, event) ~ 1, data = churn, dist = "exp")
summary(model1) 

# In this model, the "intercept" represents the 'mu'
# Remember that 'mu' = log(rho) => exp(mu) = rho
# We saw that the MLE of the hazard was 1/rho
h1.hat <- 1/exp(coef(model1))

# For the CI, we can take the CI on the coefficient itself, then transform
h1.hat.CI <- 1/exp(c(coef(model1) + qnorm(0.975)*sqrt(model1$var),
                     coef(model1) - qnorm(0.975)*sqrt(model1$var)))

# Remember that we saw the MLE to be {Number of Observed Events}/{Sum of Observed Times}
h1.closed_form <- sum(churn$event)/sum(churn$time)

# This gives the same result! 

# We can also generate some plots
# We are assuming parameteric survival based on exponential
# We know that the survival function is exp(-t/rho) = exp(-t*h)
times <- seq(0,12*20,length.out=1000)
surv_prob <- exp(-times*h1.hat)
surv_prob_lower <- exp(-times*h1.hat.CI[2])
surv_prob_upper <- exp(-times*h1.hat.CI[1])

plot(surv_prob ~ times, type='l', ylim=c(0,1))
lines(surv_prob_upper ~ times, col = 'red', lty = 3)
lines(surv_prob_lower ~ times, col = 'red', lty = 3)

# This is probably not a great fit, if we compare back to 
plot(survival_object)
lines(surv_prob ~ times, col = 'red') 
lines(surv_prob_upper ~ times, col = 'red', lty = 3)
lines(surv_prob_lower ~ times, col = 'red', lty = 3)

# We can see that we are not really capturing the structure here
# In particular, there is a far large drop at time 1 compared to the rest of 
# the pattern. 
# This makes sense in the context (people leave after the start) and perhaps
# makes sense to *truncate* the data as well for our study?

truncated_churn <- churn[which(churn$time > 1), ]
survival_object.truncated <- Surv(truncated_churn$time, truncated_churn$event)
model1.truncated <- survreg(Surv(time, event) ~ 1, data = truncated_churn, dist = "exp")
h1.hat.truncated <- 1/exp(coef(model1.truncated))
surv_prob.truncated <- exp(-times*h1.hat.truncated)
plot(survival_object.truncated)
lines(surv_prob.truncated ~ times, col = 'red') 

# This is still not perfect, but it is much better
# Remember though that a truncated analysis needs to be viewed as *conditional*
# That is: This is the estimated survival function GIVEN that the individual does not
#          churn in the first month.
# Gives an indication of why this can be useful, however.

# We can also consider subgroup analyses
# What if we want to compare customers who have phone service, to those who 
# do not have phone service?
# We can use 'subset' to fit a different parameter for each of them
m2.phone <- survreg(Surv(time, event) ~ 1, 
                    data = truncated_churn, 
                    subset = phone_service == "Yes", # Include only those with phone service
                    dist = "exp")
m2.nophone <- survreg(Surv(time, event) ~ 1, 
                    data = truncated_churn, 
                    subset = phone_service != "Yes", # Include only those without phone service
                    dist = "exp")

summary(m2.phone)
summary(m2.nophone)

h2.hat.phone <- 1/exp(coef(m2.phone))
h2.hat.nophone <- 1/exp(coef(m2.nophone))

surv_prob.phone <- exp(-times*h2.hat.phone)
surv_prob.nophone <- exp(-times*h2.hat.nophone)

plot(surv_prob.phone ~ times, type='l', ylim=c(0,1), col='red')
lines(surv_prob.nophone ~ times, col = 'blue')

# Consider the plot overlaid on the data
par(mfrow=c(1,2))
plot(survival_object.truncated[which(truncated_churn$phone_service == "Yes")], main = "With Phone Service")
lines(surv_prob.phone ~ times, col='red')
plot(survival_object.truncated[which(truncated_churn$phone_service != "Yes")], main = "Without Phone Service")
lines(surv_prob.nophone ~ times, col='red')

# We can also run a likelihood ratio test directly on this
# The idea is that we can treat rho1 and rho2 as the rates for those w/ phone
# and those without phone, respectively. Then the first model we fit 
# is a likelihood restriction of rho1 = rho2 = rho.

# Recall that loglikelihoods from independent samples add together
LRT <- -2 * (model1.truncated$loglik[1] - (m2.phone$loglik[1] + m2.nophone$loglik[1])) 
1 - pchisq(LRT, 1) # p > 0.05, do not reject H0; first model is okay.

# We could have actually done this test slightly differently
m2 <- survreg(Surv(time, event) ~ phone_service, data = truncated_churn, dist = "exp")
anova(m2, model1.truncated) # Same test, and gives a hint as to what is to come!

# This gives an easy way to check other subgroups, if we were to care
summary(survreg(Surv(time, event) ~ senior, data = truncated_churn, dist = "exp")) ## This seems to actually matter!