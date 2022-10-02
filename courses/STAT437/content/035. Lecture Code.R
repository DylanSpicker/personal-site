aml <- read.csv("data/AML/aml.csv")

head(aml) # Take a peak at the format 
# It's person-level data
# Each person has an event time, a 'status' indicator (1 = Censored; 0 = Event)
# Treatment Option in 'x', indicating whether 'Maintenance' or 'Nonmaintenance' therapy
# Some background:
#   Study on Acute Myelogenous Leukemia; The main scientific question of interest is 
#     Should the standard course of chemotherapy should be extended? (Maintained)

# Use the PLPP helper function (course website) to convert to Person-Period Format
aml_PP <- PLPP(aml, "id", "time", "status", direction="period")

# Fit the model using logistic regression, without any proportional odds assumption!

model.LR <- glm(status ~ as.factor(time) - 1, # The "-1" forces no intercept!
                data = aml_PP,
                family = binomial)

summary(model.LR) 

# Let's Extract the Estimated Hazards
expit <- function(w) { 1/(1+exp(-w)) }

hazards.LR <- expit(coef(model.LR))

# What if we had used standard estimation?
hazards.prop <- prop.table(table(aml_PP$time, aml_PP$status), 1)

cbind(hazards.prop, round(hazards.LR, 7)) 
# Looks the same! Nice. What's the benefit then?

# First, can test whether or not coefficients are significant
summary(model.LR) # Appears to be some incredibly high variance in a few periods
# This makes sense since there is such a small sample size! 
# What if we wanted confidence intervals?

hazard.var <- vcov(model.LR)
hazard.se <- sqrt(diag(hazard.var))

## Take a peak at the standard errors.
## ~3700, ~5900, and ~17730 for three different intervals! 
## That doesn't apepar super great, sadly. Still, onward!

cbind(expit(coef(model.LR) - qnorm(0.975)*hazard.se), hazards.LR, expit(coef(model.LR) + qnorm(0.975)*hazard.se))

# Notice that several of these intervals are just [0,1]
# This serves as a cautionary tale for sample size considerations!

# What about survival estimates?
surv.LR <- cumprod(1 - hazards.LR)

# And confidence intervals? 
# Work on the log(surv.LR)
G <- t(replicate(length(hazards.LR), -hazards.LR))
G[upper.tri(G)] <- 0

logS.var <- G %*% hazard.var %*% t(G)
logS.se <- sqrt(diag(logS.var))
logS <- log(surv.LR)

round(cbind(exp(logS - qnorm(0.975)*logS.se), surv.LR, exp(logS + qnorm(0.975)*logS.se)), 5)

## What about the treatment?
model.PO <- glm(status ~ as.factor(time) - 1 + as.factor(x), 
                data = aml_PP,
                family = binomial)

summary(model.PO) 

# Consider 'Nonmaintained' vs. 'Maintained'
# odds[Nonmaintained] = exp(alpha_j + beta)
# odds[Maintained] = exp(alpha_j)
# odds ratio [nonmaintained vs. maintained] = exp(beta) => Constant over time

## Test whether there appears to be a significant difference
# H0: beta1 = 0
# From summary, p = 0.4314 => Fail to reject H0
OR <- exp(coef(model.PO)[7])
OR.lower <- exp(coef(model.PO)[7] - qnorm(0.975)*sqrt(vcov(model.PO)[7,7]))
OR.upper <- exp(coef(model.PO)[7] + qnorm(0.975)*sqrt(vcov(model.PO)[7,7]))

c(OR.lower, OR, OR.upper)

## What if we wanted to relax proportional odds assumptions?
model.saturated <- glm(status ~ as.factor(time)*as.factor(x) - 1, data = aml_PP, family = binomial)

# Warning: we do *not* have enough data to reliably estimate this.
# We will proceed as an illustration, but if these data were actually analyzed in practice
# this is pushing them way further than they can go.

# Is the proportional odds assumption valid?
# Test H0: b2 = b3 = ... = 0
# Can do a difference in deviance test! 

D0 <- model.PO$deviance
D1 <- model.saturated$deviance
DeltaD <- D0 - D1

df <- model.PO$df.residual - model.saturated$df.residual

1 - pchisq(DeltaD, df) 
# Fail to Reject H0 => Proportional Odds is acceptable!

anova(model.PO, model.saturated, test="Chisq") # Exact same test!

## Exact same process could give us tests within the proportional odds model
## Could compute the survival function, the confidence intervals, etc.