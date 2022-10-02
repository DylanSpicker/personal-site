library(nlme)

air_pollution <- read.csv("data/Six Cities/air_pollution.csv")

## Previous Research Suggests that log(fev1) is related (linearly) to 
#### log(height)
#### age

# Note that {age} really has two effects: baseline age, and then longitudinal
# (e.g., cross-sectional vs. longitudinal effects)
# let's include baseline age!
# Same thing with height... include baseline as well!

# Model we are interest in is then:
model1 <- lme(
    fixed = logfev1 ~ age + log(ht) + baseage + log(baseht) ,
    random =~age|id,
    correlation = NULL, # Defaults to sigma^2 I 
    method = 'ML',
    data = air_pollution
)

summary(model1)

## Can think of this as several separate models:
# 1. The marginal model, E[Yij] = b0 + b1*Age + b2*log(Ht) + b3*BAge + b4*log(BHt)
# 2. Cross-sectional effects: E[Yi1] = b0 + (b1 + b3)*BAge + (b2 + b4)*log(BHt)
# 3. Longitudinal Model: E[Yij - Yi1] = b1*(Age - BAge) + b2*(log(Ht) - log(Bht))

# => Gives nice interpretations of effect terms
# (b1 + b3) is the cross-sectional effect of baseline age on outcome
# (b2 + b4) is the cross-sectional effect of baseline log(height) on outcome
# b1 is the longitudinal effect of age (unit increase over baseline)
# b3 is the longitudinal effect of log(height) (unit increase over baseline)

# Q1: What is the estimated change in outcome for a one-year increase in age?
## b1 = 0.0235354 => log(FEV1) increases by 0.0235354 => FEV1 multiplied by 1.024

# Q2: What is the expected change in outcome for a 10% increase in height?
## log(height) increases by 1 => height multiplied by e
## 10% increase in height is: height*1.1 = height*exp(log(1.1)) = log(height) + log(1.1)
## => b2*log(1.1) = 2.2368833*log(1.1) = 0.2131977
## => exp(0.2131977) = 1.237629 => 24% increase for 10% increase in height.

# Q3: Do the cross-sectional and longitudinal effects differ?
## Test of H0: b3 = 0 or H0: b4 = 0 [or jointly, H0: b3 = b4 = 0]
## Individually: H0: b3 = 0, p = 0.0271 => reject H0
##               H0: b4 = 0, p = 0.1325 => do not reject H0
## Joint:
L <- rbind(c(0,0,0,1,0), c(0,0,0,0,1))
LB <- c(0,0)
beta.hat <- fixef(model1)
est_var <- summary(model1)$varFix
W <- t(L%*%beta.hat - LB) %*% solve(L %*% est_var %*% t(L)) %*% (L%*%beta.hat - LB)
1 - pchisq(W, df = 2) 

## Alternatively 
model1.reduced <- update(model1, fixed = logfev1 ~ age + log(ht))
anova(model1, model1.reduced)

### Random Effects Investigation
# Recall that 'REML' provides better estimates
model1.reml <- update(model1, method = 'REML')
summary(model1.reml)

## What is the estimated D? G?
# 0.110485541^2
# 0.007078381^2 => Seems close to zero?
# -0.553*0.007078381*0.110485541
# sigma^2 = 0.060237881^2

# Try a model w/o random slope?
model1.reml.reduced <- update(model1.reml, random = ~1|id)
summary(model1.reml.reduced)

anova(model1.reml.reduced, model1.reml) # Careful, the p-value here is not correct!

# Under H0, the LR statistic (73.7581) follows mixture chi-square
# df1 = 1 (single random effect)
# df2 = 2 (two random effects)
qmixchisq(1, 2, 0.05)
1 - pmixchisq(1, 2, 73.7581)

# Reject H0! The Random Slope Matters.

# Model Diagnostics
## There will be a whole lot of plots here 

## ACF - Checks that errors are independent
plot(ACF(model1.reml), alpha = 0.01, main = "ACF plot for independent errors.") # This looks problematic!
# This may not be the best way to check the model fit though, since there are not evenly spaced errors...

# Instead we can use a 'semi-Variogram'. 
# This should fluctuate randomly around 1
vg <- Variogram(model1.reml, form = ~age|id, resType = "pearson")
plot(vg, sigma=1) ## Looks okay, honestly.

# Residuals vs. Fitted (no patterns)
plot(model1.reml, main = "Plot of residuals vs. fitted.")

# QQPlot for normality of errors
qqnorm(model1.reml, ~ residuals(., type="pearson")) # Some issues... probably

# Plots for the Predicted (BLUPs)
plot(ranef(model1.reml)) 
qqnorm(model1.reml, ~ranef(.)) # These look okay!

# Observed vs. Fitted
plot(model1.reml, logfev1 ~ fitted(.), abline = c(0,1), main = "Observed vs. Fitted")
plot(model1.reml, logfev1 ~ fitted(.)|id, abline = c(0,1), main = "Observed vs. Fitted (By Subject)")
# Could also look (e.g.) by treatment, if it existed!

### Intervals
intervals(model1.reml)

### Predictions
new_data <- data.frame(id = c(1, 25, 25, 25),
                       age = c(18, 18, 18, 18),
                       ht = c(1.54, 1.85, 1.7, 2),
                       baseht = c(1.2, 1.32, 1.32, 1.32),
                       baseage = c(9.3415, 8.0274, 8.0274, 8.0274))

# level specifies whether at the population [0] or subject [1] level
predict(model1.reml, newdata = new_data, level = c(0, 1))