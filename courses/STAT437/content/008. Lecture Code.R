################ 
# Read in the Data and Reshape
# (Taken form previous lectures)
TLC <- read.csv("data/TLC/TLC.csv")
TLC_long <- reshape(data = TLC,
                    varying = c("W0", "W1", "W4", "W6"),
                    timevar = "week",
                    idvar = "ID",
                    times = c(0, 1, 4, 6),
                    direction = "long",
                    sep = "")

TLC_wide <- reshape(data = TLC_long,
                    timevar = "week",
                    v.names = "W",
                    idvar = "ID",
                    times = c(0, 1, 4, 6),
                    direction = "wide",
                    sep = "")

# This lecture requires the "nlme" package
# In can be installed with: install.packages("nlme")
library(nlme) # Read-in the Package
?gls # This opens up the documentation for the 'gls' function

model.saturated <- gls(
  model = W ~ factor(week)*Treatment,         ## Specifies the Formula, like in GLM or LM
  data = TLC_long,                            ## Specifies the data frame (expected in long format)
  corr = corSymm(form = ~1|ID),               ## Unstructured Correlation matrix, within ID
  weights = varIdent(form = ~1|factor(week)), ## Allow for the variances to differ by week
  method = 'ML'                               ## ML or REML
)
summary(model.saturated)

## Some questions to try and answer using the model
# How does the correlation structure look? Could we use a different pattern?
# How does the variance structure look? Could we have assumed constant variance?
# What are the parameter interpretations?
# Are the patterns of change over time the same between the groups?

# Test this hypothesis using a matrix
# H0: beta5 = beta6 = beta7 = 0
L <- rbind(c(0, 0, 0, 0, 0, 1, 0, 0),
           c(0, 0, 0, 0, 0, 0, 1, 0),
           c(0, 0, 0, 0, 0, 0, 0, 1))

# rbind stacks vectors into a matrix (each row)

# Form the Test Statistic
beta.hat <- coef(model.saturated)
v.beta.hat <- model.saturated$varBeta
LB.hat <- L%*%beta.hat
LB <- c(0,0,0)

W <- t(LB)%*%solve(L%*%v.beta.hat%*%t(L))%*%LB
p.value <- 1 - pchisq(W, df=3)

## We could have also fit a reduced model, dropping those terms
model.reduced <-  gls(
  model = W ~ factor(week) + Treatment,       ## Drop the Interaction Terms
  data = TLC_long,
  corr = corSymm(form = ~1|ID),               ## Unstructured Correlation matrix, within ID
  weights = varIdent(form = ~1|factor(week)), ## Allow for the variances to differ by week
  method = 'ML'                               ## ML or REML
)
summary(model.reduced)

## LRT: by hand *or* with ANOVA
l2 <- model.saturated$logLik
l1 <- model.reduced$logLik
ts <- -2*(l1 - l2)
p.value.alt <- 1 - pchisq(ts, df=3)
anova(model.saturated, model.reduced)

## Note: Because those parameters are significant, testing alternative parameters does not make sense! (Why?!)
##       But we still *could* do it. 

## Instead let's do a prediction (with CI!): mean blood lead level at week 1 for succimer group
L <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0), byrow = FALSE, nrow=1)
point.estimate <- L%*%beta.hat
se.estimate <- sqrt(L%*%v.beta.hat%*%t(L))

# Approximate 95% CI
upper <- point.estimate + qnorm(0.975)*se.estimate
lower <- point.estimate + qnorm(0.025)*se.estimate
c(lower, point.estimate, upper)

### What about our correlation pattern?
# We typically use REML when concerned w/ correlation/covariance parameters
model.unstr <- gls(W ~ factor(week)*Treatment, 
                   data = TLC_long,
                   corr = corSymm(form = ~1|ID), 
                   weights = varIdent(form = ~1|factor(week)))
model.exch <- gls(W ~ factor(week)*Treatment, 
                  data = TLC_long,
                  corr = corCompSymm(form = ~1|ID), 
                  weights = varIdent(form = ~1|factor(week)))
anova(model.unstr, model.exch)

model.nonconst <- gls(W ~ factor(week)*Treatment, 
                       data = TLC_long,
                       corr = corSymm(form = ~1|ID))
anova(model.unstr, model.nonconst)

# ML must be used for likelihood ratio test when comparing nested mean models
# REML should be used for estimation of variance and correlation parameters
# Comparisons of nested correlation models can be made using REML likelihood ratio test.
# Other model selection criteria, such as AIC or BIC, can be used for comparing un-nested models

## What if we wanted to use a parametric curve?
model.quadratic <- gls(W ~ (week + I(week^2)) + (week + I(week^2)):Treatment, 
                       data = TLC_long,
                       corr = corSymm(form = ~1|ID), 
                       weights = varIdent(form = ~1|factor(week)))
summary(model.quadratic)

# Could run the same types of tests!
# Should be able to arbitrarily fit too (e.g. overfit!)
model.quadratic <- gls(W ~ (week + I(week^2)+ I(week^3)) + (week + I(week^2)+ I(week^3)):Treatment, 
                       data = TLC_long,
                       corr = corSymm(form = ~1|ID), 
                       weights = varIdent(form = ~1|factor(week)))

## We can also plot our fits
new_data <- data.frame(week = rep(seq(0,6,length.out = 500),2), Treatment = c(rep('A', 500), rep('P', 500)))
new_data$preds <- predict(model.quadratic, newdata=new_data)
plot(preds ~ week, data = new_data, col=as.factor(Treatment), type='l', subset=Treatment=='A')
lines(preds ~ week, data = new_data, col=as.factor(Treatment), type='l', subset=Treatment=='P')

