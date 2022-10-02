library(geepack)

## Load in Seizure Data
seizures <- read.csv("data/Epilepsy/seizures_full.csv")

# Should re-shape this to be long
seizures_long <- reshape(
    data = seizures,
    varying = c("y0","y1","y2","y3","y4","offset0","offset1","offset2","offset3","offset4"),
    sep = "",
    direction = "long",
    idvar = "ID"
)

# Plot the data by treatment group
lattice::xyplot(y ~ time|as.factor(Trt),
      groups = ID,
      data = seizures_long,
      panel = function(x, y){
        lattice::panel.xyplot(x, y, type='p')
        lattice::panel.linejoin(x, y, fun=mean, horizontal=F, lwd=2, col=1)
      })

# Maybe an outlier?
plot(y ~ time, data = seizures_long, subset = (ID == 49))

# Remove ID: 49 from the data.
seizures_long <- seizures_long[which(seizures_long$ID != 49), ]

### GEEGLM is very picky
### Data *must* be ordered by ID, then by time
seizures_long <- seizures_long[order(seizures_long$ID, seizures_long$time), ]

# We are interested in the following questions:
## What is the relative rate, in the placebo group, post-vs-pre treatment.
## Is there a significant difference in pre-vs-post RRs between the two treatment arms?
## Is there a treatment effect on the rates of seizures, overall?
## Is there evidence of overdisperson?
## What is the estimated correlation?
## Does baseline age change any of these conclusions?

# First Let's try to Decide on Correlation
uns <- geeglm(
    y ~ offset(log(offset)) + Age*Trt*as.factor(time),
    data = seizures_long,
    family = poisson,
    id = ID,
    corstr = "uns"
)
exch <- geeglm(
    y ~ offset(log(offset)) + Age*Trt*as.factor(time),
    data = seizures_long,
    family = poisson,
    id = ID,
    corstr = "exch"
)
ar1 <- geeglm(
    y ~ offset(log(offset)) + Age*Trt*as.factor(time),
    data = seizures_long,
    family = poisson,
    id = ID,
    corstr = "ar1"
)

cbind(QIC(uns),QIC(exch),QIC(ar1))

## Exchangeable Seems Preferable!
# We want pre-vs-post treatment indicator, rather than time
seizures_long$post <- as.numeric(seizures_long$time != 0)

# We want questions regarding RR in placebo and Trt group, pre and post
model1 <- geeglm(
    y ~ offset(log(offset)) + Trt*post,
    family = poisson,
    data = seizures_long,
    id = ID,
    corstr = "exch"
)
summary(model1)

# What is the relative rate, in the placebo group, post-vs-pre treatment.
## placebo group, pre-treatment: b0
## placebo group, post-treatment: b0 + b2
## => log[Relative Rate]: b2
RR1 <- exp(coef(model1)['post']) # Is this significant? (Summary says no. But)
Z <- coef(model1)['post']/sqrt(diag(vcov(model1))['post'])
2*(1 - pnorm(Z))

# Is there a significant difference in pre-vs-post RRs between the two treatment arms?
## treatment group, pre-treatment: b0 + b1
## treatment group, post-treatment: b0 + b1 + b2 + b3
## => log[Relative Rate]: b2 + b3
RR2 <- exp(coef(model1)['post'] + coef(model1)['Trt:post'])
RR2

# If they are the same between Trt = 0 and Trt = 1, then b3 = 0
Z <- abs(coef(model1)['Trt:post']/sqrt(diag(vcov(model1))['Trt:post']))
2*(1 - pnorm(Z))

## Is there a treatment effect on the rates of seizures, overall?
# H0: b1 = b3 = 0
# Can compute this directly as we did last time, LB = 0
# Can also use anova
model1.reduced <- update(model1, formula = y ~ offset(log(offset)) + post)
anova(model1.reduced, model1) # p = 0.16, do not reject H0.

## Is there evidence of overdisperson?
# variance = phi*mu_{ij}
summary(model1)$dispersion # 10.4 => Std.err 2.28

## What is the estimated correlation?
summary(model1)$corr # 0.598 => Std.err 0.0811


## Does baseline age change any of these conclusions?
# IDEA: try to work through the factors again.
# However, we can see whether adding age at all helps, easily
model2 <- geeglm(
    y ~ offset(log(offset)) + Trt*post + Trt*Age + Age*post,
    family = poisson,
    data = seizures_long,
    id = ID,
    corstr = "exch"
)

# This adds 3 additional factors compared to model1
# Can use a Wald based test of H0: Age = Trt:Age = post:Age = 0
# Can use ANOVA in R
anova(model2, model1) # Reject H0! Something here matters!

summary(model2) # Trt:Age does not seem to matter; drop that
model2.reduced <- geeglm(
    y ~ offset(log(offset)) + Trt*post + Age*post,
    family = poisson,
    data = seizures_long,
    id = ID,
    corstr = "exch"
)

# Now we could re-answer the questions above, using this new model!
# Note, we will have to indicate that all conclusions are "at fixed age!"

# Placebo baseline vs. post: b0 + b3*Age vs. b0 + b2 + b3*Age
# still just H0: b2 = 0 
RR1.new <- exp(coef(model2.reduced)['post']) # Is this significant? (Summary says no. But)
Z.new <- abs(coef(model2.reduced)['post']/sqrt(diag(vcov(model2.reduced))['post']))
2*(1 - pnorm(Z.new)) # Reject H0!

# etc.
QIC(model2.reduced)
QIC(model1.reduced)