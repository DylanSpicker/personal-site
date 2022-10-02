library(geepack)

## COVID-19 Ontario Data
covid <- read.csv("data/COVID/ontario_by_phu.csv")

# We need to sort this, for annoying reasons
covid <- covid[order(covid$ID, covid$time_idx), ]

## Consider some Basic Plotting
plot(positivity_rate ~ time_idx, col = as.factor(ID), data = covid, type='p')

## Select a Random Sample
set.seed(314)
sampled_idx <- sample(unique(covid$ID), 5, FALSE)
lattice::xyplot(positivity_rate ~ time_idx|ID,
      groups = ID,
      data = covid,
      panel = function(x, y){
        lattice::panel.xyplot(x, y, type='p')
        lattice::panel.linejoin(x, y, fun=mean, horizontal=F, lwd=2, col=1)
      })

head(covid)
covid$size_factor <- as.factor(cut(covid$estimated_pop, breaks = quantile(unique(covid$estimated_pop)), include.lowest = TRUE, labels = FALSE))
lattice::xyplot(positivity_rate ~ time_idx|size_factor,
      groups = ID,
      data = covid,
      panel = function(x, y){
        lattice::panel.xyplot(x, y, type='p')
        lattice::panel.linejoin(x, y, fun=mean, horizontal=F, lwd=2, col=1)
      })

## Let's try to model!
?geeglm

## This will take a long time to run!
# There are a *lot* of time points in these data.
# basic.model <- geeglm(
#     formula = positivity_rate ~ time_idx + estimated_pop + time_idx*estimated_pop,
#     family = binomial,      # This will specify the variance function and link!
#     data = covid,
#     id = ID,
#     corstr = "exchangeable" # correlation structure
# )

# We can consider just time points from 580 onward (last 20 days or so)

lattice::xyplot(positivity_rate ~ time_idx|size_factor,
      groups = ID,
      data = covid,
      subset = (time_idx >= 580),
      panel = function(x, y){
        lattice::panel.xyplot(x, y, type='p')
        lattice::panel.linejoin(x, y, fun=mean, horizontal=F, lwd=2, col=1)
      })

basic.model <- geeglm(
    formula = cbind(round(positivity_rate*test_volume,0), test_volume-round(positivity_rate*test_volume,0)) ~ time_idx + estimated_pop + time_idx*estimated_pop,
    family = binomial,      # This will specify the variance function and link!
    data = covid,
    id = ID,
    subset = (time_idx >= 580),
    corstr = "exchangeable" # correlation structure
)

# Probably Missing some Structure?
plot(basic.model)

## Try with a Quadratic Time Trend? 
quadratic.model <- geeglm(
    formula = cbind(round(positivity_rate*test_volume,0), test_volume-round(positivity_rate*test_volume,0)) ~ (I(time_idx^2)+time_idx)*estimated_pop,
    family = binomial,      # This will specify the variance function and link!
    data = covid,
    id = ID,
    subset = (time_idx >= 580),
    corstr = "exchangeable" # correlation structure
)

# Probably Slight Better looking?
par(mfrow=c(2,1))
plot(basic.model)
plot(quadratic.model)

## Exchangeable Correlation is Probably a Bad Assumption
## However: the realities of real data are bumping into us again.
#### Time Table (unstructured)
# >= 600 - 0.06790709 || >= 600, no ID filter - 0.1021786
# >= 595 - 0.4398305  || >= 595, no ID filter - 0.9562612
# >= 590 - 1.608114   || >= 590, no ID filter - 9.11186
# >= 585 - 7.472277   || >= 585, no ID filter - 50.22248
# >= 580 - 23.83877   || >= 580, no ID filter - 2.551463 (minutes)

## Autoregressive is Fast (enough). But is it better?
quadratic.model.AR <- geeglm(
    formula = cbind(round(positivity_rate*test_volume,0), test_volume-round(positivity_rate*test_volume,0)) ~ (I(time_idx^2)+time_idx)*estimated_pop,
    family = binomial,      # This will specify the variance function and link!
    data = covid,
    id = ID,
    subset = (time_idx >= 580),
    corstr = "ar1" # correlation structure
)

## We can compare using QIC.
# QIC: Select a Correlation Structure
# QICu: models with different mean structures
# CIC: Alternative to QIC
QIC(quadratic.model)
QIC(quadratic.model.AR)

# Can also turn to this to see how much nicer the quadratic is vs. linear
QIC(quadratic.model)
QIC(basic.model)

# Turns out that the QICu is actually *higher*! 
# The extra 2 parameters do not add enough versus linear
# Now neither of these are using the correlation structure we actually proposed.
basic.model.AR <- update(basic.model, corstr = "ar1")
QIC(quadratic.model.AR)
QIC(basic.model.AR) 

# Conclusion doesn't change. Interesting. 

# Let's consider using the factor levels rather than the estimated pop directly
# Figure out whether this might work well enough
factor.model.AR <- geeglm(
    formula = cbind(round(positivity_rate*test_volume,0), test_volume-round(positivity_rate*test_volume,0)) ~ (I(time_idx^2)+time_idx)*factor(size_factor),
    family = binomial,      # This will specify the variance function and link!
    data = covid,
    id = ID,
    subset = (time_idx >= 580),
    corstr = "ar1" # correlation structure
)

QIC(quadratic.model.AR)
QIC(factor.model.AR)    
# Not Including it as Factor 'fits' better
# Perhaps instead we should just cut it off as "big" or "not"

covid$large <- as.numeric(covid$size_factor == 4)
binary.model.AR <- geeglm(
    formula = cbind(round(positivity_rate*test_volume,0), test_volume-round(positivity_rate*test_volume,0)) ~ (I(time_idx^2)+time_idx)*(estimated_pop + large),
    family = binomial,      # This will specify the variance function and link!
    data = covid,
    id = ID,
    subset = (time_idx >= 580),
    corstr = "ar1" # correlation structure
)

QIC(quadratic.model.AR)
QIC(binary.model.AR) 

# Might we still prefer this model?
summary(binary.model.AR)

# We want to test whether 'large' and 'small' PHUs have the same time-trend for positivity
# We want to use a Wald test.
L <- rbind(c(0, 0, 0, 0, 0, 0, 1, 0, 0), c(0, 0, 0, 0, 0, 0, 0, 0, 1))
beta.hat <- coef(binary.model.AR)
LB <- c(0,0)
est_cov <- vcov(binary.model.AR)
W <- t(L%*%beta.hat - LB)%*%solve(L%*%est_cov%*%t(L))%*%(L%*%beta.hat - LB)
1 - pchisq(W, df=2)
# Reject H0 and conclude they are different!