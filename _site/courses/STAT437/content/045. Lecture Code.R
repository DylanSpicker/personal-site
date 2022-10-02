library(eha)

?eha::phreg

# Read in the dataset
mort <- read.csv("data/Mortality/mort.csv")
head(mort)

# This is in a slightly different format, compared to what we are used to! 
# Can re-format it, if we wanted
# mort$t <- mort$exit - mort$enter
# This will not work perfectly in this case owing to the unique data structure
# but with a little bit of thought should be manageable

model1.ee <- phreg(Surv(enter, exit, event) ~ ses, data = mort, dist = "weibull", shape = 1)

# What are the estimated hazards?
h.lower <- exp(-1*model1.ee$coefficients[2]) # Lower SES
h.upper <- exp(-1*model1.ee$coefficients[2])*exp(model1.ee$coefficients[1])

# Perhaps the birthyear will make a difference as well?
mort$birthyear <- mort$birthdate %/% 1

model2 <- phreg(Surv(enter, exit, event) ~ ses + birthyear, data = mort, dist = "weibull", shape = 1)

# Based on this model, we could take a look at the impact of being born
# 1 year later, holding SES constant. This is given by 
HR <- exp(coef(model2)[2]) 

# To get a 95% confindence interval on this hazard rate, we can use the 
# estimated variance
c(HR*exp(-1*qnorm(0.975)*sqrt(diag(model2$var)[2])), HR, HR*exp(qnorm(0.975)*sqrt(diag(model2$var)[2])))

# There appears to be a slight positive impact on the hazard
# This means that for every year you were born further, the hazard of mortality is increasing
# by about 2.7% (a factor of 1.027)

# There is no need to constrain shape = 1, generally speaking
model3 <- phreg(Surv(enter, exit, event) ~ ses, data = mort, dist = "weibull")

kappa <- exp(coef(model3)[3]) # > 1
rho <- exp(coef(model3)[2])  

# Note that the hazard for a Weibull distribution is given by 
# kappa * rho^{-kappa} * t^{kappa - 1}
# As a result, when kappa > 1, the hazard increases with time
# when kappa < 1, the hazard decreases with time, and 
# when kappa = 1, the hazard is time-invariant
weibull_hazard <- function(kappa, rho, t) { kappa*rho^(-1*kappa)*t^(kappa - 1) }

times <- seq(0, 20, length.out = 200)
plot(weibull_hazard(kappa, rho, times) ~ times, type = 'l')
lines(weibull_hazard(1, exp(1*coef(model1.ee)[2]), times) ~ times, lty = 2)

# We can formally test the goodness of fit between these two models
# Easiest way is simply testing whether H0: exp(log(Kappa)) = 1 => H0: log(Kappa) = 0
model3 # p-value is approximately 0. The weibull fits much better!

# Can also use a piecewise constant baseline hazards
model4 <- pchreg(Surv(enter, exit, event) ~ ses, data = mort, cuts = c(0, 4, 8, 12, 16, 20))

# The hazards are contained in the $hazards object
model4$hazards

# Add this to the above plot
ys <- rep(model4$hazards, each = 2)
xs <- c(0, rep(seq(4, 16, by = 4), each = 2), 20)
lines(ys ~ xs, lty = 3, col = 'red')

## Connection to AFT
# Let's fit an AFT model to the data
model5 <- survival::survreg(Surv(enter, exit, event) ~ ses, data = mort, dist = 'wei') 
# Notice that this does not work. 
# To accommodate this [enter, exit, event] format, EHA has an `aftreg` function
model5 <- aftreg(Surv(enter, exit, event) ~ ses, data = mort, dist = 'weibull')

model5 

# We will actually find that this is not exactly what we want/expect
# instead, should specify 'param' to be 'lifeExp' to see the parameterization
# in the format we are used to
model5 <- aftreg(Surv(enter, exit, event) ~ ses, data = mort, dist = 'weibull', param = 'lifeExp')

# Let's compare the coefficients
rbind("AFT"=unname(coef(model5)), "PH"=unname(coef(model3)))

# Note first that the log(scale) terms are the same between the two models
# this represents the intercept in the AFT model We saw this theoretically!

# Next we had seen that beta* = -beta/kappa
-1*coef(model3)[1]*exp(-1*coef(model3)[3])
coef(model5)[1]

# As a result, we can interpret the coefficient here either in terms of the PH
# formulation (e.g., a Hazards Ratio of exp(0.4842893)) OR in terms of the AFT
# formulation (e.g., a changing of aging speed from baseline)