#### PART 1
## Seizures Analysis
# From Lecture 16 - GEE 
library(geepack)
## Load in Seizure Data
seizures <- read.csv("data/Epilepsy/seizures_full.csv")
seizures_long <- reshape(
    data = seizures,
    varying = c("y0","y1","y2","y3","y4","offset0","offset1","offset2","offset3","offset4"),
    sep = "",
    direction = "long",
    idvar = "ID"
)
seizures_long <- seizures_long[which(seizures_long$ID != 49), ]
seizures_long <- seizures_long[order(seizures_long$ID, seizures_long$time), ]
seizures_long$post <- as.numeric(seizures_long$time != 0)

model.complete <- geeglm(
    y ~ offset(log(offset)) + Trt*post + Age*post,
    family = poisson,
    data = seizures_long,
    id = ID,
    corstr = "exch"
)

## Load in Seizure Data [with missingness]
seizures_M <- read.csv("data/Epilepsy/seizures_mcar.csv")
seizures_long_M <- reshape(
    data = seizures_M,
    varying = c("y0","y1","y2","y3","y4","offset0","offset1","offset2","offset3","offset4"),
    sep = "",
    direction = "long",
    idvar = "ID"
)
seizures_long_M <- seizures_long_M[which(seizures_long_M$ID != 49), ]
seizures_long_M <- seizures_long_M[order(seizures_long_M$ID, seizures_long_M$time), ]
seizures_long_M$post <- as.numeric(seizures_long_M$time != 0)

model.MCAR <- geeglm(
    y ~ offset(log(offset)) + Trt*post + Age*post,
    family = poisson,
    data = seizures_long_M,
    id = ID,
    corstr = "exch"
)

# These two analyses should look fairly close to one another
summary(model.complete)$coefficients
summary(model.MCAR)$coefficients

#### PART 2
## Six Cities Analysis
# From Lecture 21 - LMEE
air_pollution <- read.csv("data/Six Cities/air_pollution.csv")
model1 <- lme(
    fixed = logfev1 ~ age + log(ht) + baseage + log(baseht) ,
    random =~age|id,
    correlation = NULL, # Defaults to sigma^2 I 
    method = 'ML',
    data = air_pollution
)
air_pollution_NMAR <- read.csv("data/Six Cities/air_pollution_NMAR.csv")
model1.NMAR <- lme(
    fixed = logfev1 ~ age + log(ht) + baseage + log(baseht) ,
    random =~age|id,
    correlation = NULL, # Defaults to sigma^2 I 
    method = 'ML',
    data = air_pollution_NMAR,
    subset = complete.cases(logfev1)
)

# Consider the coefficient log(baseht)
summary(model1)$tTable
summary(model1.NMAR)$tTable

mean(is.na(air_pollution_NMAR$logfev1)) # Only ~10% missingness


#### PART 3
## TLC
library(nlme)
# From Lectures 3 and 8

## First: Recall that 'nlme' *are* likelihood based procedures
TLC <- read.csv("data/TLC/TLC.csv")
TLC_long <- reshape(data = TLC,
                    varying = c("W0", "W1", "W4", "W6"),
                    timevar = "week",
                    idvar = "ID",
                    direction = "long",
                    sep = "")
TLC_long <- TLC_long[order(TLC_long$ID, TLC_long$week), ]

model.complete <- gls(
    model = W ~ factor(week)*Treatment,
    data = TLC_long,
    corr = corSymm(form = ~1|ID),               ## Unstructured Correlation matrix, within ID
    weights = varIdent(form = ~1|factor(week)), ## Allow for the variances to differ by week
    method = 'ML'                               ## ML or REML
)

TLC_M <- read.csv("data/TLC/TLC_mar.csv")
TLC_long_M <- reshape(data = TLC_M,
                    varying = c("W0", "W1", "W4", "W6"),
                    timevar = "week",
                    idvar = "ID",
                    direction = "long",
                    sep = "")
TLC_long_M <- TLC_long_M[order(TLC_long_M$ID, TLC_long_M$week), ]

model.MAR <- gls(
    model = W ~ factor(week)*Treatment,
    subset = complete.cases(W),
    data = TLC_long_M,
    corr = corSymm(form = ~1|ID),               ## Unstructured Correlation matrix, within ID
    weights = varIdent(form = ~1|factor(week)), ## Allow for the variances to differ by week
    method = 'ML'                               ## ML or REML
)

## Also note though, that the relevant GEE's produce (generally) the same EE
model.complete.GEE <- geeglm(
    W ~ factor(week)*Treatment,
    data = TLC_long,
    family = gaussian,
    id = ID,
    corstr = "unst"
)
model.GEE.MAR <- geeglm(
    W ~ factor(week)*Treatment,
    data = TLC_long_M,
    family = gaussian,
    id = ID,
    corstr = "unst"
)

## These Analyses *should* be close *if* variances are correctly specified
summary(model.complete)$tTable
summary(model.MAR)$tTable
summary(model.complete.GEE)$coefficients
summary(model.GEE.MAR)$coefficients


## This is *not* true of GEEs broadly, just of linear GEEs. 
## In order for this to be considered valid, the variance and correlation need
## to be correctly specified (either invalid likelihood procedure).
## If we are concerned with it, we can turn to either weighting or imputation
## type procedures (next class!)