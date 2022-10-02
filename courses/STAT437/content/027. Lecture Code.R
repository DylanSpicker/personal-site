# In this analysis we consider weighting and imputation type corrections inside of GEEs
# We saw some differences with results between the full dataset and the partial dataset
# in terms of number of significant differences. These *may* be remedied via weight and MI
TLC <- read.csv("data/TLC/TLC.csv")
TLC_long <- reshape(data = TLC,
                    varying = c("W0", "W1", "W4", "W6"),
                    timevar = "week",
                    idvar = "ID",
                    direction = "long",
                    sep = "")
TLC_long <- TLC_long[order(TLC_long$ID, TLC_long$week), ]
TLC_M <- read.csv("data/TLC/TLC_mar.csv")
TLC_long_M <- reshape(data = TLC_M,
                    varying = c("W0", "W1", "W4", "W6"),
                    timevar = "week",
                    idvar = "ID",
                    direction = "long",
                    sep = "")
TLC_long_M <- TLC_long_M[order(TLC_long_M$ID, TLC_long_M$week), ]

## What if we are concerned about the 'GEE' variance structure specification?
## Here we specify an 'indep' structure to simulate "getting it wrong" so that 
## the likelihood technique is not defensible.
model.complete.GEE <- geeglm(
    W ~ factor(week)*Treatment,
    data = TLC_long,
    family = gaussian,
    id = ID,
    corstr = "indep"
)

model.GEE.MAR <- geeglm(
    W ~ factor(week)*Treatment,
    data = TLC_long_M,
    family = gaussian,
    id = ID,
    corstr = "indep"
)

round(summary(model.complete.GEE)$coefficients[,c(1,4)],4)
round(summary(model.GEE.MAR)$coefficients[,c(1,4)],4)

# We can use weighting or MI
######## WEIGHTING
# The idea with the IPW-GEE is that we will define weights, which are
# 1/P(observed). This can be estimated by simply performing a logistic regression
# on an observed indicator.

# Define observed indicators
TLC_long_M$R <- as.numeric(!is.na(TLC_long_M$W))

TLC_M <- reshape(
    data = TLC_long_M,
    direction = "wide",
    v.names = c("W", "R"),
    idvar = "ID", 
    sep = "",
    timevar = "week"
)

# Fit a model; probability of being observed is based on a model of the interaction 
# between W0:as.numeric(tlc_MAR$Treatment == 'A'), which increases overtime 
TLC_M$Pi0 <- 1
TLC_M$Pi1 <- glm(R1 ~ W0:as.numeric(Treatment=='A'), data = TLC_M, family=binomial)$fitted.values
TLC_M$Pi4 <- glm(R4 ~ W0:as.numeric(Treatment=='A'), data = TLC_M, subset=(R1==1), family=binomial)$fitted.values
TLC_M$Pi6 <- glm(R6 ~ W0:as.numeric(Treatment=='A'), data = TLC_M, subset=(R4==1), family=binomial)$fitted.values

TLC_M$weight0 <- (TLC_M$R0)/TLC_M$Pi0
TLC_M$weight1 <- (TLC_M$R1)/(TLC_M$Pi0*TLC_M$Pi1)
TLC_M$weight4 <- (TLC_M$R4)/(TLC_M$Pi0*TLC_M$Pi1*TLC_M$Pi4)
TLC_M$weight6 <- (TLC_M$R6)/(TLC_M$Pi0*TLC_M$Pi1*TLC_M$Pi6)

TLC_weighted_long <- reshape(
    data = TLC_M,
    direction = "long",
    varying = c("weight0", "weight1", "weight4", "weight6", 
                "R0", "R1", "R4", "R6",
                "Pi0", "Pi1", "Pi4", "Pi6",
                "W0", "W1", "W4", "W6"),
    idvar = "ID",
    sep = "",
    timevar = "week"
)

model.GEE.weighted <- geeglm(
    W ~ factor(week)*Treatment,
    data = TLC_weighted_long,
    family = gaussian,
    id = ID,
    weights = weight,
    corstr = "indep"
)

# Compare Methods So Far
round(summary(model.complete.GEE)$coefficients[,c(1,4)],4)
round(summary(model.GEE.MAR)$coefficients[,c(1,4)],4)
round(summary(model.GEE.weighted)$coefficients[,c(1,4)],4)


######## Multiple Imputation
# We will use the MICE package
# Note that this will work even *if* the missingness is not monotone, which is nice!
library(mice)
md.pattern(TLC_M) # This plot just shows missingness patterns. Handy.

# Create Imputed Datasets
preds <- matrix(c(0,1,1,rep(0,15), 
                  0,1,1,0,1,rep(0,13), 
                  0,1,1,0,1,0,1,rep(0,11)), byrow=TRUE, nrow=3) 
## This is a matrix of predictors
## It contains a '1' whenever we want to use that column to predict the relevant row, 
## and a '0' otherwise.

# List of 'blocks'
# The idea with blocks are that we want to create a list of which variables to 
# impute, together. Can just create the list (in order).
bls <- list("W1", "W4","W6")

# Run the actual imputation procedure. 'm' is the number of datasets used! 
imp_TLC_M <- mice(TLC_M, maxit = 10, m = 5, predictorMatrix = preds, blocks=bls, seed = 1, printFlag = FALSE)

# These plots will help us determine whether or not the imputed values seem reasonable
stripplot(imp_TLC_M, W1, xlab = "Imputation number")
stripplot(imp_TLC_M, W4, xlab = "Imputation number")
stripplot(imp_TLC_M, W6, xlab = "Imputation number")

### We now want to compute the estimated value for each of the imputed datasets
models <- with(imp_TLC_M, {
    # In this block, all of the column names of the dataframe are
    # available as variables, based on each imputed version of the dataset
    dat <- data.frame(
        "ID" = ID,
        "Treatment" = Treatment,
        "W0" = W0,
        "W1" = W1,
        "W4" = W4, 
        "W6" = W6
    )

    # Now 'dat' contains the completed data.frame
    # Can re-fit this model! 
    dat_long <- reshape(data = dat, 
                        varying = c("W0", "W1", "W4", "W6"),
                        timevar = "week",
                        idvar = "ID",
                        direction = "long",
                        sep = "")
    dat_long <- dat_long[order(dat_long$ID, dat_long$week), ]
    
    geeglm(
        W ~ factor(week)*Treatment,
        data = dat_long,
        family = gaussian,
        id = ID,
        corstr = "indep"
    )
})

## Can 'pool' together the estimates using 'pool'
# dfcom = residual 'df' based on complete data
round(summary(pool(models, dfcom = df.residual(model.complete.GEE)))[,c(2,6)],4) 
round(summary(model.complete.GEE)$coefficients[,c(1,4)], 4)
