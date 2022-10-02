# Load in some data
teachers <- read.csv("data/Teachers Employment/teachers.csv")

head(teachers) # Take a look at the data

# We can do some basic statistics on it 
n <- nrow(teachers)
n_censor <- sum(teachers$censor)
n_events <- n-n_censor


# It is in a "person-level" format
## Each person has a row containing {id, Xi, indicator}
# Often times we need "person-period" format
## This requires a row for each person, for each period they are 
## under observation for.
PLPP <- function(data, id, period, event, direction=c("period","level")) { 
    stopifnot(is.matrix(data) || is.data.frame(data))
    flipEvent <- FALSE 

    stopifnot(c(id, period, event) %in% c(colnames(data), 1:ncol(data)) || c(id, period, censor) %in% c(colnames(data), 1:ncol(data)))

    if (any(is.na(data[, c(id, period, event)]))) {
        stop("PLPP cannot currently handle missing data")
    }
    switch(match.arg(direction),
            period = {
                index <- rep(1:nrow(data), data[, period])
                idmax <- cumsum(data[, period])
                reve <- !data[, event]
                dat <- data[index, ]
                dat[, period] <- ave(dat[, period], dat[, id], FUN = seq_along)
                dat[, event] <- 0
                dat[idmax, event] <- reve
            },
            level = {
                tmp <- cbind(data[, c(period, id)], i = 1:nrow(data))
                index <- as.vector(by(tmp, tmp[, id], FUN = function(x) x[which.max(x[, period]), "i"]))
                dat <- data[index, ]
                dat[, event] <- as.integer(!dat[, event])
            })
    rownames(dat) = NULL
    return(dat)
}


teachers_PL <- teachers
teachers_PP <- PLPP(data = teachers_PL, id = "id", period = "x", event = "censor", direction = "period")

colnames(teachers_PP) <- c("id", "PERIOD", "EVENT")
subset(teachers_PL, id %in% c(5, 32))
subset(teachers_PP, id == 5) ## Consider the 5th teacher
subset(teachers_PP, id == 32) ## Consider the 32nd teacher

## Generate a (sort of) life table
lifetable <- table(teachers_PP$PERIOD, teachers_PP$EVENT) # This demonstrates the counts for {0, 1}

# This demonstrates the proportions
# Margin of 1 is by row
# Margin of 2 is by column
estimator <- prop.table(lifetable, 1) # This gives the hazard function estimates! 
estimator <- cbind(estimator, 1)      # Add a column of 1s for the survivor function
colnames(estimator) <- c("1 - h", "h", "S")
estimator[,"S"] <- cumprod(unlist(estimator[,"1 - h"]))

estimator <- rbind(c(1,0,1), estimator) # Add a top column for time '0'

# Add a time row
estimator <- cbind(0:(nrow(estimator)-1), estimator)

colnames(estimator)[1] <- "t"

estimator <- data.frame(estimator, check.names=FALSE) # Just make this easier to plot.

## Consider a plot of the hazard rate
## Subset to t > 0, since hazard at t = 0 is just defined to be 0
plot(h ~ t, subset = t > 0, data = estimator, type = 'b')

## Consider a plot of the survivor function
plot(S ~ t, data = estimator, type = 'b')

## To see why it's important to estimate the survivor function this way, 
## consider if we had instead used 'rk/n'
estimator$S.2 <- c(margin.table(lifetable, 1)/n, 0)

## Plot both on the same plot
plot(S ~ t, data = estimator, type = 'b', col = 'blue', ylim = c(min(estimator$S.2), 1))
lines(S.2 ~ t, data = estimator, type = 'b', col = 'red')

# We can see perfect agreement *before* any censoring occurs 
# However, once we start seeing censoring the estimator underestimates the risk
# This makes sense since we are not differentiating between those who were censored
## and those who had the event.

# If we want a median, that's going to be roughly the 't' when 'S = 0.5'
abline(h = 0.5, lty=3)

# To compute this directly, first find 'm' such that S(m) > 0.5 > S(m+1)
estimator

# S(6) = 0.5189038
# S(7) = 0.4876935

m <- 6
Sm <- estimator$S[7]
Smp1 <- estimator$S[8]

median <- m + (Sm - 0.5)/(Sm - Smp1)

median 
abline(v = median, lty = 3) 

## We can compare this to two separate means!
mean(subset(teachers, censor == 0)$x) # Mean of those who quit teaching
mean(teachers$x) # Mean of all teachers who quit teaching

## Notice that both of these are underestimated, compared to the median
## This is why this type of estimation is required.