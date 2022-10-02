## qmixchisq: Provides quantiles for the mixture of 2 chi-square distributions
## This will compute the comparison value based on the two df's and the relevant
## significance level (q).
qmixchisq <- function(df1, df2, q) {
    sig_level <- 1 - q

    L_fn <- function(c) {
        computed_sig <- 0.5*pchisq(c, df1) + 0.5*pchisq(c, df2)
        (sig_level - computed_sig)^2
    }

    lower <- qchisq(sig_level, df1) 
    upper <- qchisq(sig_level, df2)

    optimize(L_fn, interval=c(lower, upper))$minimum
}

## pmixchisq: Provides the probability for the mixture of 2 chi-square distributions
## This will compute 1-pvalue for an observed value of W, based on the two df's 
## provided.
pmixchisq <- function(df1, df2, W) {
    0.5*pchisq(W, df1) + 0.5*pchisq(W, df2)
}


## create_lag: Takes in a dataframe, an idvar (for grouping), an outcome variable
## to add lags of, a parameter (nlag) specifying how many lags to add.
## This will return a data.frame with {outcome_lag1,...,outcome_lag(nlag)} added
## Data should be in long format and should be sorted {id, time}
create_lag <- function(data, idvar, outcome, nlag=1) {
    
    ids <- unique(unlist(data[idvar]))

    if (nlag == 1) {
        data[paste0(outcome, "_lag")] <- NA
    } else {
        for (nid in 1:nlag) {
            data[paste0(outcome, "_lag", nid)] <- NA
        }
    }

    for(id in ids) {
        which_idx <- which(data[idvar] == id)

        
        if (nlag == 1) {
            data[which_idx[2:(length(which_idx))], paste0(outcome, "_lag")] <- data[which_idx[1:(length(which_idx)-1)], outcome]
        } else {
            for (nid in 1:nlag) {
                data[which_idx[(1+nid):length(which_idx)], paste0(outcome, "_lag", nid)] <- data[which_idx[1:(length(which_idx)-nid)], outcome]
            }
        }
        
    }

    return(data)
}


### Convert between 'person-level' and 'person-period' data
### Pass in the data frame, id variable, period variable, 
### event indicator, and the direction of the transition.
PLPP <- function(data, id, period, event, direction=c("period","level")) { 
    stopifnot(is.matrix(data) || is.data.frame(data))
    stopifnot(c(id, period, event) %in% c(colnames(data), 1:ncol(data)))
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