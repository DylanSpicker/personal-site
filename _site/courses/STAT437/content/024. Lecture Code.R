wallstreet <- read.csv("data/Wallstreet/wallstreet_sentiment.csv")

wallstreet_long <- reshape(
    data = wallstreet,
    idvar = "id",
    varying = c("Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8"),
    sep = "",
    direction = "long"
)

# Create an Ordered Factor to make Transitions More Easily Interpretted
wallstreet_long$Y <- ordered(wallstreet_long$Y, c("(-2) Very negative", "(-1) Slightly negative", "(0) Neither negative or positive", "(1) Slightly positive", "(2) Very positive"))
wallstreet_long <- wallstreet_long[order(wallstreet_long$id, wallstreet_long$time),]

# 1. For those who were slightly positive at the previous visit, what is the probability that they are now very positive?
# 2. For those who were "neither negative or positive", are they more likely to be very negative or very positive?
# 3. For those who were "neither negative or positive" at the first measurement, how likely were they to be slightly negative at the second measurement?
# 4. How do the probability transition matrices differ over the time periods? Does time homogeneity seem applicable?
# 5. Are individuals more likely to become more positive or more negative over time?
# 6. For those who were neutral at the second last measurement, what was the most likely report at the final time?
# 7. What is the relative risk (RR) of positive feeling, for those who were positive vs. negative at the previous time?

### Create Lag
wallstreet_long_lagged <- create_lag(wallstreet_long, "id", "Y", nlag=1)
wallstreet_long_lagged$Y <- as.numeric(wallstreet_long_lagged$Y)
wallstreet_long_lagged <- wallstreet_long_lagged[order(wallstreet_long_lagged$id, wallstreet_long_lagged$time), ]

## Frequency Tables
# Time-Homogeneous, 1 Step Process
th_ws_table <- t(table(wallstreet_long_lagged[,c("Y", "Y_lag")]))
th_p_mat <- th_ws_table/rowSums(th_ws_table)

# Non- Time-Homogeneous, 1 Step Process
# T=3 (Second Measurement)
ws_table <- t(table(wallstreet_long_lagged[which(wallstreet_long_lagged$time == 3), c("Y", "Y_lag")]))
p_mat <-  ws_table/rowSums(ws_table)

## Across all T = 3, ... , 8
ws_table_list <- lapply(3:8, function(j){
    ws_table <- t(table(wallstreet_long_lagged[which(wallstreet_long_lagged$time == j), c("Y", "Y_lag")]))
    ws_table/rowSums(ws_table)
})

## Answers
# 1. th_p_mat[4, 5] = 0.03333333
# 2. th_p_mat[3, 1] = 0.1323529 vs. th_p_mat[3, 5] = 0.002941176 ==> Very Negative
# 3. p_mat[3, 2] = 0.1764706
# 4. ws_table_list: can comment on whatever patterns
j <- 1
m <- 2
do.call(c, lapply(ws_table_list, function(x) x[j,m])) # Compares probs j->m overtime

## For questions 5 onward it makes sense to remap the categories
## We will make three categories, where we group {1, 2} into -1; {3} into 0; {4, 5} into {1}
## That way -1 = {Negative, Slightly Negative}; 0 = {Neutral}; 1 = {Slightly Positive, Positive}
wallstreet_long$Y <- ifelse(as.numeric(wallstreet_long$Y) < 3, -1, ifelse(as.numeric(wallstreet_long$Y) > 3, 1, 0))

## Recreate the lagged dataset and tables
wallstreet_long_lagged <- create_lag(wallstreet_long, "id", "Y", nlag=1)
wallstreet_long_lagged <- wallstreet_long_lagged[order(wallstreet_long_lagged$id, wallstreet_long_lagged$time), ]

th_ws_table <- t(table(wallstreet_long_lagged[,c("Y", "Y_lag")]))
th_p_mat <- th_ws_table/rowSums(th_ws_table)
ws_table_list <- lapply(3:8, function(j){
    ws_table <- t(table(wallstreet_long_lagged[which(wallstreet_long_lagged$time == j), c("Y", "Y_lag")]))
    ws_table/rowSums(ws_table)
})

## Answers
# 5. th_p_mat[,c(1, 3)]: Regardless of starting point, more likely to end up in "negative"
# 6. ws_table_list[[6]][2, ] = 0.5833333, 0.4166667, 0.0000000
#### So, 58.3% went neutral -> negative; 41.6% went neutral -> neutral; none went neutral -> positive
# 7. th_p_mat[3, 3]/th_p_mat[1, 3] = 6.972352
#### So, people who previously felt positive were ~7 times as likely to feel positive at the next time,
##### compared to those who previously felt negative.


# Logistic Regression Modelling
## We need to make the outcome binary now.
## Obvious option is to take either {-1, 0} and {1} or {-1} and {0, 1}
## Depends on interest or "deviation". If you are more interested in studying "negative perceptions"
## should likely do {-1} vs {0, 1}. Vice-versa. Based on the above results, let's do that!
wallstreet_long$Y <- ifelse(wallstreet_long$Y == -1, 1, 0)

## Let's start with testing whether a 1st order Markov chain is reasonable, compared second order
WSL_2 <- create_lag(wallstreet_long, "id", "Y", nlag = 2)
WSL_2 <- WSL_2[complete.cases(WSL_2), ] # Drops all NA

model.order2 <- glm(Y ~ Y_lag1*Y_lag2, data = WSL_2, family=binomial(link="logit"))
model.order1 <- glm(Y ~ Y_lag1, data = WSL_2, family=binomial(link="logit"))

anova(model.order1, model.order2) 

# That may be the case, but what if we include covariates?
# We can throw a bunch of features in the model, let's fit a big one!

model.cov2 <- glm(Y ~ (age + political_affiliation + libcon + ownhouse + sex + income + highested + employment)*Y_lag1*Y_lag2, data = WSL_2, family=binomial(link="logit"))
model.cov1 <- glm(Y ~ (age + political_affiliation + libcon + ownhouse + sex + income + highested + employment)*Y_lag1, data = WSL_2, family=binomial(link="logit"))

## Still Reject H0! 

## Suppose that we are interested in:
#### Age, political_affiliation, and sex on the impacts
#### Also, suppose that we are only interested in an order 1 model.
model.reduced <- glm(Y ~ (age + political_affiliation + sex)*Y_lag1, data = WSL_2, family=binomial(link="logit"))

### Questions:
#### 1) Is there a sex difference between propensity to transition towards negative emotions?
####### That is, do males have different probabilities of moving positive [0] to negative, or negative [1] to negative?
#### 2) How does political party affiliation impact the transition probabilities, if at all?
#### 3) Can we drop age from the model?

### Answers:
# Q1:
summary(model.reduced)$coefficients['sex(1) Male:Y_lag1', ]
# No. This is not significant (p-value = 0.94) so sex does not appear to impact transition probabilities {p01 vs. p11}

# Q2:
summary(model.reduced)$coefficients[c(3:6, 10:13),]

# Can think of these as either relative rates, compared to democrats (baseline)
# Or relative rates within a particular individual.

# Yes. Republicans have a highly significant (p-value = {0.001258900, 0.005494832}) impact. 
# Coefficient is 1.0594591
# P{0->1 | Republican} = b0 + b1*age + b2 + b6*I(male) 
# P{1->1 | Republican} = b0 + b1*age + b2 + b6*I(male) + b7 + b8*age + b9 + b13*I(male)
# P{0->1 | Democrat} = b0 + b1*age + b6*I(male)
# P{1->1 | Deomcrat} = b0 + b1*age + b6*I(male) + b7 + b8*age + b13*I(male)

# b2 = log(ODDS) of transitioning {positive -> negative} of Republicans vs. Democrats
# exp(b2) = 0.3825368; CI = (0.2133324, 0.6859455)
## Republicans are 0.38 times as likely as democrats to have moved positive -> negative
# b2 + b9 = log(ODDS) of transitioning {negative -> negative} of Republicans vs. Democrats
# exp(b2+b7) = exp(-0.96093047 + 1.05945907) = exp(0.0985286) = 1.103546; CI = (0.6916135, 1.7608298)

# b9 = log(ODDS_RATIO) => exp(b9) = exp(1.059459) = 2.8841
# ODDS RATIO of [Republican vs. Deomcrat @ Negative Before] vs. [Republican vs. Democrat @ Positive Before]

# Build CI:
L <- c(0,0,1,0,0,0,0,0,0,1,0,0,0,0)
SE <- sqrt(t(L) %*% vcov(model.reduced) %*% L)
pe <- sum(coef(model.reduced)[c(3, 10)])
exp(c(pe - 1.96*SE, pe, pe + 1.96*SE))

model.no_age <- glm(Y ~ (political_affiliation + sex)*Y_lag1, data = WSL_2, family=binomial(link="logit"))
anova(model.reduced, model.no_age, test="Chisq") # Fail to reject H0
