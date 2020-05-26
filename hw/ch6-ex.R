library(rethinking)

# 6E1 
# Information entropy is used because:
#  - It is continuous
# - It tends to increase with the number of possible events
# - The total uncertainty of a combination of events
#   is the sum of the uncertainties of each event separately.

#6E2
# Entropy of coin
p.heads <- 0.7
p.tails <- 1 - p.heads
-sum(c(p.heads, p.tails)*log(c(p.heads, p.tails)))
# H(p) = 0.61

#6E3
# Entropy of 4-side die
p.die <- c(0.2, 0.25, 0.25, 0.3)
-sum(p.die * log(p.die))

#6E4
p.die.2 <- p.die[1:3]
-sum(p.die.2 * log(p.die.2))

#6M1
#Deviance D(q) = -2*sum(log(q)), q: modeled probs

# Information Criteria: Estimate Out-sample deviance
# AIC: D_train (In-sample deviance) + 2p, p: # of parameters
# most appropriate under the assumptions:
# - priors are flat
# - posterior ~ Gaussian
# - Sample size N >> k (No. of parameters)

# DIC:
# - Accomodates informative priors, but requires large sample size
#   and Gaussian posterior

# WAIC:
# - Makes no assumptions about the posterior
# - pointwise
# two parts 
#   - lppd = Sum(log(Avg. likelihood)): Log-pointwise-pred-dens
#   - p_waic = Sum(variance(log-lik)): Effective num of paramters
# WAIC = -2(lppd - p_waic)

#6M2
# Model Selection: Using Information Criteria to select the best model, discard others.
# Model Averaging: Computes an ensemble of posterior predictions using all models
# Info lost:
# - MS: No information about relative accuracy of models
# - MA: Larger impact on uncertainty

#6M3
# Because WAIC is computed at the single observation level
# having additional of fewer observations in different models will indeed lead 
# to different WAIC estimates. Also, since the SE of WAIC depends on the 
# sample size, then smaller dataset will have better deviance (page 196 1ed).
# Information Criteria estimates will not be on the same scale

#6M4
data(cars)

m <- map(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b*speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 30)
  ), data = cars
)
post <- extract.samples(m, n = 1000)
n_samples <- 1000
ll <- sapply(1:n_samples,
             function(s) {
               mu <- post$a[s] + post$b[s]*cars$speed
               dnorm(cars$dist, mu, post$sigma[s], log = T)
             })

n_cases <- nrow(cars)
sum(sapply(1:n_cases, function(i) var(ll[i,])))


# After trying b ~ dnorm(0, 1) and dnorm(0, 10)
# the effective number of parameters (as in WAIC)
# do not change and remain approx 4 for the model
# above.
# Point-wise calculation? Log likelihood? 
# Estimate Conditioned on Training Data, which is independent of prior choice?\

#6M5, 6M6
# Informative prior restrict the possible parameter values
# to a well-defined region, therefore penalizing those values
# that are too extreme or not plausible upon seeing the data.
# Too informative prior will not allow the model to learn 
# from the data and will ve restricted to a very narrow region 
# on the parameter space.

#6H1
library(rethinking)
data("Howell1")
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed(1000)
i <- sample(1:nrow(d), size = nrow(d)/2)
d1 <- d[i, ]
d2 <- d[-i, ]

m1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*age,
    a ~ dnorm(140, 80),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)
m2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2,
    a ~ dnorm(140, 80),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)
m3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2 + b3*age^3,
    a ~ dnorm(140, 80),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)
m4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4,
    a ~ dnorm(140, 80),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    b4 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)
m5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5,
    a ~ dnorm(140, 80),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    b4 ~ dnorm(0, 10),
    b5 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)

m6 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
    a ~ dnorm(140, 80),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    b4 ~ dnorm(0, 10),
    b5 ~ dnorm(0, 10),
    b6 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d1
)

#6H2
age.seq <- seq(-2, 3, length.out = nrow(d1))

pred <- link(m6, data = data.frame(age = age.seq))
mu <- apply(pred, 2, mean)
mu.PI <- apply(pred, 2, PI, c(0.97)) 

plot(d1$age, d1$height)
lines(age.seq, mu, lty = 2)
shade(mu.PI, age.seq)

#6H3
age.ensemble <- ensemble(m1, m2, m3, m4, m5, m6, data = data.frame(age = age.seq))
mu <- apply(age.ensemble$link, 2, mean)
mu.PI <- apply(age.ensemble$link, 2, PI, 0.97)
plot(d1$age, d1$height)
lines(age.seq, mu, lty = 2)
shade(mu.PI, age.seq)

#6H4
# Test Sample Deviance
# log-lik
d2$age.s <- (d2$age - mean(d2$age))/d2$age

compute.dev <- function(model, n){
  age.tilde <- poly(d2$age, n, raw = T)
  X.tilde <- cbind(rep(1, nrow(d2)), age.tilde)
  theta <- coef(model)
  beta <- theta[-length(theta)]
  sigma <- theta[length(theta)]
  dev <- -2*sum(dnorm(d2$height, mean = X.tilde %*% beta, sd = sigma, log=T))
  return(dev)
}



full.dev <- list("m1" = compute.dev(m1,1), "m2" = compute.dev(m2,2), "m3" = compute.dev(m3, 3),
              "m4" = compute.dev(m4, 4), "m5" = compute.dev(m5, 5), "m6" = compute.dev(m6, 6))
full.dev <- data.frame("dev" = as.matrix(full.dev))
full.dev
#6H5
full.waic <- compare(m1, m2, m3, m4, m5, m6) 
full.waic

# Using weakly regularizing priors (very wide, dnorm(0, 10)) the 
# deviance computation does not seem to agree with the
# WAIC value for each model. When using more strongly informative priors (dnorm(0,1))
# The story is different in the sense that both computations of out of sample
# deviance agree

# 6H6
# the above exercise was done with two different sets of priors
m6.1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
    a ~ dnorm(0, 100),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    b3 ~ dnorm(0, 1),
    b4 ~ dnorm(0, 1),
    b5 ~ dnorm(0, 1),
    b6 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = d1
)

precis(m6)

age.seq <- seq(-2, 3, length.out = nrow(d1))

pred <- link(m6.1, data = data.frame(age = age.seq))
mu <- apply(pred, 2, mean)
mu.PI <- apply(pred, 2, PI, c(0.97)) 

plot(d1$age, d1$height)
lines(age.seq, mu, lty = 2)
shade(mu.PI, age.seq)

compute.dev(m6.1, 6)
full.waic

