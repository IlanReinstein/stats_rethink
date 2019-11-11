library(rethinking)
data(chimpanzees)
d <- chimpanzees

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs(~ treatment + prosoc_left + condition, d)

m11.1 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ), 
  data = d
)

set.seed(1999)
m11.1 <- brms::brm(pulled_left ~ 1, family = "bernoulli", data=d, sample_prior = "only")
