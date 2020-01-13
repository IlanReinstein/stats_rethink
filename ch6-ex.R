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
# - Makes no assumptions about the posterior\
# - pointwise