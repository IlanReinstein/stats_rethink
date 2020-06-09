library(rethinking)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)

# 3E1
sum(samples < 0.2)/1e4
sum(posterior[p_grid < 0.2])
#3E2
sum(samples < 0.8)/1e4
sum(posterior[p_grid < 0.8])
#3E3
sum(samples > 0.2 & samples < 0.8)/1e4
sum(posterior[p_grid > 0.2 & p_grid < 0.8 ])
#3E4
quantile(samples, 0.2)
#3E5
1 - quantile(samples, 0.2)
#3E6
HPDI(samples, 0.66)
#3E7
PI(samples, 0.66)
### Medium
#3M1
likelihood.2 <- dbinom(8, 15, prob = p_grid)
posterior.2 <- likelihood.2*prior
posterior.2 <- posterior.2/sum(posterior.2)

#3M2
samples.2 <- sample(p_grid, prob = posterior.2, size = 1e4, replace = T)
HPDI(samples.2, 0.9)

#3M3
w <- rbinom(1e4, size = 15, prob = samples.2)
mean(w == 8)

#3M4
w2 <- rbinom(1e4, size = 9, prob = samples.2)
mean(w2 == 6)
simplehist(w2)

#3M5
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior.2 <- ifelse(p_grid > 0.5, 1, 0)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)


#The 90% HPDI is wider with the nonuniform better prior
HPDI(samples, 0.9)

ppc <- rbinom(1e4, size = 15, prob = samples)
mean(ppc == 8)

simplehist(ppc)
# The probability of obtainig an 8 is higher than with th uniform prior
# since our prior belief on the proportion of water on earth is higher than 50%

ppc.2 <- rbinom(1e4, size = 9, prob = samples)
mean(ppc.2 == 6)

#The same behavior goes for 6 tosses of the globe

true_ppc <- rbinom(1e4, size = 15, prob = 0.7)
mean(true_ppc == 8)


## 3H1
rm(list = ls())
data(homeworkch3)

#Total number of boys
boys.total <- sum(birth1) + sum(birth2)
birth.total <- length(birth1) + length(birth2)
trials <- 1e4


p_grid <- seq(0,1,length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(boys.total, birth.total, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)

# MAP
p_grid[which.max(posterior)]

##3H2
samples <- sample(p_grid, prob = posterior, size = trials, replace = T)
HPDI(samples, 0.5)
HPDI(samples, 0.89)
HPDI(samples, 0.97)


#3H3
ppc.1 <- rbinom(trials, size = birth.total, prob = samples)
dens(ppc.1)
abline(v = boys.total)

#3H4
b1.boys <- sum(birth1)
b1.total <- length(birth1)
ppc.2 <- rbinom(trials, size = b1.total, prob = samples)
dens(ppc.2)
abline(v = b1.boys)
# The model does not fit the data very well in this case

#3H5
b2.boys <- birth2[birth1 == 0]
ppc.3 <- rbinom(trials, size = length(b2.boys), prob = samples)
dens(ppc.3)
abline(v = sum(b2.boys))
# The model is underetimating the number of boys in second birth.
# perhaps the assumption of sex independence between births is incorrect 
# or it needs to be encoded into the model

