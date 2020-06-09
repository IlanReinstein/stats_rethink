# 3.1 Sampling from a Grid-approximate posterior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

plot(samples)

library(rethinking)
dens(samples)


# 3.2 Sampling to summarize
# 3.2.1 Intervals of defined boundaries
sum(posterior[p_grid < 0.5])

sum(samples < 0.5) / 1e4

sum(samples > 0.5 & samples < 0.75) / 1e4

# 3.2.2 Intervals of defined mass
# range of parameter values compatible with model and data
quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))

# Percentile Intervals (PI): asign equal mass to the each tail
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- prior * likelihood
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = T, prob = posterior)

PI(samples, 0.5)

# Highest Posterior Density Interval (HPDI)
HPDI(samples, 0.5)

# Point Estimates
# MAP (maximum a posteriori estimate)
p_grid[which.max(posterior)]

chainmode(samples, adj = 0.01)
mean(samples)
median(samples)

# loss functions
sum(posterior * abs(0.5 - p_grid))

loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]

# 3.3 Sampling to simulate predictions
# Model design: samples from prior, no data
# Model checking: after data
# Software validation
# Research design: 
# Forecasting

# 3.3.1 Dummy data
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

# Is the model adequate? PPD - posterior predictive distribution
# Average of all possible outcome distributions across all the parameter values
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
