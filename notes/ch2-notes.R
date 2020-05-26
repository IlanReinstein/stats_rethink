library(rethinking)
# Grid Approximation
p_grid <- seq(0, 1, length.out = 20)
prior <- rep(1, 20)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")
# Quadratix Approximation

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p),
    p ~ dunif(0, 1)
  ),
  data = list(W=6, L = 3)
)
precis(globe.qa)
parallel::detectCores()
