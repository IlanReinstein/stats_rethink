# 2M1
# W, W, W
p_grid <- seq(0, 1, length.out = 20)
prior <- rep(1, 20)
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")

# W, W, W, L
likelihood <- dbinom(3, size = 4, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")

# L, W, W, L, W, W, W
likelihood <- dbinom(5, size = 7, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")

#2M2
prior <- ifelse(p_grid < 0.5, 0, 1)
# W, W, W
likelihood <- dbinom(3, size = 3, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
# W, W, W, L
likelihood <- dbinom(3, size = 4, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")
# L, W, W, L, W, W, W
likelihood <- dbinom(5, size = 7, prob = p_grid)
unstd.post <- likelihood * prior
posterior <- unstd.post/sum(unstd.post)

plot(p_grid, posterior, type = "b", 
     xlab = "probability of water", ylab = "posterior probability")

#2M3
# Via Bayes rule
p_earth <- 0.5
p_mars <- 0.5
p_w_earth <- 0.7
p_l_earth <- 1 - p_w_earth
p_l_mars <- 1.0

p_earth_w <- (p_l_earth * p_earth) / (p_l_earth*p_earth + p_l_mars*p_mars)
p_earth_w

#2M4

#