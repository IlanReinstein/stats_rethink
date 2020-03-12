## Chapter 4 Excersices
rm(list = ls())
# 4M1
# prior simulations
nsim <- 1e4
sample_mu <- rnorm(nsim, 0, 10)
sample_sigma <- runif(nsim, 0, 10)
prior_y <- rnorm(nsim, sample_mu, sample_sigma)
prior_y


#4M2

flist <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dunif(0, 10)
)

# Hard

#4H1
data("Howell1")
d <- Howell1

#standardize weight
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)

mh1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    sigma ~ dunif(0, 64)
  ), data = d
)

new.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
new.weights.s <- (new.weights - mean(d$weight))/sd(d$weight)
pred_dat <- list(weight.s= new.weights.s)
mu <- link(mh1, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(mh1, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)


#4H2
# a)
d2 <- d[d$age < 18,]
d2$weight.n <- (d2$weight - mean(d2$weight))/10 #(d2$weight - mean(d2$weight))/sd(d2$weight)
mh2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.n,
    a ~ dnorm(150, 100),
    b ~ dnorm(0, 20),
    sigma ~ dunif(0, 64)
  )  , data = d2
)

precis(mh2)

# We have centered and scaled the weight variable so that a one
# unit change corresponds to a 10-unit change on natural scale.
# According to the output of our model, a 10 unit increase above 
# the average weight reprsents a 27.2 cm increase in height.
# For young people (<18 yo), this is expected given the strong correaltion between height and weight
# but a major change in weight may also represent an unknown physical condition that does not incur in 
# increase in height. No such cases seem to be present in the data

# b)
weight.seq <- seq(from = -2, to = 3, length.out = 50)
pred_dat <- list(weight.n = weight.seq)
mu <- link(mh2, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(mh2, data = pred_dat)
height.HPDI <- apply(sim.height, 2, HPDI, prob = 0.89)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)


plot(height ~ weight.n, data = d2)
# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)
# plot a shaded region for 89% HPDI
shade(mu.HPDI, weight.seq)
# plot PI for height
shade(height.HPDI, weight.seq)

# c)
# low and high values of weight are not properly captured by this model, 
# nor is the polynomial/nonlinear trend in the data.
# A better moder would include the exponential relations between height and weight,
# as a common growth model, assuming height stops increasing after certain age, regardless of the weight

# 4H3
# a)
d$log_weight <- log(d$weight)

mh3 <- map(
  alist(
    height ~ dnorm(mu, sigma), 
    mu <- a + b*log_weight,
    a ~ dnorm(178, 100), 
    b ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ), data = d
)

precis(mh3)

# b)


weight.seq <- seq(from=5, to =  70, by = 1)

pred_dat <- list(log_weight = log(weight.seq))
mu <- link(mh3, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.97)
sim.height <- sim(mh3, data = pred_dat)
height.HPDI <- apply(sim.height, 2, HPDI, prob = 0.97)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.97)


plot(height ~ weight, data = d, col = col.alpha(rangi2, 0.4))
# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)
# plot a shaded region for 89% HPDI
shade(mu.HPDI, weight.seq)
# plot PI for height
shade(height.HPDI, weight.seq)
