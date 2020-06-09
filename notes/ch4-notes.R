# 4.1.1 Normal by addition
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))
# 4.1.2 Normal by multiplication
prod(1 + runif(12, 0, 0.1))

growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = T)

# Large fluctuations in multiplicative effects will not produce Gaussian distributions
big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))
dens(big, norm.comp = T)

# For sufficiently small fluctuations, the growth may be approximated by a sum -> producing Gaussian distribution
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))
dens(small, norm.comp = T)

# 4.1.3 Normal by Log-multiplication
# The log of large multiplicative fluctuations is additive -> Gaussian
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))
dens(log.big, norm.comp = T)

### 4.3 A Gaussian Model for Height

library(rethinking)
data("Howell1")
d <- Howell1

str(d)
precis(d)
d$height


d2 <- d[d$age >= 18, ]
dens(d2$height)

curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)


# Prior Sampling
sample_mu <- rnorm(1e4, 178, 100)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)



# 4.3.3 Grid approximation, height model

mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )
#Not working properly??
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# 4.3.4 sampling from posterior
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )


dens(sample.mu, adj = 1)
dens(sample.sigma)

HPDI(sample.mu)
HPDI(sample.sigma)


## Smaller sample of data
d3 <- sample(d2$height, size = 20)

mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i) sum( dnorm(
  d3,
  mean=post2$mu[i] ,
  sd=post2$sigma[i] ,
  log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                       prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]

plot( sample2.mu , sample2.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens(sample2.sigma, norm.comp = T)


# 4.3.5 Fitting with MAP
data("Howell1")
d <- Howell1

d2 <- d[d$age >= 18, ]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- map(flist, data = d2)
precis(m4.1)

m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1), 
    sigma ~ dunif(0, 50)
  ), data = d2
)
precis(m4.2)


# 4.3.6 Sampling form a MAP fit

#Variance
diag(vcov(m4.1))
#Correlation matrix
cov2cor(vcov(m4.1))

post <- extract.samples(m4.1, n = 1e4)
head(post)

### 4.4 Adding Predictor

m4.3 <- map(
  alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*weight,
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0,50)
), data = d2)


precis(m4.3, corr = T)

#centering
d2$weight.c <- d2$weight - mean(d2$weight)

m4.4 <- map(
  alist(
    height ~ dnorm(a + b*weight.c, sigma),
    a ~ dnorm(178, 100), 
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

precis(m4.4, corr = T)

plot(height ~ weight, data = d2)
abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"])

post <- extract.samples(m4.3)
post[1:5,]


## Plotting and modeling first 10 cases in d2

N <- 10
dN <- d2[1:N,]

mN <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = dN  )

# extract 20 samples from posterior
post <- extract.samples(mN, n = 20)

# display raw data and sample size
plot(dN$weight, dN$height, xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2, xlab = "weight", ylab = "height")
mtext(concat("N = ", N))

#plot lines, with transparency

for (i in 1:20)
  abline(a = post$a[i], b = post$b[i], col = col.alpha("black", 0.3))


## Plotting uncertainty

mu_at_50 <- post$a + post$b * 50
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weigth=50")

HPDI(mu_at_50, prob = 0.89)


mu <- link(m4.3)
str(mu)


# define seq of weights to compute predictions for
# these values will be on horizontal axis
weight.seq <- seq(from=25, to =  70, by = 1)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
str(mu)

# use type = "n" to hide raw data
plot(height ~ weight, d2, type = "n")

# loop over samples and plot each mu value
for (i in 1:100)
  points(weight.seq, mu[i, ], pch = 16, col = col.alpha(rangi2, 0.1))

# summarize distribution of mu

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
mu.mean
mu.HPDI

# plot raw data
# fading out points to make line and interval more visible
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))

# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)

# plot a shaded region for 89% HPDI
shade(mu.HPDI, weight.seq)

# Prediction intervals
sim.height <- sim(m4.3, data = list(weight = weight.seq))
str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob = 0.89)

#plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi, 0.5))

# draw MAP line
lines(weight.seq, mu.mean)

#draw HPDI region for line
shade(mu.HPDI, weight.seq)

# draw PI region for simulated heights
shade(height.PI, weight.seq)


###############

## 4.5 Polynomial regression

data("Howell1")
d <- Howell1

plot(height ~ weight, data = d)

#standardize weight
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)

plot(height ~ weight.s, data = d)

d$weight.s2 <- d$weight.s^2

m4.5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d
)

precis(m4.5)


weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight.s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# cubic model
d$weight.s3 <- d$weight^3

m4.6 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d
)

plot(height ~ weight.s, d, col = col.alpha(rangi2, 0.5), xaxt  = "n")
at <- c(-2,-1,0, 1, 2)
labels <- at*sd(d$weight) + mean(d$weight)
axis(side=1, at = at, labels = round(labels, 1))
