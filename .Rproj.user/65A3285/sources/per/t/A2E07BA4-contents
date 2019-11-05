#5E1
# 2, 4

#5E2
# ad ~ norm(mu, sigma)
# mu[i] ~ a + bl*Lat[i] + bP*P[i]

#5E3
#t[i] ~ norm(mu, sigma)
#mu[i] = a + b1*f[i] + b2*s[i]
# bl > 0, bs > 0

# 5M1
library(rethinking)
rm(list= ls())
data("WaffleDivorce")
dat <- WaffleDivorce

# 5M4
lds_pop <- read.csv("lds_population.tsv", sep="\t", header = F)
lds_pop$n_lds <- as.numeric(gsub("%", "", lds_pop$V5))
lds_pop$Location <- lds_pop$V2

dat <- merge(dat, lds_pop[,c("Location", "n_lds")])
dat$log_lds <- log(dat$n_lds)
dat$zPropLDS <- (dat$n_lds - mean(dat$n_lds))/sd(dat$n_lds)
dat$zLogLDS <- (dat$zPropLDS - mean(dat$zPropLDS))/sd(dat$zPropLDS)

dat$MedianAgeMarriage.s <- (dat$MedianAgeMarriage - mean(dat$MedianAgeMarriage))/
  sd(dat$MedianAgeMarriage)
dat$Marriage.s <- (dat$Marriage - mean(dat$Marriage))/sd(dat$Marriage)

mlds <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bM*Marriage.s + bA*MedianAgeMarriage.s + bL*dat$log_lds,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    bM ~ dnorm(0, 1),
    bL ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dat
)
precis(mlds)

# 5M5
#See notebook

# 5H1
data(foxes)
dat <- foxes
dat

# Weight vs Area
m1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)

# compute precentile interval of mean
area.seq <- seq(from = 0, to = 6, length.out = 30)
mu <- link(m1, data = data.frame(area= area.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(weight ~ area, data = dat, col = rangi2)
lines(area.seq, mu.mean)
shade(mu.PI, area.seq)

# Weight vs groupsize
m2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bG*groupsize,
    a ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)

# compute precentile interval of mean
gs.seq <- seq(from = 0, to = 10, length.out = nrow(dat))
mu <- link(m2, data = data.frame(groupsize = gs.seq))
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(weight ~ groupsize, data = dat, col = rangi2)
abline(m2)
shade(mu.PI, gs.seq)

# 5H2
# Do foxes that move in greater areas have higher body weight GIVEN their groupsize?
# Do foxes that belong to bigger groups have higher body weight GIVEN the area size in which they move?
m3 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area + bG*groupsize,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)

# Holding Groupsize constant
gs.mean <- mean(dat$groupsize)
area.seq <- seq(from = 0, to = 6, length.out = nrow(dat))
pred.dat <- data.frame(area = area.seq, groupsize = gs.mean)

mu <- link(m3, data = pred.dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(weight ~ area, data = dat)
mtext("Avg. Group Size")
lines(area.seq, mu.mean)
shade(mu.PI, area.seq)

# holding area constant
area.mean <- mean(dat$area)
gs.seq <- seq(from = 0, to = 10, length.out = nrow(dat))
pred.dat <- data.frame(area = area.mean, groupsize = gs.seq)

mu <- link(m3, data = pred.dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(weight ~ groupsize, data = dat)
mtext("Avg. Area size")
lines(gs.seq, mu.mean)
shade(mu.PI, gs.seq)

# as we can see from the pairs plot
# groupsize and area are positively and strongly correlated
# but none is strongly correlated with the outcome. 
# this corresponds to a mased relationship.
# There is no apparent relationship in bivariate regressions
# One of the predictors is + corr with the outcome while the other is -


# 5H3
m4.1 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a +  bG*groupsize + bF*avgfood,
    a ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bF ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)

precis(m4.1)
# the effect of groupsize has increased with respect to prevoius models since it
# is possitively and strongly correalted with the Average Food intake

# holding groupsize constant
f.seq <- seq(from = min(dat$avgfood), to = max(dat$avgfood), length.out = nrow(dat))
gs.mean <- mean(dat$groupsize)
pred.dat <- data.frame(avgfood = f.seq, groupsize = gs.mean)
mu <- link(m4.1, data = pred.dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(weight ~ avgfood, data = dat)
lines(f.seq, mu.mean)
shade(mu.PI, f.seq)


# holding Average food at mean
food.mean <- mean(dat$avgfood)
gs.seq <- seq(from = min(dat$groupsize) - 1, to = max(dat$groupsize) + 1, length.out = nrow(dat))
pred.dat <- data.frame(avgfood = food.mean, groupsize = gs.seq)

mu <- link(m4.1, data = pred.dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


plot(weight ~ groupsize, data = dat)
lines(gs.seq, mu.mean)
shade(mu.PI, gs.seq)


# The effect of groupsize is clearly stronger when conbined with average food
# The effect of average food is very wide and uncertain.


m4.2 <- map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bA*area + bG*groupsize + bF*avgfood,
    a ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bG ~ dnorm(0, 10),
    bF ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dat
)
plot(precis(m4.2))
plot(coeftab(m1,m2, m3, m4.1, m4.2))
