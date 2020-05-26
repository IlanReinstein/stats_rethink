library(rethinking)

data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)

dd <- d[complete.cases(d$rgdppc_2000), ]

d.A1 <- dd[dd$cont_africa==1, ]
d.A0 <- dd[dd$cont_africa==0, ]

# African Nations
m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100), 
    bR ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ),
  data = d.A1
)

rug.seq <- seq(0, 10, length.out = nrow(d.A1))
mu <- link(m7.1, data = data.frame(rugged = rug.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(d.A1$rugged, d.A1$log_gdp)
lines(rug.seq, mu.mean)
shade(mu.PI, rug.seq)
#Non-African Nations
m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100), 
    bR ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ),
  data = d.A0
)

# Notes on why splitting data fails
# 1- Parameters that do not depend on whether the country belongs to Africa or not
# Assuming difference variance between countries
# 2- Need to include splitting variable in model.
# Account for uncertainty in prediction when splitting
# 3- We need to be able to compare models i.e., fit on the same data.
# 4- borrowing strength advantages


rug.seq <- seq(0, 10, length.out = nrow(d.A0))
mu <- link(m7.2, data = data.frame(rugged = rug.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(d.A0$rugged, d.A0$log_gdp)
lines(rug.seq, mu.mean)
shade(mu.PI, rug.seq)

## 7.1.1
m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100), 
    bR ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ),
  data = dd
)

m7.4 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm(8, 100), 
    bR ~ dnorm(0, 1), 
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
compare(m7.3, m7.4)



rug.seq <- seq(-1, 8, by = 0.25)
mu.nA <- link(m7.4, data = data.frame(rugged = rug.seq, cont_africa = 0))
mu.A <- link(m7.4, data = data.frame(rugged = rug.seq, cont_africa = 1))
mu.nA.mean <- apply(mu.nA, 2, mean)
mu.A.mean <- apply(mu.A, 2, mean)
mu.nA.PI <- apply(mu.nA, 2, PI)
mu.A.PI <- apply(mu.A, 2, PI)

# Fig 7.3
plot(dd$rugged, dd$log_gdp)
lines(rug.seq, mu.A.mean, col = "blue")
lines(rug.seq, mu.nA.mean)
shade(mu.A.PI, rug.seq, col = rgb(0, 0, 1, alpha = 0.3))
shade(mu.nA.PI, rug.seq, col = rgb(0, 0, 0, alpha = 0.3))

# 7.1.2
m7.5 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma*rugged + bA*cont_africa,
    gamma <- bR + bAR*cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)

compare(m7.3, m7.4, m7.5)


rug.seq <- seq(-1, 8, by = 0.25)
mu.nA <- link(m7.5, data = data.frame(rugged = rug.seq, cont_africa = 0))
mu.A <- link(m7.5, data = data.frame(rugged = rug.seq, cont_africa = 1))
mu.nA.mean <- apply(mu.nA, 2, mean)
mu.A.mean <- apply(mu.A, 2, mean)
mu.nA.PI <- apply(mu.nA, 2, PI)
mu.A.PI <- apply(mu.A, 2, PI)

# Fig 7.3
plot(log(rgdppc_2000) ~ rugged, data = d.A1,
     col=rangi2, ylab ="log GDP year 2000",
     xlab = "Terrain Ruggeness Index")
mtext("African Nations", 3)
lines(rug.seq, mu.A.mean, col = rangi2)
shade(mu.A.PI, rug.seq, col = col.alpha(rangi2, 0.3))

plot(log(rgdppc_2000) ~ rugged, data = d.A0,
     col="black", ylab ="log GDP year 2000",
     xlab = "Terrain Ruggeness Index")
mtext("Non-African Nations", 3)
lines(rug.seq, mu.nA.mean)
shade(mu.nA.PI, rug.seq)

# Difficulties for interpreting interaction effects
# - Parameters Change Meaning: Interactions depend on more than one
# parameter for interpretation. We need to account for these to understand 
# a change in the covariates
# - Incorporating Uncertainty: Since gamma depends on parameters, 
# and parameters have a posterior, then gamma also has ap posterior

post <- extract.samples(m7.5)
gamma.Africa <- post$bR + post$bAR*1
gamma.notAfrica <- post$bR + post$bAR*0

# Distribution of slopes in and outside Africa
dens(gamma.Africa, xlim = c(-0.5, 0.6), ylim = c(0, 5.5),
     xlab = "gamma", col = rangi2)
dens(gamma.notAfrica, add = T)

# Prob that slope in Africa < slope notAfrica
diff <- gamma.Africa - gamma.notAfrica
sum(diff < 0)/length(diff)
