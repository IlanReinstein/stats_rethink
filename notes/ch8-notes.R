library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)

dd <- d[complete.cases(d), ]

m8.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa, 
    a ~ dnorm(0, 100), 
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
precis(m8.1)

dd.trim <- dd[, c("log_gdp", "rugged", "cont_africa")]

m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa, 
    a ~ dnorm(0, 100), 
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)
  ),
  data = dd.trim
)

precis(m8.1stan)

m8.1stan_4chains <- map2stan(m8.1stan, chains = 4, cores = 4)
precis(m8.1stan_4chains)

post <- extract.samples(m8.1stan)
str(post)
pairs(m8.1stan)
