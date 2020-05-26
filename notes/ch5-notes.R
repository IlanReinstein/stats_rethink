library(rethinking)

rm(list = ls())
data("WaffleDivorce")
d <- WaffleDivorce

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

# fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

# compute precentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out = 30)
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s = MAM.seq))
mu.PI <- apply(mu, 2, PI)

#plot it all
plot(Divorce ~ MedianAgeMarriage.s, data = d, col = rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

precis(m5.1)

# model on marriage rate
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)

m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

precis(m5.2)

# Multivariate model
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b1*Marriage.s + b2*MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.3)

plot(precis(m5.3))

# 5.1.3.1 predictor residual plots

m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

# compute expected value at MAP, for each State
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# compute residual for each state
m.resid <- d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, d, col = rangi2)
abline(m5.4)
for(i in 1:length(m.resid)){
  x <- d$MedianAgeMarriage.s[i] # x location of line segment
  y <- d$Marriage.s[i] # observed enbpoint of line segment
  # draw the line segment
  lines(c(x, x), c(mu[i], y), lwd = 0.5, col = col.alpha("black", 0.7))
}

# 5.1.3.2 counterfactual plots
# prepare new counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(
  Marriage.s = R.seq,
  MedianAgeMarriage.s = A.avg
)

# compute counterfactual mean divorce (mu)
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

#simulate counterfactual divorce outcomess
R.sim <- sim(m5.3, data = pred.data, n = 1e4)

R.PI <- apply(R.sim, 2, PI)

#display predictions, hiding raw data
plot(Divorce ~ Marriage.s, data = d, type = "n")
mtext("MedianAgeMarriage.s = 0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)

# constant marriage rate
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3.5, length.out = 30)
pred.data2 <- data.frame(
  Marriage.s = R.avg,
  MedianAgeMarriage.s = A.seq
)

mu <- link(m5.3, data = pred.data2)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

A.sim <- sim(m5.3, data = pred.data2, n = 1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, type = "n")
mtext("Marriage.s= 0")
lines(A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)

# 5.1.3.3 posterior prediction plots

# call link without specifying new data
mu <- link(m5.3)

#summairze samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate observations
# no new data
divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

# predicted vs observed
plot(mu.mean ~ d$Divorce, col = rangi2, ylim = range(mu.PI),
     xlab = "Observed divorce", ylab = "predicted divorce")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(d)){
  lines(rep(d$Divorce[i],2), c(mu.PI[1, i], mu.PI[2, i]), col = rangi2)
}
identify(x = d$Divorce, y = mu.mean, labels = d$Loc, cex = 0.8)

# compute residuals
divorce.resid <- d$Divorce - mu.mean

# get ordering by divorce rate
o <- order(divorce.resid)

#make the plot
dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6, 5), cex = 0.6)
abline(v = 0, col = col.alpha("black", 0.2))
for(i in 1:nrow(d)){
  j <- o[i]
  lines(d$Divorce[j] - c(mu.PI[1, j], mu.PI[2, j]), rep(i, 2))
  points(d$Divorce[j] - c(divorce.PI[1, j], divorce.PI[2, j]), rep(i, 2),
         pch = 3, cex = 0.6, col = "gray")
}


# 5.2 Masked relationship
data("milk")
d <- milk
str(d)

dcc <- d[complete.cases(d), ]

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1), 
    sigma ~ dunif(0, 1)
  ), data = dcc
)
precis(m5.5, digits = 3)

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc = np.seq)

mu <- link(m5.5, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col = rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)


# With mass as predictor
dcc$log.mass <- log(dcc$mass)

m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + b*log.mass, 
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc
)

precis(m5.6)

# mulivariate model

m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b1*neocortex.perc + b2*log.mass,
    a ~ dnorm(0,100),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data = dcc
)

precis(m5.7)

mean.log.mass <- mean(dcc$log.mass)
np.seq <- 1:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, type = "n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)

mean.neocortex <- mean(dcc$neocortex.perc)
lm.seq <- seq(from = -2, to = 4.5, length.out = 100)
pred.data <- data.frame(
  neocortex.perc = mean.neocortex,
  log.mass = lm.seq
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, type = "n")
lines(lm.seq, mu.mean)
lines(lm.seq, mu.PI[1,], lty = 2)
lines(lm.seq, mu.PI[2,], lty = 2)

# 5.3 When adding variable hurts
N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop*height + rnorm(N, 0, 0.02)
leg_right <- leg_prop*height + rnorm(N, 0, 0.02)

d <- data.frame(height, leg_left, leg_right)

m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100), 
    bl ~ dnorm(2, 10), 
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)

precis(m5.8)
plot(precis(m5.8))

post <- extract.samples(m5.8)
plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

sum_blbr <- post$bl + post$br
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "sum pf bl and br")

#5.3.2 multicollinear milk
data("milk")
d <- milk

# kcal.per.g on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + bf*perc.fat, 
    a ~ dnorm(0.6, 10), 
    bf ~ dnorm(0, 1), 
    sigma ~ dunif(0, 10)
  ), data = d
)

#kcal.per.g on per.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose,
    a ~ dnorm(0.6, 10), 
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.10, digits = 3)
precis(m5.11, digits = 3)

m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose + bf*perc.fat,
    a ~ dnorm(0.6, 10), 
    bl ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.12, digits = 3)

pairs(~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)

# 5.3.3 post-treatment bias
N <- 100

h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)
d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)

m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma), 
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0, 100),
    c(bh, bt, bf) ~ dnorm(0, 10), 
    sigma ~ dunif(0, 10)
  ), data = d
)

precis(m5.13)


m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma), 
    mu <- a + bh*h0 + bt*treatment ,
    a ~ dnorm(0, 100),
    c(bh, bt) ~ dnorm(0, 10), 
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.14)

# 5.4 Categorical variables
data(Howell1)
d <- Howell1

m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data = d
)
precis(m5.15)

post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

m5.15b <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af*(1 - male) + am*male,
    af ~ dnorm(178, 100),
    am ~ dnorm(178, 100),
    sigma ~ dunif(0, 50)
  ), data = d
)

precis(m5.15b)

#5.4.2 Many categories
data(milk)
d <- milk
str(d)
unique(d$clade)

(d$clade.NWM <- ifelse(d$clade == "New World Monkey", 1, 0))
d$clade.OWM <- ifelse(d$clade == "Old World Monkey", 1, 0)
d$clade.S <- ifelse(d$clade == "Strepsirrhine", 1, 0)


m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), 
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
    a ~ dnorm(0.6, 10),
    c(b.NWM, b.OWM, b.S) ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.16)

# sample posterior
post <- extract.samples(m5.16)

# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))

# difference bewtwen two groups
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))

d$clade_id <- coerce_index(d$clade)
d$clade_id

m5.16_alt <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.16_alt, depth = 2)
