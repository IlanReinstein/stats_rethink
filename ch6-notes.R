library(rethinking)

sppnames <- c("afarensis", "africanus", "habilis", "boisei", "rudolfensis",
              "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61, 53.5)
d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)

# Simplest model possible
m6.1 <- lm(brain ~ mass, data=d)

# compute R2 
1 - var(resid(m6.1))/var(d$brain)
summary(m6.1)

m6.2 <- lm(brain ~ mass + I(mass^2), data=d)

# Deviances
# compute deviance by cheating
(-2)**logLik(m6.1)