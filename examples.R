library(dlm)
set.seed(1)

# unknown position at sea example
## model
## Y_t = \theta + \epsilon_t
## \epsilon_t \sim N(0, \sigma_2=0.5)

## known variance
sigma <- sqrt(0.5)

## priors \theta \sim N(m_0, C_0)
m <- c(1, NA, NA)
C <- c(2, NA, NA)

## observations
Y <- c(NA, 1.3, 1.2)

for (i in seq(2, length(Y))) {
    m[i] <- m[i - 1] + C[i - 1] * (Y[i] - m[i - 1]) /
            (C[i - 1] + sigma ^ 2)
    C[i] <- (sigma ^ 2) * C[i-1] /
            (sigma ^ 2 + C[i-1])
}

## now do the same but with generated data
n <- 100
epsilon.t <- rnorm(n, 0, sigma)
theta <- 1.17
Y.t <- theta + epsilon.t


## set up recursive algorithm again
Y <- c(NA, Y.t)
m <- numeric(n+1)
C <- numeric(n+1)
m[1] <- 1
C[1] <- 2

for (i in seq(2, length(Y))) {
    m[i] <- m[i - 1] + C[i - 1] * (Y[i] - m[i - 1]) /
            (C[i - 1] + sigma ^ 2)
    C[i] <- (sigma ^ 2) * C[i-1] /
            (sigma ^ 2 + C[i-1])
}

# simulate random walk with noise
## y_t = \theta_t + v_t, v_t \sim N(0, V)
## \theta_t = \theta_{t-1} + w_t, w_t \sim N(0, W)
v <- 4.5
sigma.w <- sqrt(0.9)
w.t <- rnorm(n, 0, sigma.w)
#theta.t <- 
epsilon.t <- rnorm(n, 0, sigma)
v.t <- rnorm(n, 0, 2)
w.t <- rnorm(n, 0, 1)

# allocate memory
theta.t <- numeric(n)
y.t <- numeric(n)

# set initial value for theta and y
theta.t[1] <- 1.17
for (i in 2:n) {
    theta.t[i] <- theta.t[i-1] + v + w.t[i]
}

y.t <- theta.t + epsilon.t

# simulate (2.6)
set.seed(1)
n <- 100
sigma.b <- 1.17
sigma.u <- 1.3
V <- 12.6
beta <- c(1.5, rep(0, n-1))
mu <- c(14.3, rep(0, n-1))

w.t.1 <- rnorm(n, 0, sigma.u)
w.t.2 <- rnorm(n, 0, sigma.b)
v.t <- rnorm(n, 0, sqrt(V))

Y.t <- c(mu + v.t)

for (i in 2:n) {
    beta[i] <- beta[i - 1] + w.t.2[i]
    mu[i] <- mu[i-1] + beta[i-1] + w.t.1[i]
    Y.t[i] <- mu[i] + v.t[i] 
}

plot(Y.t)

# now write in using matrix notation
library(mvtnorm)
set.seed(1)
n <- 100
sigma.b <- 1.17
sigma.u <- 1.3
V <- 12.6
theta.t <- rbind(rep(1.5, n), 
                 rep(14.3, n))
G <- matrix(c(1, 0, 1, 1), nrow=2)
W <- diag(c(sigma.u^2, sigma.b^2))
F <- c(1, 0)

w.t <- t(rmvnorm(n, sigma=W))
v.t <- rnorm(n, 0, sqrt(V))

Y.t <- F %*% theta.t + v.t

for (i in 2:n) {
    theta.t[, i] <- G %*% theta.t[, i-1] + cbind(w.t[, i-1])
    Y.t[i] <- F %*% theta.t[, i] + v.t[i]
}

plot(as.vector(Y.t))
