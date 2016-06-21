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

# priors

C.0 * (Y[1] - m.0) / (1.24 - m.0) - C.0

