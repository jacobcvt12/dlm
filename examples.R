library(dlm)

set.seed(1)

# simulate random walk with noise
# y_t = \theta_t + v_t, v_t \sim N(0, V)
# \theta_t = \theta_{t-1} + w_t, w_t \sim N(0, W)
v.t <- rnorm(1000, 0, 2)
w.t <- rnorm(1000, 0, 1)

# allocate memory
theta.t <- numeric(1000)
y.t <- numeric(1000)

# set initial value for theta and y
theta.t[1] <- w.t[1]
y.t[1] <- theta.t[1] + v.t[1]
for (i in 2:1000) {
    theta.t[i] <- theta.t[i-1] + w.t[i]
    y.t[i] <- theta.t[i] + v.t[i]
}

