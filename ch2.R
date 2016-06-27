library(dlm)

# dlm corresponding to random walk plus noise
rw <- dlm(m0=0,  # prior mean on theta.0
          C0=10, # prior variance on theta.0
          FF=1,  # slope of observation equation
          V=1.4, # variance of noise of observation equation
          GG=1,  # growth rate of state equation
          W=0.2  # variance of noise of state equation
          )

unlist(rw)

# dlm corresponding to linear growth model
lg <- dlm(m0=c(0, 0),
          C0 = 10 * diag(2),
          FF=cbind(1, 0),
          V=1.4,
          GG=matrix(c(1, 0, 1, 1), nrow=2),
          W=diag(c(0, 0.2)))
lg

# modify observation and system variance for linear growth model
V(lg) <- 0.8
W(lg)[2, 2] <- 0.5
V(lg)
W(lg)

# dynamic linear regression
# Y_t = theta_{t, 1} + theta_{t, 2} x_t + epsilon_t, epsilon_t ~ N(0, sigma_t^2)
# theta_t = G_t theta_{t-1} + w_t, w_t ~ N_2(0, W_t)
# => F_t = [1, x_t]
# so part of F varies with time, the other part does not
x.t <- rnorm(100) # covariates
dlr <- dlm(FF=matrix(c(1, 0), nrow=1),
           V=1.3,
           GG=diag(2),
           W=diag(c(0.4, 0.2)),
           m0=c(0, 0),
           C0=10 * diag(2),
           JFF=cbind(0, 1),
           X=x.t)
dlr
FF(dlr)

# adjust observation variaance for random walk to be time varying
JV(rw) <- 1
is.dlm(rw)
dlm(rw)

X(rw) <- rep(c(0.75, 1.25), c(10, 20))

rw <- dlm(rw)
V(rw)
