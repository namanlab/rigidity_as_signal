set.seed(123)

### Parameters
n <- 2e6
p <- 0.6
alpha_H_S <- 1.2
alpha_H_W <- 0.8
V0S <- 12
V0W <- 20
sigma0 <- 10   # variance of Y's
tau1 <- 1.0     # variance of noise e1
tau2 <- 1.0     # variance of noise e2
PH <- 1.5

### Simulate Y and e
Y1 <- rnorm(n, V0S, sigma0)
Y2 <- rnorm(n, V0W, sigma0)
e1 <- rnorm(n, 0, tau1)
e2 <- rnorm(n, 0, tau2)

### Construct X and W
X1 <- alpha_H_S * Y1 + e1
X2 <- alpha_H_W * Y2 + e2

W0 <- p*Y1 + (1-p)*Y2
W1 <- p*X1 + (1-p)*X2 - PH

### Monte Carlo estimate
est <- mean(pmax(W1,0) * (W0 <= PH))
cat("Simulation estimate:", est, "\n")

### Theoretical calculation
phi <- function(x) dnorm(x)
Phi <- function(x) pnorm(x)

mu0 <- p*V0S + (1-p)*V0W
mu1 <- p*(alpha_H_S*V0S) + (1-p)*(alpha_H_W*V0W) - PH
var0 <- sigma0^2 * (p^2 + (1-p)^2)
var1 <- p^2*(alpha_H_S^2*sigma0^2 + tau1^2) + (1-p)^2*(alpha_H_W^2*sigma0^2 + tau2^2)
cov10 <- sigma0^2*(p^2*alpha_H_S + (1-p)^2*alpha_H_W)

s <- sqrt(var1 - cov10^2/var0)

m_fun <- function(x0){
  mu1 + (cov10/var0)*(x0 - mu0)
}

integrand <- function(x0){
  m <- m_fun(x0)
  val <- s*phi(m/s) + m*Phi(m/s)
  val * dnorm(x0, mean=mu0, sd=sqrt(var0))
}

theory <- integrate(integrand, lower=-Inf, upper=PH, rel.tol=1e-6)$value
cat("Theoretical (integral) :", theory, "\n")