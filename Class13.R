#Class 13
# Cut-off point for 2.5% and 97.5% with N = 25, mu = 100, sigma = 15
qnorm(c(0.025,0.975), 100, 15/sqrt(25))

# Z = (Xbar - mu) / (sigma / sqrt(N))
qnorm(c(0.025, 0.975))

# Xbar = mu + qnorm * sigma/sqrt(N)
100 + qnorm(0.025) * 15/sqrt(25)
100 - qnorm(0.025) * 15/sqrt(25)

# Cut off the extreme 1%
100 - qnorm(0.005) * 15/sqrt(25)
100 + qnorm(0.005) * 15/sqrt(25)

# Shoe sizes, mu = 42, sigma = 2, N = 50, 5% extreme
42 + qnorm(0.025) * 2 / sqrt(50)
42 - qnorm(0.025) * 2 / sqrt(50)

# Best guess for mean is the sample mean
# Confidence interval: uses xbar instead of mu in the formula
# If the population mean is in the range, we have 95% chance of drawing a sample with our sample mean
# we have p % confidence that the sample comes from a population 
# with the parameter falling within the interval
x = 117; sigma = 15; z = qnorm(0.005); N = 30
c(x + z * sigma/sqrt(N), x - z * sigma/sqrt(N))

x = 105; sigma = 15; z = qnorm(0.025); N = 20
c(x + z * sigma/sqrt(N), x - z * sigma/sqrt(N))


# Known population variance
i <- 10000
mu <- 100; sigma <- 15; n <- 10
mean(replicate( i, {
  s <- rnorm(n, mu, sigma)
  mu >= mean(s) - qnorm(0.975) * sigma/sqrt(n) &
    mu <= mean(s) + qnorm(0.975) * sigma/sqrt(n)
}))

# Unknown population variance
i <- 10000
mu <- 100; sigma <- 15; n <- 30
mean(replicate( i, {
  s <- rnorm(n, mu, sigma)
  mu >= mean(s) - qnorm(0.975) * sd(s)/sqrt(n) &
  mu <= mean(s) + qnorm(0.975) * sd(s)/sqrt(n)
}))

# T-distribution is like Z score but with s/sample sd instead of sigma
# t = (Xbar - mu)/(sd / sqrt(N))
# Uses df, degrees of freedom. More df, closer t-distribution is to z-distribution
# Uses qt, pt, dt, rt 
i <- 10000
mu <- 100; sigma <- 15; n <- 3
mean(replicate( i, {
  s <- rnorm(n, mu, sigma)
  mu >= mean(s) - qt(0.975, n-1) * sd(s)/sqrt(n) &
  mu <= mean(s) + qt(0.975, n-1) * sd(s)/sqrt(n)
}))

# 25 people sampled, mean shoe size is 40, sd is 3
x = 40; sd = 3; n = 25
c(x - qt(0.975, n-1) * (sd/sqrt(n)), x + qt(0.975, n-1) * (sd/sqrt(n)))



