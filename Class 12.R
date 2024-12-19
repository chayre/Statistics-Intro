# Class 12
# Sampling Distribution of Means
mu <- 100;sigma <- 1
n <- 1
i <- 100000
replicate(i, mean(rnorm(n, mu, sigma))) -> sample_mean
hist(sample_mean, freq=FALSE, ylim = c(0,0.25))
curve(dnorm(x, mu, sigma), add=TRUE, col= rgb(1, 1, 0, 0.2))  
# As n goes up, historgram gets more centered around mu. With n = 1 it is just dnorm
n <- 10
replicate(i, mean(rnorm(n, mu, sigma))) -> sample_mean
hist(sample_mean, add=T, col = rgb(1, 0, 0, 0.2), freq=FALSE)

n <- 100
replicate(i, mean(rnorm(n, mu, sigma))) -> sample_mean
hist(sample_mean, add=T, col = rgb(1, 0, 1, 0.2), freq=FALSE)

n <- 1000
replicate(i, mean(rnorm(n, mu, sigma))) -> sample_mean
hist(sample_mean, add=T, col = rgb(0, 1, 1, 0.2), freq=FALSE)


# As n goes up, histogram gets more centered around mu. With n = 1 it is just dnorm  
# Mean and median are unbiased estimators. But mean is more efficient estimator
# standard error is standard deviation of sampling distribution

curve(dnorm(x, mu, sigma), mu - 3*sigma, mu + 3*sigma, ylim=c(0,0.15), lwd=3)
n <- 20
replicate(i, mean(rnorm(n, mu, sigma))) -> sample_mean
hist(sample_mean, add=T, col = rgb(0, 1, 0, 0.2), freq=FALSE)
replicate(i, median(rnorm(n, mu, sigma))) -> sample_median
hist(sample_median, add=T, col = rgb(1, 0, 0, 0.2), freq=FALSE)

# Mean is a better guess than median. rgb 4th argument is opacity

#Exercises
#Compare avg people and geniuses
# Avg people
mu <- 100
sigma <- 15
curve(dnorm(x, mu, sigma), mu - 3*sigma, mu + 4*sigma, ylim=c(0,0.2), lwd=3, col="red")
hist(replicate(i, mean(rnorm(20, mu, sigma))), freq=FALSE, col=rgb(1, 0, 0, 0.2), add=TRUE)

mu <- 115
sigma <- 10
curve(dnorm(x,mu,sigma), add=TRUE, col="blue")
hist(replicate(i, mean(rnorm(20, mu, sigma))), freq=FALSE, col="blue", add=TRUE)


# Assuming IQ is normally distributed, what is the probability of drawing a random sample 
# with N = 25 and sample mean > 130
mu <- 100; sigma <- 15; n <- 20
hist(replicate(i, mean(rnorm(n, mu, sigma))), freq=FALSE, ylim=c(0, 0.15))
curve(dnorm(x, mu, sigma / sqrt(n)), add=T, lwd=3)

# Exercise 2 what is the probability of pulling a sample with N = 25 and sample mean > 130
# Standard error of sample mean is sigma / sqrt(25)
mu <- 100; n <- 25; sigma <- 15/sqrt(n)
pnorm(130, mu, sigma, lower.tail=F) 

#Between 85 and 115
pnorm(115, mu, sigma) - pnorm(85, mu, sigma) 

#Between 95 and 105
pnorm(105, mu, sigma) - pnorm(95, mu, sigma) 

mu <- 100; n <- 10; sigma <- 15/sqrt(n)
#Greater than 130
1-pnorm(130, mu, sigma) 

#Between 85 and 115
pnorm(115, mu, sigma) - pnorm(85, mu, sigma) 

#Between 95 and 105
pnorm(105, mu, sigma) - pnorm(95, mu, sigma) 

#If n is 1 then it's pnorm with sample_sigma = sigma, greater chance of means varying from 100

#Exercise 3, what is range of values 95% of sample means will fall in with a sample of 50 people
mu <- 100; n <- 50; sigma <- 15/sqrt(n)
qnorm(0.025, mu, sigma)
qnorm(0.975, mu, sigma)
qnorm(c(0.025, 0.975), mu, sigma)
# 95 - 105
#How many people do you need to sample to have 95% chance their their mean will fall between 95 and 105
mu <- 100; n <- 35; sigma <- 15/sqrt(n)
qnorm(0.025, mu, sigma)
qnorm(0.975, mu, sigma)
#At least 35 people
mu + qnorm(0.025) * sigma # = 95
mu - qnorm(0.025) * sigma
(qnorm(0.975)/((105-mu)/15))^2
(qnorm(0.975)*3)^2

#Bonus
#Use variance function
#Plot simulated sampling distribution of IQ variance
corrvariance <- function(s) {
  sum(((s-mean(s))^2) / (length(s) - 1))
}
variance <- function(s) {
  sum(((s-mean(s))^2) / length(s))
}
mu <- 100; n <-10; sigma <- 15; i <- 50000
hist(replicate(i, variance(rnorm(n, mu, sigma)), freq=FALSE))
hist(replicate(i, corrvariance(rnorm(n, mu, sigma)), add=T, col= rgb(1, 1, 0, 0.2), freq=FALSE))
(225)
abline(v=225, col="red")
# As n goes up it tends to go to center around 225, which is 15^2, the population variance
# But it's right-skewed with smaller n, biased
# The mean of all these (mean(replicate...)) is like 180, not 225

# Sample estimate of variance = 1 / (N-1) * sum(Xi- mean(X))^2
# Bessel's correction uses N-1 instead of N. Estimating a parameter "costs" 1 degree of freedom
mean(replicate(i, var(rnorm(n, mu, sigma)), freq=FALSE))

#Mean vs. Median minimizes sum of squared deviations
mu <- 100; sigma <- 15
rnorm(20, mu, sigma) -> iqs
i <- 100000
mean(replicate(i, {
  rnorm(20, mu, sigma) -> iqs
  sum((iqs - mean(iqs))^2) -> mean.dev 
  sum((iqs - median(iqs))^2) -> median.dev
  mean.dev <= median.dev | isTRUE(all.equal(mean.dev, median.dev))
}))