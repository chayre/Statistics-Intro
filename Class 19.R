# Class 19

n <- 20 # assuming equal-sized samples
alpha <- .05 # divided between two tails
sigma <- 6 # assuming homogeneity of variance
d <- 5 # non-standardized effect size
# ie difference between population means given alt.h

#se = sqrt(2 * sigma^2 / n)
se <- sqrt(2 * sigma^2 / n)

curve(dnorm(x, 0, se), -5, 11, lwd=3) #-5 and 11 makes the 2nd curve fit in
curve(dnorm(x, d, se), add=TRUE)
# h0 thick because of lwd
# cr = critical value
cr <- qnorm(p = alpha/2, mean = 0, sd = se, lower.tail = FALSE)
abline(v=cr, lty=2) # dashed line because of lty
# power area to right of dashed line, Ha chart
# to calculate power calculate critical value of Ho, then use critical value against Ha distribution
power <- pnorm(cr, mean = d, sd = se, lower.tail = FALSE)
# moving alpha affects power, also changing standard error (by increasing N), or changing expected effect size
# if sigma unknown, use t distribution

#Normal distribution with mean at 0 and se as above
#standardized
hist(replicate(10000, (mean(rnorm(n, , sigma))
  - mean(rnorm(n, , sigma))) / se),
  main="", xlab="", xlim=c(-5, 11), freq=FALSE)
hist(replicate(10000, (mean(rnorm(n, d, sigma))
  - mean(rnorm(n, 0, sigma))) / se),
  add=TRUE, freq=FALSE)
curve(dnorm(x, 0, se/se), add=TRUE)
curve(dnorm(x, d/se, se/se), add=TRUE)


# If we don't know population standard deviation
d <- 15
hist(replicate(10000, {
  s1 <- rnorm(n, 0, sigma)
  s2 <- rnorm (n, 0, sigma)
  (mean(s1) - mean(s2)) / sqrt((var(s1)+var(s2))/n)
    
}), main="", xlab="", xlim=c(-5, 11), freq=FALSE)

#use t distribution
curve(dt(x, 2*n-2), add=TRUE)

#increase d to 15. this is non-central t-distribution
hist(replicate(10000, {
  s1 <- rnorm(n, d, sigma)
  s2 <- rnorm (n, 0, sigma)
  (mean(s1) - mean(s2)) / sqrt((var(s1)+var(s2))/n)
  
}), main="", xlab="", xlim=c(-5, 15), freq=FALSE, add=TRUE, col="red")

# if non-central t, use d/se
curve(dt(x, 2*n-2, d/se), add=TRUE, col="red")

crt <- qt(alpha/2, 2*n - 2, lower.tail=FALSE)
abline(v=crt, lty=2) 
t_power <- pt(crt, 2*n - 2, d/se, lower.tail = FALSE)
t_power

#can be done with power.t.test
#Gives you N
power.t.test(NULL, 0.5, 1, 0.05, 0.8)
# Gives probability of obtaining statistically significant result
power.t.test(n = 20, delta = 5, sd = 8, 0.05, NULL)
