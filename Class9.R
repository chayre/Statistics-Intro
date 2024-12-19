#Class 9
#Plot lady with tea cups probability
n <- 20
p <- 0.5

plot(0:n, dbinom(0:n, n, p), type="n", ylim = c(0, 0.2)) #Now we have an empty plot but we have the plot points
text(0:n, dbinom(0:n,n, p), round(dbinom(0:n,n, p), 3)) #Plot with the text in place of plot points

#Probability of exactly 14 cups guessed
dbinom(14, n, p)

sum(dbinom(14:n, n, p))#Don't reject null hypothesis for 14, this is greater than 0.5
1- pbinom(13, n, p) #pbinom gives the probability of 0 - 13
pbinom(13, n, p, lower.tail=FALSE) #Also same as above

#Plot IQ from 55 to 145. Make it a line because it's continuous
plot(55:145, dnorm(55:145, 100, 15), type="l")
#or use curve
curve(x^2, -10, 10)
curve(sqrt(x))
curve(sqrt)
curve(dnorm(x, 100, 15), 55, 145, xlab="IQ", ylab="proportion")
curve(dnorm, -3, 3)
#To make line at 90 IQ. lty is dotted
abline(v = 90, lty=2, col="red")

#Percent of population below 90 IQ
pnorm(90, 100, 15)

#What is the IQ of 10th percentile IQ
floor(qnorm(0.1, 100, 15))
abline(v = floor(qnorm(0.1, 100, 15)), lty=2, col="green", lwd=2)

#What is 10th percentile score if we have mean of 150 and sd 30
floor(qnorm(0.1, 150, 30))

#How many scored below 48 with mean of 56 and sd 12, population 800
floor(pnorm(48, 56, 12)*800)
#How many scored below 56 with mean 48 and sd 8, population 500
floor(pnorm(56, 48, 8, lower.tail = F)*500)

#What proportion of people have IQ between 70 and 130
pnorm(130, 100, 15) - pnorm(70, 100, 15)

#What is 25th percentile to pass exam with mean of 72 and variance 36
ceiling(qnorm(0.25, 72, 6))

#Tea lady guessing 2 right out of 10
n <- 10; p <- 0.5
plot(0:n, dbinom(0:n, n, 0.5))
mu <- n*p
sigma <- sqrt(n*p*(1-p))
curve(dnorm(x, mu, sigma), add=TRUE, col="red")
pbinom(2, n, p)
pnorm(2, mu, sigma)
#If we want to approximate discrete with continuous, do continuous correction; take value at 2.5
abline(v=2.5, lty=2, col="blue")
pnorm(2.5, mu, sigma)
