#Class 8 Assignment
#Colin Ayres
#Import Titanic Data
read.csv("TitanicSurvival.csv") -> newtitanic
newtitanic

#Exclude First-Class from dataset
newtitanic[newtitanic$passengerClass != "1st",] -> poorship
summary(poorship)

#Exchange missing data in age vector with the mean age
mean(poorship$age, na.rm = TRUE) -> poorage
poorship$age <- ifelse(is.na(poorship$age), poorage, poorship$age)

#Plot bar plot of means of age for those who survived and those who died
tapply(poorship$age, poorship$survived, mean) -> surviveage
barplot(surviveage, ylim=c(0, 40)) -> surviveage_plot
surviveage_plot

#Improve variance function so it works with na.rm argument. Takes na.rm as an argument
variance <- function(s, narm = FALSE) {
 sum((s-mean(s, na.rm = narm))^2, na.rm = narm) / sum(!is.na(s))
}
variance(newtitanic$age, TRUE)

#Probability Density: d...
#dbinom(x, size, prob) e.g. probability that lady guesses 4 cups out of a sample of 10 and probability of 0.5
#dnorm(x, mu, sigma) 
#plot(x, y)
plot(0:3, c(1, 3, 2, 4))
#Plot lady with tea cups probability
n <- 10
p <- 0.5
plot(0:n, dbinom(0:n, n, p), ylim = c(0, 0.3)) -> plot_points
text(0:n, dbinom(0:n,n, p), round(dbinom(0:n,n, p), 3), pos=3) #pos 3 above text, pos 1 below text

#Quantile: q...
#qbinom(q, size, prob) e.g. which value cuts off a given percentage of area under the curve
#qnorm(q, mu, sigma) give it a probability and it tells us the value which cuts off that percent of distribution

#Cumulative probability: p...
#pbinom(p,size, prob) cumulative probability of all probabilities up to p 
#pnorm(p, mu, sigma) -> percentile

#Random number generator: r...
#rbinom(r, size, prob)
#rnorm(r,size, prob)

