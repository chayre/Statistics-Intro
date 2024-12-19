#Class 11
#Colin Ayres
#Mean minimizes sum of squared deviations; median minimizes sum of absolute deviations
i <- 1000
#Use prob to alter distribution skew
mean(replicate(i, {
  sample(seq(35, 50, by=0.5), 5, replace=TRUE, prob = c(rep(1,25), 5, 6, 7, 8, 9, 10)) -> newshoes
  sum(abs(newshoes - mean(newshoes))^2) -> mean.dev 
  sum(abs(newshoes - median(newshoes))^2) -> median.dev
  mean.dev < median.dev | isTRUE(all.equal(mean.dev, median.dev))
}))
#Use unif for uniform distribution
qunif(0.5, 35, 50) -> foot.centre
mean(replicate(i, {
  runif(20, 35, 50) -> foot
  sum(abs(foot.centre - mean(newshoes))) -> mean.dev 
  sum(abs(foot.centre - median(newshoes))) -> median.dev
  mean.dev < median.dev | isTRUE(all.equal(mean.dev, median.dev))
}))

curve(dunif(x, 35, 50), 30, 50)
curve(dunif(x, 35, 50), 30, 55)
qunif(0.5, 35, 50)

#IQ test Example
mu <- 120; sigma <- 15
rnorm(5, 120, 15) -> iqs
i <- 10000
mean(replicate(i, {
  rnorm(5, 120, 15) -> iqs
  sum(abs(mu - mean(iqs))) -> mean.dev 
  sum(abs(mu - median(iqs))) -> median.dev
  mean.dev < median.dev | isTRUE(all.equal(mean.dev, median.dev))
}))

#If you have a sample and you don't know the population parameters, it's better to guess 
#the population parameter based on the sample median
