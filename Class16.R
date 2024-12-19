#Class 16
read.csv("norwegian_cdi_items.csv") -> items
read.csv2("norwegian_cdi.csv") -> cdi
cdi[cdi$form == "WG",] -> cdi  
subset(cdi, select = c(child_id, age, comprehension, production, sex, caregiver_education)) -> cdi
cdi[cdi$age==20,] -> cdi20
merge(cdi20, items, all.y = FALSE) -> items_extra

items_extra[items_extra$category=="vehicles",] -> cdi_vehicles
na.omit(cdi_vehicles) -> cdi_vehicles

cdi_vehicles$production_vehicles <- ifelse(cdi_vehicles$value == "produces", 1, 0)
cdi_vehicles$comprehension_vehicles <- ifelse(cdi_vehicles$value == "understands", 1, 0)

#If either production or comprehension then they can comprehend. "either" column represents comprehension
cdi_vehicles$either_vehicles <- ifelse(cdi_vehicles$production_vehicles == 1 | cdi_vehicles$comprehension_vehicles == 1 | cdi_vehicles$value == "yes", 1, 0)

aggregate(list(summed_comprehension_vehicles = cdi_vehicles$either_vehicles), list(age = cdi_vehicles$age, 
caregiver_education = cdi_vehicles$caregiver_education, sex = cdi_vehicles$sex, 
student = cdi_vehicles$child_id), sum) -> cdi_vehicles_sum

barplot(tapply(cdi_vehicles_sum$summed_comprehension_vehicles, cdi_vehicles_sum$sex, mean, na.rm = TRUE))

#z scores
(105-100)/(15/sqrt(30))
qnorm(0.975)

#one-sample t-test
scores <- c(10, 50, 46, 32, 37, 28, 41, 20, 32, 43)
(mean(scores) - 26.5) / (sd(scores)/sqrt(length(scores))) -> t.statistic
qt(0.975, length(scores)-1) 
#probability of this score. p-value
pt(t.statistic, length(scores)-1, lower.tail=F) * 2

#paired sample t-test. 49 students took test before and after semester
n = 49; mean_diff = 0.5; std_dev_diff = 1.75;
(mean_diff - 0)/(std_dev_diff/sqrt(n)) -> t.statistic
pt(t.statistic, n-1, lower.tail=F) * 2

#paired sample t-test
m <- c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5)
f <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5)
mean(m - f) / (sd(m - f) / sqrt(length(m))) -> t.statistic
pt(t.statistic, length(m)-1, lower.tail=F) * 2

#two sample t-test
#h0 = u1 - u2 = 0


#sampling distribution of difference between two means
N <- 25; mu <- 100; sigma <- 15
i <- 10000
sampling_mean <- replicate(i,mean(rnorm(N,mu,sigma)))
hist(sampling_mean, prob = T)
curve(dnorm(x, mu, sigma / sqrt(N)), add=T)
mean(sampling_mean)
sd(sampling_mean)
sigma/sqrt(N)

mu1 <- 100; sigma1 <- 15
mu2 <- 110; sigma2 <- 10
sampling_diff <- replicate(i, mean(rnorm(N, mu1, sigma1)) - mean(rnorm(N, mu2, sigma2)))
hist(sampling_diff)
mean(sampling_diff)
var(sampling_diff)
sigma1^2 / N + sigma2^2 / N # sum of both variances
#if you know both stdev: 
# z = (x1 - x2) - (mu1 - mu2) / sqrt( sigma1^2/N1 + sigma2^2 /N2)

#student t-test
# t = (x1 - x2) / sp * sqrt(1/n1 + 1/n2)  
#sp is a weighted mean of standard deviations
#sp = (N1 - 1)*S1 + (N2 - 1)*S2 / (N1+N2-2)
#degrees freedom = N1 + N2 - 2

#paired t-test
t.test(m, f, paired = TRUE) -> pairs_satisfaction_test
names(pairs_satisfaction_test)
pairs_satisfaction_test$p.value

#welch two sample t-test (if sigmas different)
t.test(m, f)

#two sample t-test (if sigmas same)
t.test(m, f, var.equal = TRUE)


