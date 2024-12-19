#R Class 6
data("ToothGrowth")
ToothGrowth
summary(ToothGrowth)

# Factor = Nominal

# How many got each dose
table(ToothGrowth$dose)

#How many in each category got each dose
table(ToothGrowth$dose, ToothGrowth$supp)

#Convert dose to factor variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
summary(ToothGrowth)

#Calculate mean for each group and plot; tapply
mean(ToothGrowth[ToothGrowth$supp == "OJ", "len"]) -> OJMean
mean(ToothGrowth[ToothGrowth$supp == "VC", "len"]) -> VCMean
tapply(ToothGrowth$len, ToothGrowth$supp, mean) -> SuppMeanLen
barplot(SuppMeanLen)

#Calculate standard deviation
stddeviation <- function(s) {
  sqrt(sum(((s-mean(s))^2) / length(s)))
}
tapply(ToothGrowth$len, ToothGrowth$supp, stddeviation) -> SuppSDLen
SuppSDLen

#Draw barplot of mean length of teeth depending on the combination of supplement and dose
#Need a list of factors
list(ToothGrowth$supp, ToothGrowth$dose) -> CombinedList
tapply(ToothGrowth$len, CombinedList, mean)
barplot(tapply(ToothGrowth$len, CombinedList, mean), beside = TRUE, legend.text = TRUE)

#Data imputation - fill data with mean
mean(titanic$age, na.rm = TRUE) -> age_mean
age_mean
titanic$age <- ifelse(is.na(titanic$age), mean(titanic$age, na.rm = T), titanic$age)
titanic

#Scientific notation saves space when writing small or large numbers
#1.23e+08
a <- sqrt(2)
a * a
a * a == 2
print(a * a, digits = 20)

#"safe" test for equality (near equality)
#values smaller than 1.5e-08 should be treated as zeroes
all.equal(a * a, 2) 

# Sum of deviations from mean should be zero
# Small enough to treat as zero
sum(titanic$age - mean(titanic$age))

#absolute value
sum(abs(titanic$age - mean(titanic$age)))
#Median minimizes sum of absolute deviations
sum(abs(titanic$age - median(titanic$age)))

#Generate 100 random shoe sizes from 35 = 50
sample(35:50, 100, replace = TRUE) -> newshoes
#Pseudorandom, we can choose seed
set.seed(10)
sample(35:50, 100, replace = TRUE) -> newshoes
#Shoe sizes go by 0.5 though and we only have integer
sample(seq(35, 50, by=.5), 100, replace = TRUE) -> newshoes

# Median is smaller
sum(abs(newshoes - mean(newshoes))) >= sum(abs(newshoes - median(newshoes)))

#Do it many times to check if this is true (100000 times)
i <- 100

#Instead of for loop, use 'replicate' (i times)
replicate(i, {
  sample(35:50, 100, replace = TRUE) -> newshoes
  sum(abs(newshoes - mean(newshoes))) -> mean.dev 
  sum(abs(newshoes - median(newshoes))) -> median.dev
  mean.dev > median.dev | isTRUE(all.equal(mean.dev, median.dev))
}) -> longlogic
#1 if all are true
mean(longlogic)

#?'&' help file on &