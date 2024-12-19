# Class 18
# Chi-squared = Sum of [(Observed - Expected) / Expected]
#120 students 4 instructors. Expected is 30 for each. What is chi-squared?
value <- (36 - 30)^2/30 + (24 - 30)^2/30 + (24 - 30)^2/30 + (36 - 30)^2/30
value
#chi-squared functions chisd,chisp,chisq,chisr 
pchisq(value, 1, lower.tail=FALSE)

#Two types of chi squared
#Pearson test of independence
#contingency test
#df=(rows???1)x(cols???1) e.g. df=1 in 2x2 table
#test of goodness of fit:
#df = number of frequency counts minus 1

#Goodness of fit: same data but now checking goodness of fit
pchisq(value, 3, lower.tail=FALSE)

#Titanic data again
read.csv("TitanicSurvival.csv") -> titanic
table(titanic$passengerClass)
nrow(titanic) / 3 -> expectedClass
chisqClass <- sum((table(titanic$passengerClass) - expectedClass)^2 / expectedClass)
chisqClass
(323-436.33)^2/436.33 + (277-436.33)^2/436.33 + (709-436.33)^2/436.33
pchisq(chisqClass, 2, lower.tail=FALSE)

chisq.test(table(titanic$passengerClass))
chisq.test(table(titanic$passengerClass), p = c(1, 1, 2)/4)

chisq.test(table(titanic$passengerClass), p = c(1, 1, 2)/4)$p.value
chisq.test(table(titanic$passengerClass), p = c(1, 1, 2)/4)$expected

table(titanic$sex, titanic$survived)
nrow(titanic) / 4 -> expectedSurvive
#expected
table((466/1309)*809, (466/1309)*500, (843/1309)*809, (843/1309)*500)
chisq.test(titanic$sex, titanic$survived)$expected
chisq.test(titanic$sex, titanic$survived)
#No correction
chisq.test(titanic$sex, titanic$survived, correct=FALSE)