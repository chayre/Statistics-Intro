#Colin Ayres
#Gioele Ovarelli
#Graded Assignment 1
#Problem 1 import data
read.csv("TitanicSurvival.csv") -> titanic
titanic

#Problem 2 show passengerClass frequency
summary(as.factor(titanic$passengerClass))

#Problem 3 
mean(titanic$age, na.rm = TRUE) -> age_mean

#Problem 4 Write standard deviation
variance <- function(s) {
  sum(((s-mean(s))^2) / length(s))
}
sqrt(variance(titanic[!is.na(titanic$age), "age"])) -> age_sd 

#Problem 5 Make adult column
titanic$adult <- ifelse(titanic$age > 18, TRUE, FALSE)
summary(titanic)

#Problem 6 Add standardized age values
titanic$age_z <- ((titanic$age - age_mean)/age_sd)
titanic

#Plotting Distribution of Adults
hist(log(titanic[titanic$adult, "age"]), main="Age Distribution of Adults", xlab = "Age")
hist(titanic$age)
hist(titanic$age_z)

#Boxplot
boxplot(titanic$age)
boxplot(titanic$age_z)

#barplot
barplot(table(titanic$passengerClass), ylab="Frequency", xlab = "Passgener Class", main = "Distribution of Passenger Class", col = "cyan")

#Contingency Table
table(titanic$passengerClass, titanic$survived)
barplot(table(titanic$survived, titanic$passengerClass), beside = TRUE, legend.text = TRUE, col = c("blue", "red"))

barplot(table(titanic$survived, titanic$passengerClass), beside = TRUE, legend.text = TRUE, col = rainbow(2))

#Help
?rainbow

#Relationship between Numerical and Nominal values
#tapply takes 3 or more arguments. Vector of values to do something with, Vector used to divide 1st vector into groups, name of function we want to apply to each of the groups
tapply(titanic$age, titanic$passengerClass, mean, na.rm = TRUE) -> ageclass
barplot(ageclass, ylim=c(0,max(titanic$age, na.rm=T)))
