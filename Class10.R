#Class 10
#Things to remember: ifelse, square bracket notation, tapply, aggregate (same as tapply but returns 
#dataframe instead of vector/matrix; useful when you want to use data)
#Make our own data on student grades
#concatenate and make student ID column
student <- paste("student", 1:5, sep = "_")
#paste(c("student", "instructor), 1:5, sep = "_")
#Make random grades with sample
#set seed to 111
set.seed(111)

#Make data frame
grades <- data.frame(student, oct = sample (0:10, 5), nov = sample (0:2, 5), dec = sample(0:10, 5))
#sample from random list of warsaw districts. sample with replacement. less likely to live in Praga
sample(c("Praga", "Warszawa"), 5, replace = TRUE, prob = c(1, 2)) -> grades$district
#datset ready
#wide format

#Plot bar plot of means for each month
barplot(c(mean(grades$oct), mean(grades$nov), mean(grades$dec)), ylim = c(0, 10))

#create long format data, which is most common
reshape(grades, direction = "long", varying = c("oct", "nov", "dec"), v.names = "score", timevar = "month", times = c("oct", "nov", "dec"), idvar = "student") -> long_grades
long_grades
#now do the same but selecting for 3 character name columns, not manual
nchar(colnames(grades)) == 3
month <- colnames(grades)[nchar(colnames(grades)) == 3] #oct, nov, dec
reshape(grades, direction = "long", varying = month, v.names = "score", timevar = "month", times = month, idvar = "student") -> long_grades
long_grades

#barplot of mean scores by month
#Make it a factor column but can't use as.factor
long_grades$month <- factor(long_grades$month, levels = month)
barplot(tapply(long_grades$score, long_grades$month, mean), ylim = c(0,10))

#Now plot grades by month and district (side-by-side)
barplot(tapply(long_grades$score, list(long_grades$month, long_grades$district), mean), beside = TRUE, legend = TRUE, ylim = c(0,10))

#Calculate sum of scores for each student
mean(long_grades[student == "student_1", "score"]) #For one student
#For all students
tapply(long_grades$score, list(long_grades$student), sum)
#Or use which returns a data frame instead of a vector
aggregate(long_grades$score, list(student = long_grades$student, district = long_grades$district), sum) -> studentsums
#List column names
colnames(studentsums)
#Change column names
colnames(studentsums) <- c("student", "district", "sum")
colnames(studentsums)

#Import cdi data
read.csv("cdi.csv") -> cdi
colnames(cdi)[grep("item_", colnames(cdi))] -> words
#Reshape data to long
reshape(cdi, direction = "long", varying = words, v.names = "responses", timevar = "word", times = words, idvar = "student") -> long_cdi
colnames(long_cdi)
long_cdi
#Make produce column
long_cdi$produces <- ifelse(long_cdi$responses == "produces", 1, 0)
#Make understand column
long_cdi$understands <- ifelse(long_cdi$responses == "understands", 1, 0)

long_cdi
