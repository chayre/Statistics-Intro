
variance <- function(s) {
sum((s-mean(s))^2) / length(s)
}

variance(1:10)
var(1:10)

#read.table() tab-deliminated
#read.csv() comma-deliminated
#read.csv2() semicolon-deliminated

read.csv("shoes.csv") -> shoes

shoes[6,2] #6th row, 2nd column
shoes[1:3, 2:3] #First three rows, 2nd and third columns

shoes[3, ] #3rd row all columns
shoes[c(5,7), ] #5th row and 7th row, all columns
shoes[nrow(shoes), ] #Gives the number of rows. There is ncol function as well

#Column access
shoes[ ,2] #Gives the 2nd column
length(shoes[,1]) #same as ncol, length of column

#Access Column by Name
table(shoes[,"Nationality"])
#or
mean(shoes$Shoe.Size)

#Conditional row access
shoes[shoes$Nationality != "Polish", ]

head(shoes) #useful to see first rows
summary(shoes) #summary of each column of data

#to get value use ->
#to set value use <-

#IDS of all people with shoe size below median
shoes[shoes$Shoe.Size < median(shoes$Shoe.Size), "ID"]

#Change person with ID C to have shoe size *100
shoes[shoes$ID == "C", 3] <- shoes[shoes$ID == "C", 3]*100 

#Delete one of the values in csv file and reload data
#Could also set the value to NA [literally,  no quotations]
read.csv("badshoes.csv") -> badshoes
summary(badshoes)
badshoes
mean(badshoes$Shoe.Size, na.rm = TRUE)

#Make a new column if you are polish or not
#ifelse(condition, value if true, value if false)
ifelse(shoes$Nationality != "Polish", "Foreign", "Home")
#Save it to another column
shoes$status <- ifelse(shoes$Nationality != "Polish", "Foreign", "Home")

#Make a character vector of 10, 11, 12
v1 <- c("10","11","12")
#Alternative way
v2 <- as.character(10:12)
#Logical vector
v1 == v2 -> v3
#Boolean
all.equal(v1, v2)

mean(v3)

# Factor: In statistics a nominal variable
# Factor levels: values of the variable
summary(shoes$Nationality)
shoes$Nationality <- as.factor(shoes$Nationality)
summary(shoes$Nationality)
levels(shoes$Nationality)
shoes$Nationality <- as.character(shoes$Nationality)
summary(shoes$Nationality)

shoes