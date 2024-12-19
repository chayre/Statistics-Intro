#R Class 7 
#Colin Ayres
#Import Data
read.csv2("starwars.csv") -> starwars
starwars

#Only humans
starwars[starwars$species == "Human",] -> humonly
humonly

#Plot frequency of eye color
barplot(table(humonly$eye_color))

#Blue and brown are most common. Recode the rest as "other"
humonly$eye_color
humonly$new_color <- ifelse(humonly$eye_color == "blue" || humonly$eye_color == "brown", humonly$eye_color, "other")
humonly$new_color

#Convert gender to factor
humonly$gender <- as.factor(humonly$gender)
summary(humonly$gender)

#Change missing data in height to mean height
mean(humonly$height, na.rm = TRUE)
humonly$height <- ifelse(is.na(humonly$height), 178, humonly$height)
humonly$height

#Plot mean height by gender
tapply(humonly$height, humonly$gender, mean) -> heightgendermean
barplot(heightgendermean)