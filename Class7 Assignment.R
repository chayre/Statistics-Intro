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
humonly$new_color <- ifelse(humonly$eye_color == "blue" | humonly$eye_color == "brown", humonly$eye_color, "other") #Have to use single line or
#alternatively: ifelse(humonly$eye_color %in% c("blue","brown"), humonly$eye_color, "other") 
#Check if eyecolor is in this array
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
barplot(heightgendermean, ylim=c(0, max(humonly$height))) -> height_plot
#Add error bars/arrow. You get the x coordinate of the bars from height_plot
#Arrows takes first x coordinate, y coordinate, second x, second y. y is mean+/-standard dev
height_plot
tapply(humonly$height, humonly$gender, mean (na.rm = TRUE)) -> hummeans
sqrt(tapply(humonly$height, humonly$gender, variance)) -> humsds
#Code 3 draws arrow at both ends, angle flattens the arrow to lines
arrows(x0 = height_plot, y0 = means - humsds, x1 = height_plot, y1 = meanhei + humsds, angle = 90, code = 3)

#R higher level graphical functions [barplot(), boxplot(), hist(), (unless add =T)]
#often return useful values invisible
#Lower level functions modify existing plot
#Use plot's coordinate system [eg. segments(), text(), points(), arrows()]
#Current plot is the last call to a higher level function. To reset graphics run last higher level function
#Graphical parameters modify function behavior (color, ylabel)
