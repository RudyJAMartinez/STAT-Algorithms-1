# HW1
setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 2/HW1") # set your own path

#Libraries
library(tidyverse)

# Exercise 1
cars=read.csv("Cars.csv", header = TRUE)  # read data set 

# (1.a) ########################################################
MPG_Combo = 0.55*cars$MPG_City+0.45*cars$MPG_Highway  # combined mpg variable 
cars = data.frame(cars, MPG_Combo)   # data frame with MPG_Combo 
attach(cars)

boxplot(cars$MPG_Combo,                     
        main = "Distribution of Fuel Effeciency",
        ylab = "MPG_Combo",
        col = "White",
        border = "skyblue4",
        horizontal = FALSE
)
points(mean(cars$MPG_Combo, na.rm=TRUE), col="red")

# (1.b) ########################################################
boxplot(MPG_Combo ~ Type, data=cars, 
        main = "Distribution of Fuel Effeciency by Vehicle Type",
        xlab = "Type",
        ylab = "MPG_Combo",
        col = "White",
        border = "skyblue4",
        horizontal = FALSE
)

# (1.c) ########################################################
summary(cars$Invoice)

boxplot(cars$Invoice,
        main = "Invoice Amount ($) for All Vehicles",
        ylab = "Invoice Amount ($)",
        col = "White",
        border = "skyblue4",
        horizontal = FALSE
)

hist(cars$Invoice,
     main = "Invoice Amount ($) for All Vehicles ", 
     xlab = "Invoice Amount ($)",
     col = "slategrey",
     border = "skyblue4")

plot(density(cars$Invoice), 
     main="Invoice Amount ($) for All Vehicles",
     xlab = "Invoice Amount ($)", 
     col = "slategrey", 
     border = "skyblue4")

qqnorm(cars$Invoice); qqline(cars$Invoice, col = 2)

shapiro.test(cars$Invoice)

# (1.d) ########################################################
boxplot(Invoice ~ Origin, data=cars, 
        main = "Invoice Amount ($) by Continent (Origin)",
        xlab = "Continent (Origin)",
        ylab = "Invoice Amount ($)",
        col = "White",
        border = "skyblue4",
        horizontal = FALSE
)

histogram_plot = ggplot(data=cars, mapping=aes(x=Invoice))+geom_histogram(aes(fill=Origin, color=Origin), alpha = 0.25, bins=40) + facet_wrap(Origin~.)
histogram_plot

qq_plot1 = ggplot(cars, aes(sample = Invoice, col=Origin)) + facet_wrap(~Origin, ncol=1)
qq_plot1 + stat_qq() + stat_qq_line() + theme_gray()

shapiro.test(cars[cars$Origin=="Asia", "Invoice"])
shapiro.test(cars[cars$Origin=="Europe", "Invoice"])
shapiro.test(cars[cars$Origin=="USA", "Invoice"])




# Exercise 2

# (2.a) ########################################################
# Answer included in formal report

# (2.b) ########################################################	
# Answer included in formal report

# (2.c) ########################################################
asia_europe = filter(cars, Origin == 'Asia' | Origin == 'Europe')

wilcox.test(Invoice ~Origin, data=asia_europe, exact=FALSE)



# Exercise 3

# (3.a) ########################################################
shapiro.test(airquality[airquality$Month==7, "Wind"])
shapiro.test(airquality[airquality$Month==8, "Wind"])

qq_plot2 = ggplot(july_august, aes(sample = Wind, col=Month)) + facet_wrap(~Month, ncol=2)
qq_plot2 + stat_qq() + stat_qq_line() + theme_gray() + theme(legend.position = "none")

july_august = filter(airquality, Month == 7 | Month == 8)

var.test(Wind ~Month, july_august, alternative ="two.sided")

t.test(Wind ~Month, july_august,alternative ="two.sided", var.equal=TRUE)

# (3.b) ########################################################
# Answer included in formal report

# (3.c) ########################################################
# Answer included in formal report

