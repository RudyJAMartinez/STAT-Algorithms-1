
#setwd("C:/...") # set your working directory

birthweight = read.csv("birthweight_final.csv", header=TRUE)
birthweight$Black = as.factor(birthweight$Black)
birthweight$Married = as.factor(birthweight$Married)
birthweight$Boy = as.factor(birthweight$Boy)
birthweight$MomSmoke = as.factor(birthweight$MomSmoke)
birthweight$Ed = as.factor(birthweight$Ed)
