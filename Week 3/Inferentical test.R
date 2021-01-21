setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 3")  # need to change this path to yours

#install.packages("BSDA")  # BSDA package for Sign test

water=read.table("WATER.txt",header=TRUE, sep="")

str(water)

###############################################
# one-sample test 
# normality check for mortal and hardness
##############################################
summary(water$mortal)
hist(water$mortal)
boxplot(water$mortal); points(mean(water$mortal), col="red")
qqnorm(water$mortal);qqline(water$mortal, col = 2)
shapiro.test(water$mortal)

summary(water$hardness)
hist(water$hardness)
boxplot(water$hardness); points(mean(water$hardness), col="red")
qqnorm(water$hardness);qqline(water$hardness, col = 2)
shapiro.test(water$hardness)

# one-sample t-test
t.test(water$mortal, mu=1500)

# sign test
library(BSDA)
SIGN.test(water$hardness, md=45)

################################################
# two-sample test
# normality check by location
################################################

################################################
# hardness (South vs. North)
boxplot(hardness ~ location, data=water, main="hardness by location",
        xlab="location", ylab="hardness")

shapiro.test(water$hardness[water$location=="S"])
shapiro.test(water$hardness[water$location=="N"])

# non-parametric wilcox test
wilcox.test(hardness ~ location, data=water, exact=FALSE)


#################################################
# mortality (South vs. North)
boxplot(mortal ~ location, data=water, main="mortal by location",
        xlab="location", ylab="mortal")

shapiro.test(water$mortal[water$location=="S"])
shapiro.test(water$mortal[water$location=="N"])

# Equal variance test to decide - pooled t-test or satterthwaite t-test?
var.test(mortal ~ location, water, 
         alternative = "two.sided")
bartlett.test(mortal ~ location, water)

# parametric t-test
t.test(mortal ~ location, water, 
         alternative = "two.sided",var.equal=TRUE)

