library(car); library(DescTools); library(MASS)

####################################
# Review of balanced case
#####################################

setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 5")  # need to change this path
tooth <- read.csv("ToothGrowth.csv")
tooth$Dose= as.factor(tooth$Dose)

summary(aov(Toothlength ~ Supplement + Dose, data=tooth)) 
summary(aov(Toothlength ~ Dose + Supplement, data=tooth))

#######################################
# unbalanced case
#######################################
ozkid <- read.csv("ozkids.csv",header=TRUE)
str(ozkid)

table(ozkid$origin); table(ozkid$sex)  # check unbalance 

###################################################
# what happens when the order of variables changes?
###################################################
summary(aov(days ~ origin + grade, data=ozkid))
summary(aov(days ~  grade + origin, data=ozkid))

anova(lm(days~origin + grade, data=ozkid))
anova(lm(days~grade + origin, data=ozkid))

###################################################
# boxplots of days by Origin
dev.new(width = 100, height = 100, unit = "px")

par(mfrow=c(2,2))
boxplot(days ~ origin,data=ozkid, main=" Days by Origin ",
        xlab="origin", ylab="days")

# boxplots of days by Sex

boxplot(days ~ sex,data=ozkid, main=" Days by Sex ",
        xlab="Sex", ylab="days")

# boxplots of days by grade

boxplot(days ~ grade,data=ozkid, main=" Days by grade ",
        xlab="grade", ylab="days")

# boxplots of days by type

boxplot(days ~ type,data=ozkid, main=" Days by type ",
        xlab="type", ylab="days")
####################################################

aov.ozkid1= aov(days ~  grade + origin, data=ozkid) # type 1 test
aov.ozkid2= aov(days ~  origin + grade, data=ozkid) 

aov.ozkid3=Anova(aov.ozkid1, type=3)  # type 3 test from package 'car'
Anova(aov.ozkid2, type=3)
summary(aov.ozkid1)

# post-hoc analysis
TukeyHSD(aov.ozkid1)

# assumption check
# In the video at around 27 minutes, I got an error with the following code.
LeveneTest(aov.ozkid1)  # error says - Model must be completely crossed formula only

########################################################################
# Levene's test works only for the full model with all interaction terms
LeveneTest(aov(days ~  grade*origin, data=ozkid))  # it works with grade*origin

# We observe small p-value 0.0003822 and it implies - heteroscedasticity (unequal variance)
# For multi-way ANOVA especially with interaction terms, it is hard to pass 
# equal variance assumption as we do not have large enough samples in each cell
# Thus, we will not seriously consider Levene's test result and mainly focus on Normality check
######################################################################

par(mfrow=c(1,1))
plot(aov.ozkid1, 2)
##############################################################
# Backward elimination on main effects model based on type 3 test
###############################################################
full.model = aov(days ~ origin + sex + grade + type, data = ozkid)

Anova(full.model, type=3)

tmp1= aov(days ~ origin + sex + grade , data = ozkid)
Anova(tmp1, type=3)

tmp2= aov(days ~ origin +  grade , data = ozkid)
Anova(tmp2, type=3)
summary(tmp2)

# include interaction and check its significance
aov.ozkid3= aov(days ~ origin +  grade +origin*grade , data = ozkid)
Anova(aov.ozkid3, type=3)
summary(aov.ozkid3)



