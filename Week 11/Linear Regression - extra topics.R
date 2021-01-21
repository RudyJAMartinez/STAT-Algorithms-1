library(MASS); library(car); library(olsrr)

setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 11")

#########################################################
# regression model with categorical variable
########################################################
professor=read.csv("Professor.csv",header=TRUE)
str(professor)
is.factor(professor$Gender); 
is.character(professor$Gender)

# if categorical variable is not a factor/ character
professor$Gender = as.factor(professor$Gender)
# or
lm(SALARY~factor(Gender)+PUBS, data=professor)

#############################################################
# # main effect model
##############################################################
#################################################################
# regression with factor/ character "Gender"
summary(lm(SALARY~Gender+PUBS, data=professor))  


#################################################################
# regression with numerically coded Gender.num
Gender.num=ifelse(professor$Gender=="Female",0,1)
professor=data.frame(professor,Gender.num)
View(professor)

summary(lm(SALARY~Gender.num+PUBS, data=professor))

#############################################################
# # interaction effect model
##############################################################

summary(lm(SALARY~Gender*PUBS, data=professor))  # main effect and interaction model

############################################################################
# regression model with categorical variable with more than three levels
############################################################################
tooth <- read.csv("ToothGrowth.csv") # data analyzed in balanced ANOVA
str(tooth)
tooth$Dose= as.factor(tooth$Dose)   # three levels: 0.5/ 1/ 2
str(tooth)

summary(lm(Toothlength ~ Dose, data=tooth))
summary(lm(Toothlength ~ factor(Dose), data=tooth))

###################################################
# log transformation
##################################################
athletes=read.table("athletes.txt", header=TRUE)
names(athletes)[1]="Sex"

lm.athletes=lm(Ferr~.-Sport, data=athletes)
model.stepwise<-ols_step_both_p(lm.athletes, pent = 0.05, prem = 0.05, details = FALSE)
model.stepwise

lm.step=lm(Ferr~Sex+BMI+LBM, data=athletes)
summary(lm.step)

dev.new(width = 1000, height = 1000, unit = "px")
par(mfrow=c(2,2))
plot(lm.step, which=1:4)  # diagnostics plot 

#####################################################
lm.log.athletes=lm(log(Ferr)~.-Sport, data=athletes)
model.log.stepwise<-ols_step_both_p(lm.log.athletes, pent = 0.05, prem = 0.05, details = FALSE)
model.log.stepwise

lm.log.step=lm(log(Ferr)~Sex+BMI+LBM, data=athletes)
summary(lm.log.step)

par(mfrow=c(2,2))
plot(lm.log.step, which=1:4)  # diagnostics plot 

