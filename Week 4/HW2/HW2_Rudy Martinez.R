## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Set Working Directory------------------------------------------------------------------------------------------------
setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 4/HW2")


## ----Read Files-----------------------------------------------------------------------------------------------------------
heart = read.csv("heartbpchol.csv"); 
heart$BP_Status = as.factor(heart$BP_Status); 
heart$Cholesterol = as.numeric(heart$Cholesterol)

bupa = read.csv("bupa.csv"); 
bupa$drinkgroup = as.factor(bupa$drinkgroup); 
bupa$mcv = as.numeric(bupa$mcv); 
bupa$alkphos = as.numeric(bupa$alkphos)

psych = read.csv("psych.csv");
psych$sex = as.factor(psych$sex);
psych$rank = as.factor(psych$rank);
psych$salary = as.numeric(psych$salary)

cars_new = read.csv("cars_new.csv");
cars_new$type = as.factor(cars_new$type);
cars_new$origin = as.factor(cars_new$origin);
cars_new$cylinders = as.factor(cars_new$cylinders);
cars_new$mpg_highway = as.numeric(cars_new$mpg_highway)


## ----Libraries, message=FALSE, warning=FALSE------------------------------------------------------------------------------
library(DescTools)
library(MASS)
library(car)


## ----Exercise 1.A: Step 1-------------------------------------------------------------------------------------------------
table(heart$BP_Status)

boxplot(Cholesterol ~ BP_Status, data=heart, 
        main="Distribution of Cholesterol by BP_Status",
        xlab = "BP_Status",
        ylab = "Cholesterol",
        col = "Grey",
        border = "slategray",
        horizontal = FALSE
        )


## ----Exercise 1.A: Step 2-------------------------------------------------------------------------------------------------
aov.res_heart= aov(Cholesterol~BP_Status, data=heart)

summary(aov.res_heart) #ANOVA result


## ----Exercise 1.A: R-square-----------------------------------------------------------------------------------------------
lm.res_heart = lm(Cholesterol ~ BP_Status, data = heart)

summary(lm.res_heart)$r.squared


## ----Exercise 1.A: Check Equal Variance Assumption------------------------------------------------------------------------
LeveneTest(aov.res_heart)


## ----Exercise 1.A: Check Normality----------------------------------------------------------------------------------------
par(mfrow=c(1,1))# diagnostics plot - in one 
plot(aov.res_heart, 2)


## ----Exercise 1.B---------------------------------------------------------------------------------------------------------
ScheffeTest(aov.res_heart)


## ----Exercise 2.A: Data Exploration---------------------------------------------------------------------------------------
table(bupa$drinkgroup)

boxplot(mcv ~ drinkgroup, data=bupa, 
        main="Distribution of MCV by drinkgroup",
        xlab = "drinkgroup",
        ylab = "mcv",
        col = "Grey",
        border = "slategray",
        horizontal = FALSE
        )


## ----Exercise 2.A: One-Way ANOVA------------------------------------------------------------------------------------------
aov.res_bupa_mcv= aov(mcv~drinkgroup, data=bupa)

summary(aov.res_bupa_mcv) #ANOVA result


## ----Exercise 2.A: R-square-----------------------------------------------------------------------------------------------
lm.res_bupa_mcv = lm(mcv ~ drinkgroup, data = bupa)

summary(lm.res_bupa_mcv)$r.squared


## ----Exercise 2.A: Check Equal Variance Assumption------------------------------------------------------------------------
LeveneTest(aov.res_bupa_mcv)


## ----Exercise 2.A: Check Normality----------------------------------------------------------------------------------------
par(mfrow=c(1,1))# diagnostics plot - in one 
plot(aov.res_bupa_mcv, 2)


## ----Exercise 2.B: Data Exploration---------------------------------------------------------------------------------------
table(bupa$drinkgroup)

boxplot(alkphos ~ drinkgroup, data=bupa, 
        main="Distribution of alkphos by drinkgroup",
        xlab = "drinkgroup",
        ylab = "alkphos",
        col = "Grey",
        border = "slategray",
        horizontal = FALSE
        )


## ----Exercise 2.B: One-Way ANOVA------------------------------------------------------------------------------------------
aov.res_bupa_alkphos= aov(alkphos~drinkgroup, data=bupa)

summary(aov.res_bupa_alkphos) #ANOVA result


## ----Exercise 2.B: R-square-----------------------------------------------------------------------------------------------
lm.res_bupa_alkphos = lm(alkphos ~ drinkgroup, data = bupa)

summary(lm.res_bupa_alkphos)$r.squared


## ----Exercise 2.B: Equal Variance Assumption------------------------------------------------------------------------------
LeveneTest(aov.res_bupa_alkphos)


## ----Exercise 2.B: Check Normality----------------------------------------------------------------------------------------
par(mfrow=c(1,1))# diagnostics plot - in one 
plot(aov.res_bupa_alkphos, 2)


## ----Exercise 2.C---------------------------------------------------------------------------------------------------------
ScheffeTest(aov.res_bupa_mcv)
ScheffeTest(aov.res_bupa_alkphos)


## ----Exercise 3-----------------------------------------------------------------------------------------------------------
## ----Exercise 3.A: Two-Way ANOVA Model (Type 1)---------------------------------------------------------------------------
aov.psych1 = aov(salary ~ sex * rank, data = psych)
aov.psych_3 = aov(salary ~ rank * sex, data = psych)

summary(aov.psych1)
summary(aov.psych_3)


## ----Exercise 3.A: Two-Way ANOVA Model (Type 3)---------------------------------------------------------------------------
Anova(aov.psych1, type = 3)


## ----Exercise 3.A: Variation Explained by the Model-----------------------------------------------------------------------
lm.psych1= lm(salary ~ sex * rank , data = psych)
summary(lm.psych1)$r.squared


## ----Exercise 3.B: Two-Way ANOVA Model (Type 1)---------------------------------------------------------------------------
aov.psych2 = aov(salary ~ sex + rank, data = psych)
aov.psych4 = aov(salary ~ rank + sex, data = psych)

summary(aov.psych2)
summary(aov.psych4)


## ----Exercise 3.B: Two-Way ANOVA Model (Type 3)---------------------------------------------------------------------------
Anova(aov.psych2, type = 3)


## ----Exercise 3.B: Variation Explained by the Model-----------------------------------------------------------------------
lm.psych2= lm(salary ~ sex + rank , data = psych)
summary(lm.psych2)$r.squared


## ----Exercise 3.C---------------------------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(aov.psych2, 2)


## ----Exercise 3.D---------------------------------------------------------------------------------------------------------
TukeyHSD(aov.psych2)


## ----Exercise 4-----------------------------------------------------------------------------------------------------------
## ----Exercise 4.A: Two-Way ANOVA Model (Type 3)---------------------------------------------------------------------------
aov.cars_new1 = aov(mpg_highway ~ cylinders + origin + type, data = cars_new)
Anova(aov.cars_new1, type = 3)


## ----Exercise 4.A: Variation Explained by the Model-----------------------------------------------------------------------
lm.cars_new1= lm(mpg_highway ~ cylinders + type , data = cars_new)
summary(lm.cars_new1)$r.squared


## ----Exercise 4.A: Normality Check----------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(aov.cars_new1, 2)


## ----Exercise 4.B: Two-Way ANOVA Model with Interaction (Type 3)----------------------------------------------------------
aov.cars_new2 = aov(mpg_highway ~ cylinders * type, data = cars_new)

Anova(aov.cars_new2, type = 3)


## ----Exercise 4.B: Variation Explained by the Model-----------------------------------------------------------------------
lm.cars_new2= lm(mpg_highway ~ cylinders * type , data = cars_new)
summary(lm.cars_new2)$r.squared


## ----Exercise 4.B: Normality Check----------------------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(aov.cars_new2, 2)


## ----Exercise 4.C: Post-Hoc Test------------------------------------------------------------------------------------------
TukeyHSD(aov.cars_new2)

