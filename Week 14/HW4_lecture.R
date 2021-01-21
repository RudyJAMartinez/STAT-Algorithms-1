#https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r

setwd("C:/Users/sdy897/Box/Working files/Class/UTSA_STA6443_Algorithms1/HW/2020 FALL/HW4")
library(DescTools); library(ResourceSelection)

liver=read.csv("liver.csv", header=TRUE)
str(liver)

liverF = liver[which(liver$Gender == "Female"),]
liverM = liver[which(liver$Gender == "Male"),]

glm.full.F <- glm(LiverPatient ~ Age+TB+DB+Alkphos+Alamine+Aspartate+TP+ALB, data = liverF, family = "binomial")


glm.null.M <- glm(LiverPatient ~ 1, data = liverM, family = "binomial")
glm.full.M <- glm(LiverPatient ~ Age+TB+DB+Alkphos+Alamine+Aspartate+TP+ALB, data = liverM, family = "binomial")

fitted.prob <- fitted(glm.full.M, type = "response")


# stepwise selection starting from null model
# Use this one for the HW
(step.model.1<-step(glm.null.M, scope = list(upper=glm.full.M),
                      direction="both",test="Chisq", trace = F)) 

summary(step.model.1)

# stepwise selection strating from full model
#(step.model.2<-step(glm.full.M, direction ="both",test="Chisq", trace = F))
#summary(step.model.2)

sleep=read.csv("sleep.csv", header=TRUE)
str(sleep)





