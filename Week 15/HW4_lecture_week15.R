
setwd("C:/Users/sdy897/Box/Working files/Class/UTSA_STA6443_Algorithms1/HW/2020 FALL/HW4")
library(DescTools); library(ResourceSelection)

# Q1 and Q2
liver=read.csv("liver.csv", header=TRUE)
str(liver)

liverF = liver[which(liver$Gender == "Female"),]  # for Q1
liverM = liver[which(liver$Gender == "Male"),]  # for Q2

glm.null.F <- glm(LiverPatient ~ 1, data = liverF, family = "binomial")
glm.full.F <- glm(LiverPatient ~ Age+TB+DB+Alkphos+Alamine+Aspartate+TP+ALB, data = liverF, family = "binomial")


glm.null.M <- glm(LiverPatient ~ 1, data = liverM, family = "binomial")
glm.full.M <- glm(LiverPatient ~ Age+TB+DB+Alkphos+Alamine+Aspartate+TP+ALB, data = liverM, family = "binomial")

fitted.prob <- fitted(glm.full.M, type = "response")


# stepwise selection starting from null model
# Use this one for the HW
# Q2
(step.model.1<-step(glm.null.M, scope = list(upper=glm.full.M),
                      direction="both",test="Chisq", trace = F)) 

summary(step.model.1)

# stepwise selection strating from full model
#(step.model.2<-step(glm.full.M, direction ="both",test="Chisq", trace = F))
#summary(step.model.2)

########################################################################
sleep=read.csv("sleep_new.csv", header=TRUE)
str(sleep)

glm.null.sleep1 <- glm(maxlife10 ~ 1, data = sleep, family = "binomial")

glm.full.sleep1 <- glm(maxlife10 ~ bodyweight+brainweight+totalsleep+gestationtime
                       +as.factor(predationindex)+as.factor(sleepexposureindex), data = sleep, family = "binomial")

step.sleep1 <- step(glm.null.sleep1, scope = list(upper=glm.full.sleep1),
     direction="both",test="Chisq", trace = F)

summary(step.sleep1)

###############################################################
glm.null.sleep2 <- glm(maxlife10 ~ 1, data = sleep, family = "binomial")

glm.full.sleep2 <- glm(maxlife10 ~ bodyweight+brainweight+totalsleep+gestationtime
                       + predationindex + sleepexposureindex, data = sleep, family = "binomial")

step.sleep2 <- step(glm.null.sleep2, scope = list(upper=glm.full.sleep2),
                    direction="both",test="Chisq", trace = F)

summary(step.sleep2)

