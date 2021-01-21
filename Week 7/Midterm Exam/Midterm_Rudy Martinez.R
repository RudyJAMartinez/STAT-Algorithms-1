
# setwd("...")

bweight=read.csv("birthweight.csv", header=TRUE)

bweight$Black=as.factor(bweight$Black);
bweight$Married=as.factor(bweight$Married);
bweight$Boy=as.factor(bweight$Boy);
bweight$MomSmoke=as.factor(bweight$MomSmoke);
bweight$Ed=as.factor(bweight$Ed);

str(bweight)
