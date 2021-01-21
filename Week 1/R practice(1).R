# R practice #

################################################
# Install package and call
################################################
# 1. Package installation - required once with, e.g., install.packages("MASS")
# 2. Once it is installed we need to call it for the use e.g., library("MASS")
# note: some packages require installation of dependnet packages

install.packages("MASS")  # need only once for your computer
library("MASS")  # need for package loading  

#################################################
# data import
#################################################

## 1. built-in data
?airquality  # information about dataset

View(airquality) 
dim(airquality)  # the number of observations and variables
class(airquality) # format
str(airquality) # structure
head(airquality) # head of the data
tail(airquality) # tail of the data
?str

data()  # other R built-in data 

## 2. import excel or csv file
setwd("/Users/rudymartinez/Desktop/MSDA/Fall 2020/STA 6443_Algorithms I/STAT-Algorithms-1/Week 1")  # need to change this path

prof=read.csv("Professor.csv", header=TRUE)

View(prof)
dim(prof)  
class(prof) 
str(prof) 

install.packages("readxl")
library("readxl")
cars=read_xlsx("CARS.xlsx")

## 3. import sas or spss file also possible - with propoer library installed

##########################################
# quick practice
##########################################

# R is case-sensitive 

# extract SALARY information as a vector 
prof$SALARY
prof[ ,"SALARY"]
prof[ ,4]

# data format (numeric/ factor/ character)
class(prof$SALARY); class(prof$Gender)

table(prof$Gender)  # for factor variable
plot(prof$PUBS, prof$SALARY)   # scatter plot

attach(prof)  # attach R objects - no need to 
table(Gender)
plot(PUBS, SALARY)
detach(prof)   # don't forget detach()

new.var=log(prof$SALARY)  # define new variable
prof=cbind(prof,new.var) 
View(prof)

############################################
# data split or filtering
###########################################

# split the data - Female and Male
prof.female=prof[prof$Gender=="Female", ]
prof.male=prof[prof$Gender=="Male", ]

# filter the data - observations with SALARY <50,000
prof.sub1=prof[prof$SALARY < 50000, ]

# delete observation with SALARY < 50,000
tmp.id=which(prof$SALARY < 50000)
prof.sub2=prof[-tmp.id, ]

# leave only two varialbes - PUBS and SALARY
prof.sub3=prof[ ,c("PUBS","SALARY")]
prof.sub4=prof[ ,c(2,4)]

# delete observation with 

######################################
# logical 
#####################################

a = c(1,2,3,4,6)
b = c(0,2,3,5,6)

sum(a==b) # number of same elements (element-wise comparison; location matters)
sum(a!=b) # number of different elements

