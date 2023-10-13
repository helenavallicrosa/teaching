#### GETTING STARTED ####
#http://www.r-tutor.com/r-introduction
### the guts of R

# assignment and calculator: how to assign values to variables,
# and make calculations with it

x <- 2
x <- 3 #it rewrites on top of the old value
y <- x + 10
z <- x - 2
i <- exp(2) #function exponential

rm(x) #remove variable

### logic
x <- 3
x > y # bigger than
x < y # smaller than
x == y # equal
x != y # non equal

#### 1. Data types ####

#numeric (number)
class(i) #check the data class
is.integer(i) #ask the question if object i is integer

#integer (number no decimals)
ai <- as.integer(i)

#caracter (letters)
z <- lobster #that doesn't work. It would if lobster was already created
z <- c("lobster") # it creates a character variable
class(z)

#### 2. Vectors ####
# Vectors concatenate several individual values in an object
v1 <- c(1,2)
v1 <- c(11,22,33)
v2 <- c("Mon", "Tue", "Wed", "Thu", "Fri")
v3 <- rep(22,2) #function that automatically repeats values several times.
v4 <- 1:100 #from 1 to 100
v5 <- seq(from=1, to=100, by=2) 
v4[20]
v5[c(10,12)] # how to select data from a vector
v5[10,12] # how NOT to do it
v6 <- v2[-3] # eliminates the data on the third slot
v7 <- sample(10:50, 10) #for further details about functions press F1 when pointer on a function
v8 <- rnorm(50, 100, 100) # creates data with normal distribution
v9 <- rnorm(100, 50, 100) # same with other parameters
hist(v9) # change plot appearence, more info in help "hist" page

# summary 
length(v4)
summary(v4)
sum(v1)
min()
max()
mean()

#### 3. Matrix ####
# Data structures in two dimensions, only numbers
mat1 <- matrix(v5, nrow=11, ncol=5) # click on the global environment and visualize
colnames(mat1) <- v2

# navigate matrices
mat1[4,3] #specific slot
mat1[4,] #row
mat1[,3] #column
q <- mat1[,c(1,3)]

#### 4. Data frame ####
# Essentially a matrix that can combine different data types
cars <- mtcars # it combines numeric and characters
length(cars)
ncol(cars)
nrow(cars)
colnames(cars)
cars[,4]
cars$hp

#### 5. List ####
# A vector concatenating data objects of different kinds
all <- list(v1,v2,v3,v4,cars,mat1)
all[6]

#### 6. Exercises ####

# Write a R program to create a sequence of numbers from 20 to 50

# find the mean of the numbers going from 20 to 60

# sum of numbers from 51 to 91

# repeat "I got this" 19 times

# create a matrix of 3 columns and 5 rows filled with numbers from 300 to 335

#Write a R program to create three vectors a,b,c with 3 integers. 
#Combine the three vectors to become a 3×3 matrix where each column 
#represents a vector. Print the content of the matrix.

#More exercises with solutions: https://www.w3resource.com/r-programming-exercises/basic/index.php

#### USING R ####
#### 1. Import data ####

# set directory to shorten your routes
getwd() # which working directory is selected
setwd("/Users/helena/Desktop") # change it to a new one

# data format
NP <- read.csv("~/Desktop/Grassland_data.csv") # change the route between " " according to your files

# Packages are R code containing functions you can use. To install:
# install.packages("openxlsx")
# or go to tools > Install Packages > type the name of the desired package
library(openxlsx) # It charges the package "openxlsx"

T1 <- read.xlsx("~/Desktop/Art. Defoliacio/Manuscrit/v6/Table 1.xlsx") #function from openxlsx package to charge xlsx files

load("~/Desktop/Guaiana Papers/Treball/Above.R") # When the data is in R format

#### 2. Play with data ####
#find and replace tool
#subset
library(plyr)
load("~/Desktop/Guaiana Papers/Treball/Above.R")
summary(Above) #R has its data frames charges in the base by default. Cars is one of them

skimr::skim(Above) # call package just now without charging it into the session with library()

head(Above)
colnames(Above)
unique(Above$topography)
Above[,1]
Above$site
hist(Above$C)
lowC <- subset(Above, Above$C < 50)
hist(lowC$C)

# create a new column with $
data() #all the databases available in R
npk <- npk

npk$all_fert <- paste(npk$N, npk$P, npk$K, sep = "_")

# Exercise: Subset the data by N, just take the N fertilized.
fertN <- subset(npk, npk$N == 1)

# paste df o vectors
rbind()
cbind()

all_fert <- paste(npk$N, npk$P, npk$K, sep = "_")
new <- cbind(npk, all_fert)

#### 3. Create basic models and statistics ####
npk <- npk

# linear model
m1 <- lm(yield ~ N+P+K, data = npk) #apply log(N)
m1
hist(resid(m1), nclass=6)
summary(m1)
boxplot(yield~N, data=npk)

sum <- summary(m1)

sum$coefficients

# glm
m1 <- glm(yield ~ N+P+K, data = npk, family=Gamma(link = "inverse")) #apply log(N)

# Exercise: are there any differences in yield due to its block?
m2 <- lm(yield ~ block, data= npk)
summary(m2)

# linear mixed models
library(nlme)
library(lmerTest)
mm1 <- lme(yield ~ N+P+K, random=~1|block, data=npk, na.action = na.omit) #add na.omit
hist(resid(mm1), nclass=8)
summary(mm1)
a <- summary(mm1)

a$varFix
save(mm1, "~/Desktop/mm1.R")
write.csv(mm1, "")

#### FEELING LIKE A HACKER ####
#### 1. Loops ####

# for
year <- c(2010,2011,2012,2013,2014,2015)
year <- 2010:2015

for (i in year){
  print(paste("The year is", i))
}

# while
i <- 1

while (i < 6) {
  print(i)
  i = i+1
}

# if else
x <- -1

if(x > 0){
  print("Positive number")
} else {
  print("Negative number")
}

#### 2. Functions ####
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

e <- seq(1:30)

kelvin <- celsius_to_kelvin(temp_C=e)

#### 3. Good manners and tricks ####

# format according to the subprocess
# same policy with spaces
# search tool
# cmd + shift + c <- convert all selected text to annotation and viceversa
# click on a key. It will highlight in bold the closing one
# you hide whole section just clicking on the arrow next to its title
# gc() for cleaning memory

#### 4. Practical example 1 ####
library(nlme)
npk <- npk

elem=colnames(npk[2:4])
data=npk
i=1
mixed_m <- function(elem,data){
  out <- data.frame()
  for(i in 1:length(elem)){
    form <- paste("lme(yield ~",elem[i], ", random=~1|block, data=data, na.action=na.omit)") # create model expression
    m1 <- eval(parse(text=form)) # run model expression
    sum <- summary(m1) # save model summary
    sum_mods <- data.frame(sum$tTable, sum$AIC, elem[i]) # select and format into a dataframe the interesting features from summary
    out <- rbind(out, sum_mods) # add the results in the object "out" previously created
  }
  colnames(out)[6:7] <- c("AIC","element") # modify some column names
  return(out) # define the output
}

treatment <- mixed_m(elem=colnames(npk[2:4]), data=npk)
rownames(treatment) <- c("Control N", "N fert", "Control P", "P fert", "Control K", "K fert")

options(scipen=999)
write.csv(treatment, file="~/Desktop/Guaiana Papers/Treball/treatment_mixed_topographyfix.csv")
