# Set working directory
setwd("./Desktop/R_workspace/")
getwd()

# Assign, help, arithmetic 
x = 1:10
x
mean(x)
help("mean")
?mean
x <- 10
x
x = 1
x
375/27
375%/%27

# Built-in Data, correlation
help(USArrests)
USArrests
plot(USArrests)
str(USArrests)
cor(USArrests)

?cars
cars
plot(cars)
str(cars)

?lm
lm(dist~speed, data=cars)

# Data Frame
C = seq(0, 100, 10)
C
F = C*9/5 + 32
F
T = data.frame(Celsius=C, Fahrenheit=F)

# List of all variables in current state & Save current datas into file
ls()
save.image(file="myscript.RData")

# Array creation in easy ways
seq(0, 100, 10)
rep(1:3,2)

# Remove current vairables by remove()
?remove
rm(list=ls())

# Load data from "~.RData"
?load
load(file="myscript.RData")

# Built-in functions of array : rank, rev, unique, length, class
x1 = c(3, 1, 4, 15, 92)
rank(x1)
x2 = c(3, 1, 4, 6, 5, 9) 
rev(x2)
x3 = c(3:5, 11:8, 8 + 0:5)
x3
unique(x3)
length(x3)
class(x3)

# homogeneous 1d : vector
vector1 <- c(10,20,30,40,50,60) #Numeric Vector 
vector1
vector2 <- c('15','A',"Henry") #Character Vector
vector2
vector3 <- c(T,F,FALSE,TRUE) #Logical vector
vector3

## vector arithmetic / we can put vector as index of another vector
vector1^2
s = c('a','b','c','d','x')
s
s[c(1,2,4)]
s[1:3]

# homogeneous 2d : matrix
A <- matrix(c(9,2,9,8,8,5,10,8,8,10,3,4),nrow=4)
A
colnames(A) <- c("Plan","Analysis","Writing")
rownames(A) <- c("S1","S2","S3","S4")
A
dim(A)

## visualization of matrices
?barplot
barplot(A)
barplot(A, beside=TRUE)
barplot(A, beside=TRUE, legend=TRUE)
barplot(A, beside=TRUE, legend=TRUE, col=1:2)
barplot(A, beside=TRUE, legend=TRUE, col=1:4)

# homogeneous 3d : array
array1 <- array(1:18, dim=c(3,3,2))
array1
dim(array1)
array1[3,3,2]

# heterogeneous 1d : list
n= c(4, 5, 7, 12) 
s = c("and", "baby", "you", "zoo") 
b = c(TRUE, FALSE, TRUE, F, T) 
x = list(n, s, b)
x
x[[2]][2]

# heterogeneous 2d : data frame
head(ChickWeight)

## Declaration of data frame
Name <- c("Mercury", "Venus","Earth","Mars","Jupiter","Saturn", "Uranus", "Neptune")
Diameter <- c(0.382,0.949,1.0,0.532,11.209,9.449,4.007,3.883)
Ring <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
OurPlanets <- data.frame(Name, Diameter, Ring)
class(OurPlanets)
OurPlanets

?factor
dat = c(1,2,3,3,2,1,3,2,3,2,1,2,3,1,2,3,2,1,2,2)
dat 
fct = factor(dat, levels=1:3, labels = c("Small", "Medium", "Large"))
fct
plot(fct, col=2:4)
str(fct)
class(fct)
table(fct)
mode(fct)
numbers <- factor(c(9,8,10,8,9))
str(numbers)

head(state.x77)
?state.x77
class(state.x77)
dim(state.x77)
str(state.x77)
temp <- as.data.frame(state.x77)
dim(temp)
temp
class(temp)
