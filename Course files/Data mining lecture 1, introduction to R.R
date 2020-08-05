# symbol "#" makes line a comment, the computer does not interpret this line as a command
# use "#" when you want to write a comment 
# It is considered a good practice to always comment in detail on your actions and their objectives


print("Hello, World")


## set working directory

setwd("C:/Users/denis/Desktop/KOREA") # set the working directory in the directory that is located at the C:/Users/denis/Desktop/KOREA path
# it'll be a different folder for you


## libraries

install.packages("quanteda") # install a package called quanteda, use only once on single computer
library(quanteda) # load a package called quanteda


## help is your friend

help(sum) # shows the documentation for the "sum" function
?sum # shows the documentation for the "sum" function
?quanteda # shows the documentation for the "quanteda" package

# !!! VERY USEFUL FOR BASE R FUNCTIONS  
example(sum) # shows an example of how to use the function
# !!! less useful for functions from packages that have less detailed documentation

help.start() # R manual


## basic calculator
example("+") # arithmetic

?"+"
2 %/% 3 # integer division (целочисленное деление)
4 %% 2 # remainder of integer division (остаток от целочисленного деления)
5 %% 2
2 / 3 # normal division

10^2 + 36 


## variables

a = 1 # assign value 1 (type: numeric) to name/variable "a"
b = a # assign value to which the name "a" is linked to name/variable "b"
b
b = b + 1 # assign value to which the name "b" is linked + 1 to name/variable "b"
b
rm(b) # remove variable from memory (unlink the name and value)

# don't use = when declaring variables
?"="
# use <-
a <- 1
b <- a
b
2 -> b # you can do this, but don't
b
# Rule of thumb - use only "<-" when working with variables, use "=" in function arguments assignments


############# 
#   ToDo
#############

# Compute the sum of 4, 5, 8 and 11 by 
# first combining them into a vector and then using the
# function sum.



############
#
############


## R have a LOT of builtin functions (libraries: base, stat, etc)
?c # Combine Values into a Vector or List
# The vector can only contain values of one type
c(1, "мама", 2) # there's no error, because R automatically converted numbers to characters

# calculate mean
(3 + 4 + 5) / 3 
mean(x = c(3, 4, 5))

rnorm(10) # create 10 numbers from normal distribution
plot(rnorm(100)) # plot 100 numbers from normal distribution, where x is index of each number and y is value of corresponding number

sort(c(1, 2, 1000, 500)) # sort vector
order(c(1, 2, 1000, 500)) # get sorted indexes


## You don't have to write all of your project in 1 script. Use source()
# to create a new R script press cntrl + shift + n
# or click the file button in the upper right corner of the Rstudio interface. New file -> R script
a <- c(1, 2, 3, 4, 5) # assign vector to name/variable a
source("calculated mean.R", encoding = "UTF-8") # execute script that is located there "C:/Users/denis/Desktop/KOREA/calculated mean.R"
# you don't have to write the full path to the file if it's in the work directory because we installed the work directory in the beginning
calculated_mean # variable from "calculated mean.R"

########
# ToDo
########
# Make a file called firstscript.R containing Rcode that generates 100 random numbers and
# plots them, and run this script several times



#######
#
#######

## data stuctures and more mathematics
a <- c(1, 2, 3, 4, 5) # numeric vector
b <- c("мама", "папа", "сын") # character vector
b
mean(b) # mean don't work with characters
a
a <- 1 + a
# what will happen?
a
# "1" is scalar, "a" is vector of type numeric
1:10 == seq(from = 1, to = 10, by = 1) # create sequence of numbers from 1 to 10 with step 1
10:1 == c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1) # create sequence of numbers from 10 to 1 with step 1


a[1:2] # index 1 and 2 elements from vector "a"

a[1:5] # index 1 and 2, and ..., and 5 elements from vector "a"
a[1:length(a)] # length - calculate length of vector "a"
a[2:3]
a[5:1] # index 5 and 4, and ..., and 1 elements from vector "a"
a[c(1, 1, 5, 2)] # index 1, 1, 5, 2 elements from vector "a"
a[c(1, 1, 5, 2)] <- Inf # replace the values located on indices 1, 5, 2 with infinity
a

a <- 1:10
b <- 1:10
sum(a) 
a + b # add up elements of vectors "a" and "b" located on the same indexes
a[16] <- 16 # assign value 16 to 16th element of vector "a"
a # R automatically fills the missing elements with NA (not availible)
a + b # must be same length or at least one should be scalar

a[-1:-15] # remove elements that are within the index range 1:15


########
# ToDo
########
# create vector 2 4 6 8
# calculate the factorial of each element of the resulting vector
# take first and last elements of this result

# print values of first and second maximum of given vectors using max function (googling appreciated): 
c1 <- c(2, 4, 9, 8, 7, 6)
c2 <- c(9, 0, 8, 7, 6, 5, 1)
c3 <- c(0, 6, 5, 7, 9, 1, 4, 10)
c4 <- c(6, 8, 9, 1, 2, 3, 4, 5)

# print values of all even numbers in vectors given in previous task


#######
#
#######


# data.frame

t <- data.frame( # assign object of class data.frame to name/variable t
  x = c(11, 12, 14), # assign vector of values to variable/column "x"
  y = c(19, 20, 21), # same
  z = c(10, 9, 7), # same
  char = c("мама", "папа", "сын"), # same
  stringsAsFactors = FALSE # rule of thumb - always specify this optional argument of data.frame
)
# you can also write options(stringsAsFactors = FALSE) in the start of script to make this specification automatic
t

t[t$char == "мама", 4] # index the rows of column 4 that are equal to a character "mama"
?"$"
t$char # index all of rows of variable/column which name is "char"
t$char[1:2] # index 1 and 2 rows of variable/column which name is "char"
names(t) # extract all names of variables/columns from "t"

names(t) <- c("x", "y", "z", "char") # assign vector of characters to names of variables/columns in "t"
# this function didn't change anything because it assigned the same names


mean(t$x) # calculate average of values of variable "x"

mean(t[, "x"]) # calculate average of values of variable "x"
# $ is a short and simple form of this expression

t[1, "x"]
t[1:3, "x"]
t[1:4, "x"]
t[1:2, c("x", "y")]
t[1:2, 1:2]

t$x[t$x > 12]
is.data.frame(t) # check if object is data.frame class
is.data.frame(c(1, 2, 3)) # vector is not a data.frame
is.vector(c(1, 2, 3)) # vector is vector

# in-depth indexing tutorial (hard)
example("[")

ncol(t) # number of columns in "t"
nrow(t) # number of rows in "t"
dim(t) # dimensions of "t"
ncol(t) * nrow(t) # number of elements (cells) in "t"

rbind(t, t) # combine "t" with "t" by rows
cbind(t, t) # combine "t" with "t" by columns

########
# ToDo
########
df <- data.frame(
  x = c(2, 0, 3, 5, 7, 8, 10),
  y = 1:7
)
# sum vector x and y

# replace vector x with vector y

# index every second variable/column of given df:
set.seed(123) 
df <- matrix(rnorm(100), ncol = 10) %>% 
  as.data.frame()


########
# 
########
