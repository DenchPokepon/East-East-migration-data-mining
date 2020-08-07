## list
L <- list(
  one = 1, 
  two = c(1, 2),
  five = seq(0, 1, length = 5)
) # a list is a more general data structure than a table
# it can store different value types in it which do not necessarily have to be identical in length
# a list may contain another list which may also contain a list and so on
# list can be viewed as a tree. list has different named or unnamed roots (every can contain different value types and data structures)
# if a list contains another list/lists, the root continues/splits into several branches and so on
L$one
L$two
L$five
names(L)
L$five + 10
L$five <- L$five + 10
a <- L[1:2]
L[1]
L[[1]]
is.list(list())
as.list(c(1, 2, 3))
L
unlist(L)
str(unlist(L))
do.call(c, L)

names(L)

c(L[1], L[2], L[3])
c(L[[1]], L[[2]], L[[3]])


L2 <- list(
  node1 = list(num_leaf = c(1, 2, 3)),
  node2 = c("c++", "python"),
  node3 = list(
    sub_node1 = list(char_leaf = "end"),
    sub_node2 = list(NULL)
  )
)

a <- "node1"
b <- "num_leaf"

## simplest data types
"I love R" # character
factor("I love R") # same
1 # numeric
0.9 # numeric
TRUE # boolean
is.numeric(1)
is.logical(TRUE)

NA # not availible
is.na(NA)
NaN # not a number
is.nan(NaN)
NULL # NULL - empty object
is.null(NULL)

L$one <- NULL
L$five <- NULL

Inf # infinity
-Inf


## programming tools

?"==" # relational operators: <, >, >=, <=, ==, !=
?"&" # logical operators: & "and", | "or", ! "not", xor() "only one of x and y"
?"all" # is all TRUE in a logical vector?
?"any" # is any TRUE in a logical vector?
?"which" # which is TRUE in logical vector? returns indexes of all TRUE in vector
?"%in%" # value matching (not order dependent).

2 == 2

2 == 2 & 2 == 1

"R" == c("c++", "c", "python")
"R" == c("c++", "R", "Fortran")
c("c++", "R", "Fortran") == "R"

c("c++", "R", "Fortran") %in% c("ruby", "rust", "R")
c("c++", "R", "Fortran") == c("ruby", "rust", "R")

c("c++", "R", "Fortran")[c("c++", "R", "Fortran") %in% "R"]

2 == 2 | 2 > 3
c(2, 2) == c(2, 2) & c(TRUE, TRUE, TRUE)

2 > 1 | 2 > 3 & 2 > 3
(2 > 1 | 2 > 3) & 2 > 3
2 > 3 & 2 > 3 | 2 > 1
2 > 3 & (2 > 3 | 2 > 1)

all(c(2, 2) == c(2, 2))
any(c(2, 2) == c(2,1))

# for loop

for (i in 1:10) print(i)

i <- 1
while (i != "s") {
  print(i)
  i <- i + 1
}

repeat { # repeats until break statement
  1 + 1
  if (1 + 1 == 2) {
    break
  } else {
    next
  }
}

if (1 == 1) print("1 = 1") else print("math broke")

if (2 + 1 == 4) {
  print("2 + 1 = 4")
} else if (2 + 2 == 5) {
  print("2 + 2 = 5")
} else {
  NULL
}

my.sum <- function(x) {
  res <- 0
  for (i in x) {
    res <- res + i
  }
  return(res)
}
my.sum(x = c(1, 2, 3))

sum(c(1, 2, 3))
my.sum(c(1, 2, 3))

# %>% dplyr package
c(1, 2, 3, 4, 5) %>%
  factorial() %>%
  mean() %>%
  round(0)
