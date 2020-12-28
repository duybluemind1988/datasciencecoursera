#install.packages("swirl")
#library("swirl")
#swirl()

x <- list(a = 1:5, b = rnorm(10))
x
lapply(x, mean)
x <- 1:4
lapply(x, runif, min = 0, max = 10)
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
lapply(x, function(elt) { elt[,1] })

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x,mean)
sapply(x,mean)

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
x
f
split(x, f)
lapply(split(x, f), mean)
library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
str(s)
sapply(s, function(x) {
   colMeans(x[, c("Ozone", "Solar.R", "Wind")],na.rm=T)
   })

str(tapply)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
tapply(x, f, mean, simplify = TRUE)
x <- matrix(rnorm(200), 20, 10)
x
apply(x, 2, mean) ## Take the mean of each column
apply(x, 1, sum) ## Take the mean of each row
apply(x, 1, sum)

mapply(rep, 1:4, 4:1)

log(-1)

library(datasets)
data(iris)
?iris
iris
mean(iris$Sepal.Length)
apply(iris[,1:4],2,mean)

library(datasets)
data(mtcars)
mtcars
with(mtcars,tapply(mpg,cyl,mean))
apply(mtcars,2,mean)


sapply(mtcars, cyl, mean)

with(mtcars, tapply(mpg, cyl, mean)) #

split(mtcars, mtcars$cyl)

apply(mtcars, 2, mean)

mean(mtcars$mpg, mtcars$cyl)

tapply(mtcars$cyl, mtcars$mpg, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean) #

lapply(mtcars, mean)

tapply(mtcars$hp, mtcars$cyl, mean) #

library(tidyverse)
a <- mtcars %>% filter(cyl==4) %>% summarize(mean(hp))
b <- mtcars %>% filter(cyl==8) %>% summarize(mean(hp))
a
b
b-a

debug(ls)
ls
