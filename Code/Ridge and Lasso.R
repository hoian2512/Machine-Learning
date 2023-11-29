#Library
library(ggplot2)
library(lattice)
library(caret)
library(ISLR2)
library(Matrix)
library(glmnet)

#Hitter data
hitter_data = read.csv("Hitters.csv", header = TRUE)
View(hitter_data)
names(hitter_data)

#Check missing value 1
dim(hitter_data)
sum(is.na(hitter_data$Salary))

#Processing missing value
hitter_data = na.omit(hitter_data)
dim(hitter_data)

#Check missing value 2
sum(is.na(hitter_data))

#ridge
x = model.matrix(Salary ~ ., hitter_data)[, -1]
y = hitter_data$Salary

grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
