library(MASS)
summary(Boston)
attach(Boston)

# Chapter 2, #10
pairs(Boston)

plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)

hist(Boston$crim, 25)
median(Boston$crim)
range(Boston$crim)
quantile(Boston$crim, 0.95)

hist(Boston$tax, 25)
median(Boston$tax)
range(Boston$tax)

hist(Boston$ptratio, 25)

sum(Boston$chas)

median(Boston$ptratio)

subset(Boston, medv == min(Boston$medv))
highlight <- function(x, value, col.value, col=NA, ...){
  hst <- hist(x, ...)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col=cols, ...)
}
highlight(Boston$crim, Boston$crim[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$zn, Boston$zn[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$indus, Boston$indus[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$chas, Boston$chas[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$nox, Boston$nox[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$rm, Boston$rm[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$age, Boston$age[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$dis, Boston$dis[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$rad, Boston$rad[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$tax, Boston$tax[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$ptratio, Boston$ptratio[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$black, Boston$black[Boston$medv == min(Boston$medv)], "red")
highlight(Boston$lstat, Boston$lstat[Boston$medv == min(Boston$medv)], "red")

nrow(subset(Boston, rm > 7))
nrow(subset(Boston, rm > 8))
summary(subset(Boston, rm > 8))
summary(Boston)

# Chapter 3, #15

lm.zn = lm(crim~zn)
summary(lm.zn)
#yes

lm.indus = lm(crim~indus)
summary(lm.indus)
#yes

lm.chas = lm(crim~chas)
summary(lm.chas)
#no

lm.nox = lm(crim~nox)
summary(lm.nox)
#yes

lm.rm = lm(crim~rm)
summary(lm.rm)
#yes

lm.age = lm(crim~age)
summary(lm.age)
#yes

lm.dis = lm(crim~dis)
summary(lm.dis)
#yes

lm.rad = lm(crim~rad)
summary(lm.rad)
#yes

lm.tax = lm(crim~tax)
summary(lm.tax)
#yes

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)
#yes

lm.black = lm(crim~black)
summary(lm.black)
#yes

lm.lstat = lm(crim~lstat)
summary(lm.lstat)
#yes

lm.medv = lm(crim~medv)
summary(lm.medv)
#yes

lm.all = lm(crim~., data = Boston)
summary(lm.all)

univariate = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
multiple = coefficients(lm.all)[2:14]
plot(univariate, multiple)

lm.zn = lm(crim~poly(zn,3))
summary(lm.zn)
#1, 2

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) 
#1, 2, 3

lm.chas = lm(crim~poly(chas,3))

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3

##Chapter 6, #9

library(ISLR)
attach(College)
set.seed(2015)

train = data.frame(College)
test = data.frame(College)

tr = sample(1:nrow(College), 0.7*nrow(College))

college.train = train[tr,]
college.test = test[-tr,] 

lm.training = lm(Apps~., data = college.train)
summary(lm.training)

lm.predicted = predict(lm.training, college.test)
mean((college.test[, 2] - lm.predicted)^2)

library(glmnet)

college.mat <- model.matrix(Apps~., data = College)

ridge.college = cv.glmnet(college.mat[tr,], college.train[, 2], alpha=0)
ridge.best = ridge.college$lambda.min

ridge.pred = predict(ridge.college, newx=college.mat[-tr,], s=ridge.best)
mean((college.test[, 2] - ridge.pred)^2)

lasso.college = cv.glmnet(college.mat[tr,], college.train[, 2], alpha=1)
lasso.best = lasso.college$lambda.min

lasso.pred = predict(lasso.college, newx=college.mat[-tr,], s=lasso.best)
mean((college.test[, 2] - lasso.pred)^2)

lasso.college = glmnet(model.matrix(Apps~., data=College), College[, 2], alpha=1)
predict(lasso.college, s=lasso.best, type="coefficients")

library(pls)

pcr.fit = pcr(Apps~., data=college.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, college.test, ncomp=10)
mean((college.test[, "Apps"] - data.frame(pcr.pred))^2)

test.avg = mean(college.test[, "Apps"])
lm.test.r2 = 1 - mean((college.test[, "Apps"] - lm.predicted)^2) /mean((college.test[, "Apps"] - test.avg)^2)

#Chapter 6, #11
library(MASS)
library(glmnet)

set.seed(2015)

x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)
coef(cv.lasso)

sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

set.seed(2015)

x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)

coef(cv.ridge)

sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

#Chapter 8, #8
library(ISLR)
attach(Carseats)

set.seed(2015)
train = sample(dim(Carseats)[1], dim(Carseats)[1]*2/3)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

pruned.carseats = prune.tree(tree.carseats, best = 5)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)

library(randomForest)
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

importance(bag.carseats)

rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 6, ntree = 500, importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

importance(rf.carseats)

#Chapter 8, #11
library(ISLR)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]
library(gbm)
set.seed(342)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, distribution = "bernoulli")


library(ISLR)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train,]
Caravan.test = Caravan[-train,]

library(gbm)

set.seed(2015)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, distribution = "bernoulli")
summary(boost.caravan)

boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)

lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)

#Problem 1
beauty <- read.csv("BeautyData.csv")
attach(beauty)
lm.beauty = lm(CourseEvals~., data = beauty)
summary(lm.beauty)

#Problem 2
midcity <- read.csv("MidCity.csv")
attach(midcity)
lm.brick = lm(Price~., data = midcity)
summary(lm.brick)
