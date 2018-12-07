# FINAL, Problem 2: CDA on Temperatures based on 6 sensors output
# Due Sunday December 21st, 2014
# By Daniel Bejarano

library(robust)
library(caret)
library(pROC)
library(kernlab)
library(frbs)

load("C:/Users/dbejarano/Dropbox/aFALL 2014/DATA ANALYTICS/FINAL/Temperatures.RData")
temp = Temperatures

#Histograms
hist(log(temp$x1))
hist(temp$x2)
hist(temp$x3)
hist(temp$x4)
hist(temp$x5)
hist(temp$x6)
hist(temp$y)
hist(log(temp$y))

#Data simplification
x1 = (temp$x1)
x2 = (temp$x2)
x3 = (temp$x3)
x4 = (temp$x4)
x5 = (temp$x5)
x6 = (temp$x6)
y = temp$y

#Data Transformations
x1t = (temp$x1)^1.5
x2t = (temp$x2)^5
x3t = (temp$x3)^2
x4t = (temp$x4)^1.5
x5t = log(temp$x5)^2
x6t = (temp$x6)

#Inverses of Trans
x1i = x1t
x2i = x2t
x3i = x3t
x4i = x4t
x5i = x5t
x6i = x6t

plot(temp$x1, temp$y)
plot(x1t, temp$y)

#Plots
pairs(temp)

#Linear Regression
y_lm = lm(y ~ x1 + x2 + x3 + x4, data = temp)
summary(y_lm)
plot(y_lm)

#Linear Regression on exponential variables
y_lm_exp = lm(y ~ x1t + x2t + x3t + x4t + x5t)
summary(y_lm_exp)
plot(y_lm_exp)

#Multiplications
y_lm_m = lm(y ~ x1i * x2i * x3i * x4i * x5i * x6t)
summary(y_lm_m)
plot(y_lm_m)
index_train = unlist(createDataPartition(temp$y, p = 0.8))
temp_train = temp[index_train,]
temp_test = temp[-index_train,]
y_lm_train = lm(y ~ x1i * x2i * x3i * x4i * x5i * x6t, data=temp_train)
summary(y_lm_m)

#Best using only two variables
y_lm_2 = lm(y ~ x1t + x6)
summary(y_lm_2)
plot(y_lm_2)

#Polynomial Regression
poly = train(y~poly(x1,5) + poly(x2,5), 
                  data = temp,
                  method = "leapBackward", # linear regression using backward stepping for variable selection
                  tuneGrid = data.frame(nvmax=1:5), # check how many variables we should keep, from 1 to 5
                  trControl = trainControl(method='repeatedcv',repeats=5,returnResamp='all'),
                  selectionFunction=oneSE) # use the one standard error rule for picking the best model

#MARS
marsGrid = expand.grid(.degree=1:4,.nprune=6:15) 
mars = train(y ~ x1 + x2 + x3 + x4 + x5, # use all features except name to make predictions
                  data = temp, # use just the training data
                  method = "earth", # the MARS model
                  tuneGrid = marsGrid, # pass it the grid of tuning parameters to try
                  trControl = trainControl(method='cv',returnResamp='all')) # get all the cross validation results back

#HYFIS
method.type = "HYFIS"
range.data = (temp, 2, range)
control.HYFIS = list(num.labels = 5, max.iter = 50, step.size = 0.01, type.tnorm = "MIN",
                      type.snorm = "MAX", type.defuz = "COG",
                      type.implication.func = "ZADEH", name = "Sim-0")

object.HYFIS = frbs.learn(temp[,1:5], range.data, method.type, control.HYFIS)

