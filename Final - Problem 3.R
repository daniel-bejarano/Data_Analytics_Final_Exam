# FINAL, Problem 3: Predictive Diabetes Model
# Due Sunday December 21st, 2014
# By Daniel Bejarano


library(caret)
library(pROC)
library(kernlab)

load("C:/Users/dbejarano/Dropbox/aFALL 2014/DATA ANALYTICS/FINAL/Diabetes.RData")

#### Part 1: cheap test ####

#Remove first two columns
diab = Diabetes[,c(1,3,4,6,7,8,9)]
diab = na.omit(diab)

#Histograms on all variables
hist(diab$bp)
hist(diab$triceps)
hist(log(diab$insulin))
hist(diab$bmi)
hist(diab$pedigree)
hist(diab$age)
hist(diab$class)
pairs(diab)
hist(diab$age[which(diab$class == "Class1")])

#Data partition
index_train_1 = unlist(createDataPartition(diab$class, p = 0.8))
diab_train = diab[index_train_1,]
diab_test = diab[-index_train_1,]

#Variable Selection
reg_1 = regsubsets(class ~ preg + bp + triceps + bmi + pedigree + age,
                   data = diab_train, nvmax = 6)
summary(reg_1)

#TRAIN the data using logistic regression
lrFit_1 = train(class ~  bmi + pedigree + age, 
               data = diab_train,
               method = "plr", metric = "ROC",
               trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                        summaryFunction = twoClassSummary, classProbs = TRUE)) 
lrFit_1
summary(lrFit_1)

#TRAIN the data using support vector machines
svmFit_1 = train(class ~ bmi + pedigree + age + preg + triceps,
                data = diab_train,
                method = "svmRadial", tuneLength = 8, metric = "ROC",
                preProc = c("center", "scale"), 
                trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                         summaryFunction = twoClassSummary, classProbs = TRUE))
svmFit_1
summary(svmFit_1)

#PREDICT as Class1 or Class2 (factor)
pred_lr_1 = predict(lrFit_1, diab_test)
pred_svm_1 = predict(svmFit_1, diab_test)

#PREDICT with probabilities of Class 1 or Class2 
prob_lr_1 = predict(lrFit_1, diab_test, type = "prob") 
prob_svm_1 = predict(svmFit_1, diab_test, type = "prob") 

#create data frame contianing the actual classification and the two predictions for test
results = data.frame(actual = as.factor(diab$class), log_reg = as.factor(pred_lr_1), 
                      svm = as.factor(pred_svm_1))

#test whether each model predicted the class accurately or not
lr_Fit_test_1 = NULL
svm_Fit_test_1= NULL
for (i in 1:length(pred_lr_1)){
  if(diab_test$class[i] == pred_lr_1[i]){
    lr_Fit_test_1[[i]] = 1}
  else {lr_Fit_test_1[[i]] = 0}
  
  if(diab_test$class[i] == pred_svm_1[i]){
    svm_Fit_test_1[[i]] = 1}
  else {svm_Fit_test_1[[i]] = 0}
}

#obtianing the ratio of times it predicted correctly over the total amount of predictions
lr_error_1 = sum(lr_Fit_test_1)/length(lr_Fit_test_1)
svm_error_1 = sum(svm_Fit_test_1)/length(svm_Fit_test_1)

#Select only the probability of it being Class 1 (we are only interested in this)
predict_lr_1 = prob_lr_1$Class1
predict_svm_1 = prob_svm_1$Class1

#ROC of linear regression
lr_ROC_1 = roc(response = diab_test$class, predictor = predict_lr_1, levels = rev(levels(diab_test$class)),
              smooth=TRUE)
plot(lr_ROC_1, main = "LR ROC")
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#ROC of support vector machines
svm_ROC_1 = roc(response = diab_test$class, predictor = predict_svm_1, levels = rev(levels(diab_test$class)), 
               smooth=TRUE)
plot(svm_ROC_1, main = "SVM ROC", xlim = c(1.0, 0.0))
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#Fitting on entire dataset
lrFinal_1 = train(class ~ preg + bp + triceps + bmi + pedigree + age,
                 data = diab,
                 method = "plr")

svmFinal_1 = train(class ~ preg + bp + triceps + bmi + pedigree + age,
                  data = diab,
                  method = "svmRadial", tuneGrid = svmFit_1$bestTune, metric = "ROC", ##CHECK ROC CURVE FIRST
                  preProc = c("center", "scale"), 
                  trControl = trainControl(method="none", classProbs = TRUE)) 


#### PART 2: Model with two variables only #### 

#Variable Selection
reg_2 = regsubsets(class ~ preg + bp + triceps + bmi + pedigree + age,
                   data = diab_train, nvmax = 2)
summary(reg_2)

#TRAIN the data using logistic regression
lrFit_2 = train(class ~  bmi + age, 
                data = diab_train,
                method = "plr", metric = "ROC",
                trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                         summaryFunction = twoClassSummary, classProbs = TRUE)) 
lrFit_2
summary(lrFit_2)

#TRAIN the data using support vector machines
svmFit_2 = train(class ~ bmi + age,
                 data = diab_train,
                 method = "svmRadial", tuneLength = 8, metric = "ROC",
                 preProc = c("center", "scale"), 
                 trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                          summaryFunction = twoClassSummary, classProbs = TRUE))
svmFit_2
summary(svmFit_2)

#PREDICT as Class1 or Class2 (factor)
pred_lr_2 = predict(lrFit_2, diab_test)
pred_svm_2 = predict(svmFit_2, diab_test)

#PREDICT with probabilities of Class 1 or Class2 
prob_lr_2 = predict(lrFit_2, diab_test, type = "prob") 
prob_svm_2 = predict(svmFit_2, diab_test, type = "prob") 

#Select only the probability of it being Class 1 (we are only interested in this)
predict_lr_2 = prob_lr_2$Class1
predict_svm_2 = prob_svm_2$Class1

#ROC of linear regression
lr_ROC_2 = roc(response = diab_test$class, predictor = predict_lr_2, levels = rev(levels(diab_test$class)),
               smooth=TRUE)
plot(lr_ROC_2, main = "LR ROC")
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#ROC of support vector machines
svm_ROC_2 = roc(response = diab_test$class, predictor = predict_svm_2, levels = rev(levels(diab_test$class)), 
                smooth=TRUE)
plot(svm_ROC_2, main = "SVM ROC", xlim = c(1.0, 0.0))
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#### PART 3: Complete Test (expensive) #### 

diabetes_clean = na.omit(Diabetes)
diabetes_na = subset(Diabetes, is.na(preg) | is.na(glucose) | is.na(bp) | is.na(triceps) | is.na(insulin) | 
                       is.na(bmi) | is.na(pedigree) | is.na(age) | is.na(class))

#ONLY NAs in 2, 3, 4, 5 and 6 columns in Diabetes
#GLUCOSE ### 
glucose_na = subset(Diabetes, is.na(glucose))
lrFit_glu = train(glucose ~ preg + bp + triceps + bmi + pedigree + age + class,
                data = diabetes_clean,
                method = "plr",
                trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, summaryFunction=defaultSummary)) 

glucose_sd = sd(diabetes_clean$glucose) #standard deviation of glucose readings
diabetes_pred = diabetes_na
for (i in 1:nrow(diabetes_pred)){
  if(is.na(diabetes_pred[i,2]) == TRUE){diabetes_pred[i,2] = predict(lrFit_glu, diabetes_pred[i,])}}

#BP - Code is the same as above, so no description was written for the remaining variables
bp_na = subset(diabetes_pred, is.na(bp))
lrFit_bp = train(bp ~ preg + glucose + pedigree + age + class,
                  data = diabetes_clean,
                  method = "glm",
                  trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, summaryFunction=defaultSummary)) 
summary()
bp_sd = sd(diabetes_clean$bp)
for (i in 1:nrow(diabetes_pred)){
  if(is.na(diabetes_pred[i,3]) == TRUE){diabetes_pred[i,3] = predict(lrFit_bp, diabetes_pred[i,])}}

#TRICEPS
triceps_na = subset(diabetes_pred, is.na(triceps))
lrFit_triceps = train(triceps ~ preg + glucose + bp + pedigree + age + class,
                 data = diabetes_clean,
                 method = "glm",
                 trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, summaryFunction=defaultSummary)) 

triceps_sd = sd(diabetes_clean$triceps)
for (i in 1:nrow(diabetes_pred)){
  if(is.na(diabetes_pred[i,4]) == TRUE){diabetes_pred[i,4] = predict(lrFit_triceps, diabetes_pred[i,])}}

#INSULIN
insulin_na = subset(diabetes_pred, is.na(insulin))
lrFit_insulin = train(insulin ~ preg + glucose + bp + triceps + pedigree + age + class,
                      data = diabetes_clean,
                      method = "glm",
                      trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, summaryFunction=defaultSummary)) 

insulin_sd = sd(diabetes_clean$insulin)
for (i in 1:nrow(diabetes_pred)){
  if(is.na(diabetes_pred[i,5]) == TRUE){diabetes_pred[i,5] = predict(lrFit_insulin, diabetes_pred[i,])}}

#BMI
bmi_na = subset(diabetes_pred, is.na(bmi))
lrFit_bmi = train(bmi ~ preg + glucose + bp + triceps + insulin + pedigree + age + class,
                      data = diabetes_clean,
                      method = "glm",
                      trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, summaryFunction=defaultSummary)) 

bmi_sd = sd(diabetes_clean$bmi)
for (i in 1:nrow(diabetes_pred)){
  if(is.na(diabetes_pred[i,6]) == TRUE){diabetes_pred[i,6] = predict(lrFit_bmi, diabetes_pred[i,])}}

#Putting together the originally clean data with the one filled with the predicted values
diab_all = rbind(diabetes_clean, diabetes_pred)

## RUNNING MODELS ON NA-LESS DATASET ###

index_train_all = unlist(createDataPartition(diab_all$class, p = 0.8))
diab_train_all = diab_all[index_train_all,]
diab_test_all = diab_all[-index_train_all,]

#Variable Selection
reg_all = regsubsets(class ~ preg + glucose + bp + triceps + insulin + bmi + pedigree + age,
                   data = diab_train_all, nvmax = 8)
summary(reg_all)

#TRAIN the data using logistic regression
lrFit_all = train(class ~ preg + glucose + bmi + pedigree + insulin,
                #preg + glucose + bp + triceps + insulin + bmi + pedigree + age
                data = diab_train_all,
                method = "plr", metric = "ROC",
                trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                         summaryFunction = twoClassSummary, classProbs = TRUE)) 

#TRAIN the data using support vector machines
svmFit_all = train(class ~ preg + glucose + bmi + pedigree + insulin,
                 data = diab_train_all,
                 method = "svmRadial", tuneLength = 8, metric = "ROC",
                 preProc = c("center", "scale"), 
                 trControl = trainControl(method='repeatedcv', number = 10, repeats = 5, 
                                          summaryFunction = twoClassSummary, classProbs = TRUE))

#PREDICT as Class1 or Class2 (factor)
pred_lr_all = predict(lrFit_all, diab_test_all)
pred_svm_all = predict(svmFit_all, diab_test_all)

#PREDICT with probabilities of Class 1 or Class2 
prob_lr_all = predict(lrFit_all, diab_test_all, type = "prob") 
prob_svm_all = predict(svmFit_all, diab_test_all, type = "prob") 

#test whether each model predicted the class accurately or not
lr_Fit_test_all = NULL
svm_Fit_test_all = NULL
for (i in 1:length(pred_lr_all)){
  if(diab_test_all$class[i] == pred_lr_all[i]){
    lr_Fit_test_all[[i]] = 1}
  else {lr_Fit_test_all[[i]] = 0}
  
  if(diab_test_all$class[i] == pred_svm_all[i]){
    svm_Fit_test_all[[i]] = 1}
  else {svm_Fit_test_all[[i]] = 0}
}

#obtianing the ratio of times it predicted correctly over the total amount of predictions
lr_error_all = sum(lr_Fit_test_all)/length(lr_Fit_test_all)
svm_error_all = sum(svm_Fit_test_all)/length(svm_Fit_test_all)

#Select only the probability of it being Class 1 (we are only interested in this)
predict_lr_all = prob_lr_all$Class1
predict_svm_all = prob_svm_all$Class1

#ROC of linear regression
lr_ROC_all = roc(response = diab_test_all$class, predictor = predict_lr_all, levels = rev(levels(diab_test_all$class)),
               smooth=TRUE)
plot(lr_ROC_all, main = "LR ROC")
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#ROC of support vector machines
svm_ROC_all = roc(response = diab_test_all$class, predictor = predict_svm_all, levels = rev(levels(diab_test_all$class)), 
                smooth=TRUE)
plot(svm_ROC_all, main = "SVM ROC")
abline(v=0.2, col = "purple")
abline(h=1, col = "blue")

#Fitting on entire dataset
lrFinal_all = train(class ~ preg + glucose + bp + triceps + insulin + bmi + pedigree + age,
                  data = diab_all,
                  method = "plr")

svmFinal_all = train(class ~ preg + glucose + bp + triceps + insulin + bmi + pedigree + age,
                   data = diab_all,
                   method = "svmRadial", tuneGrid = svmFit_1$bestTune, metric = "ROC", 
                   preProc = c("center", "scale"), 
                   trControl = trainControl(method="none", classProbs = TRUE)) 

#PRINCIPAL COMPONENT ANALYSIS

#Question 2 - principal components and cummulative variance
pca_diab =  princomp(diab_all[c(1,2,3,4,5,6,7,8)], scores=TRUE, cor=FALSE)

#print(summary(pca_edata))
#print(pca_edata$loadings)

explainedvar = cumsum((pca_diab$sdev)^2)
plot(explainedvar/explainedvar[9], ylab="Cumulative Variance", xlab="Principal Vectors")	

#Question 3 - First 2 principal components
plot(pca_diab$scores[,1], pca_diab$scores[,2], ylab="Component 2", xlab="Component 1")
