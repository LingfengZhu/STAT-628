library("Matrix")
library("foreach")
library(glmnet)



#this is a function to calculate the cross validation
cv = function(data, model){
  num = dim(data)[1]
  var = dim(data)[2]
  l = round(n/10)
  MSE = 1:10
  for(i in 1:10){
    test = (l*(i-1)+1):(l*i)
    if(i == 10){
      test = (l*9+1):num
    }
    model = lm(model, data = data[-test,])
    test_X = data[test, 2:var]
    test_Y = data[test, 1]
    predict_Y = predict(model, test_X)
    MSE[i] = sum((test_Y - predict_Y)^2) / length(test_Y)
  }
  return(mean(MSE))
}

# repeat 10-fold CV 1000 times and then return the mean of MSE
FindCV = function(data,mymodel){
  n=nrow(data)
  cv_mean = 0
  for(i in 1:1000){
    shuffle = sample(1:n, n)
    model_cv = cv(data[shuffle,], mymodel)
    cv_mean = cv_mean + model_cv
  }
  cv_mean = cv_mean/1000
  return(cv_mean)
}

#aic selection
model_full=lm(BODYFAT~.,data=data_new)
model_null=lm(BODYFAT~1, data=data_new)
aic_selection = step(model_null, scope=list(lower=model_null, upper=model_full), direction="both", trace=0, k = 2)$call$formula
print(aic_selection)
# aic_selection

#bic selection
n = dim(data_new)[1]
bic_selection = step(model_full, direction = "both", trace = 0, k = log(n))$call$formula
print(bic_selection)
# bic_selection

#lasso
X = model.matrix(model_full)[,-1]
Y = data_new$BODYFAT
set.seed(123)
cv_lasso = cv.glmnet(X, Y, alpha = 1, family = "gaussian", nfolds = 10)
lasso.coef = coef(cv_lasso, s = cv_lasso$lambda.1se)
print(lasso.coef)
model_lasso = BODYFAT ~ AGE + HEIGHT + ABDOMEN +  WRIST
#lasso and bic are the same

#=====================================
#Diagnosis
fit_aic<- lm(aic_selection,data=data_new)
par(mfrow=c(2,2))
plot(fit_aic)
summary(fit_aic) #r-squared:0.7418


fit_bic<- lm(bic_selection,data=data_new)
plot(fit_bic)
summary(fit_bic) #r-squared:0.7336

#=====================================
#Compare aic, bic
bic_cv_mean<- FindCV(data_new,bic_selection) #15.6591
aic_cv_mean<- FindCV(data_new,aic_selection) #15.4747


#try other models

model_1<- BODYFAT ~ WEIGHT + ABDOMEN
fit_model_1<- lm(model_1,data=data_new)
summary(fit_model_1) #rsq = 0.7181
summary(fit_model_1)$r.squared
par(mfrow=c(2,2))
plot(fit_model_1)

model_2<- BODYFAT ~ ABDOMEN:WEIGHT
fit_model_2<- lm(model_2,data=data_new)
summary(fit_model_2)  #rsq=0.5293
plot(fit_model_2)

model_3<- BODYFAT ~ ABDOMEN
fit_model_3<- lm(model_3,data=data_new)
summary(fit_model_3) #rsq=0.6785
plot(fit_model_3)

#try other models
model_1 = BODYFAT ~ WEIGHT + ABDOMEN
fit_model_1 = lm(model_1,data=data_new)
rsq_1 = summary(fit_model_1)$r.squared
model1_cv = FindCV(data_new, model_1)

model_2 = BODYFAT ~ ABDOMEN:WEIGHT
fit_model_2 = lm(model_2,data=data_new)
rsq_2 = summary(fit_model_2)$r.squared
model2_cv = FindCV(data_new, model_2)

model_3 = BODYFAT ~ ABDOMEN
fit_model_3 = lm(model_3,data=data_new)
rsq_3 = summary(fit_model_3)$r.squared
model3_cv = FindCV(data_new, model_3)

model1_cv<- FindCV(data_new,model_1) #16.2636



