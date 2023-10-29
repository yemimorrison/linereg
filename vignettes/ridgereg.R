## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-----------------------------------------------------------------------------------------------------------------------------------------------------
library(linereg)
library(rminer)
library(caTools)
library(caret)
library(elasticnet)

## ----include=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------------------
library(mlbench)
data(BostonHousing)
split= caTools::sample.split(BostonHousing$medv,SplitRatio = 7/9)
training_set= subset(BostonHousing, split==TRUE)
test_set= subset(BostonHousing,split==FALSE)

## ----include=TRUE,eval=TRUE,echo=TRUE,results='hide'-----------------------------------------------------------------------------------------------------------
intercept<-lm(medv~1, data=training_set)
all<-lm(medv~.,data=training_set)
forward<-step(intercept, direction = 'forward', scope = formula(all),trace=0)
summary(forward)

y_pred = predict(forward,newdata = test_set)


## ----include=TRUE,eval=TRUE,echo=TRUE,results='hide'-----------------------------------------------------------------------------------------------------------
#Root mean squared error
rmse<- mmetric(test_set[,14],y_pred,"RMSE")
#mean absolute percentage error
mape<- mmetric(test_set[,14],y_pred,"MAPE")
#R squared
r2<- mmetric(test_set[,14],y_pred,"R2")

## ----include=TRUE,eval=TRUE,echo=TRUE,results='hide',fig.show='hide'-------------------------------------------------------------------------------------------

model<-linereg::ridgereg(formula=medv~.,data=training_set,lamda=1)

rmse_vec<-c() # root mean squared errors
mape_vec<-c() # mean absolute percentage error
r2_vec<-c() # R squared
mse_vec<-c() # mean squared errors
lamda<-seq(1,25,by=1)
for (i in lamda){
  
   z_model<-ridgereg(formula=medv~.,data=training_set,lamda=i)
   ridge_pred_l<-z_model$predict(test_set)
   rmse_ridge<- mmetric(test_set[,14],ridge_pred_l,"RMSE")
   mse_ridge<- mmetric(test_set[,14],ridge_pred_l,"MSE")
   mape_ridge<- mmetric(test_set[,14],ridge_pred_l,"MAPE")
   r2_ridge<- mmetric(test_set[,14],ridge_pred_l,"R2")
   rmse_vec<-c(rmse_vec,rmse_ridge)
   mape_vec<-c(mape_vec,mape_ridge)
   r2_vec<-c(r2_vec,r2_ridge)
   mse_vec<-c(mse_vec,mse_ridge)
}
rmse_vec
mape_vec
r2_vec
mse_vec
plot(lamda,mse_vec)
plot(lamda,rmse_vec)
plot(lamda,mape_vec)
plot(lamda,r2_vec)


## ----include=TRUE,eval=TRUE,echo=TRUE--------------------------------------------------------------------------------------------------------------------------
c<- trainControl(method='repeatedcv',number=10,repeats = 10)

ridgefit<- train(medv~.,
                data = training_set,
                method='ridge',
                preProcess=c('scale','center'),
                tuneLength=15,
                trControl=c)
ridgefit
plot(ridgefit)


## ----include=TRUE,eval=TRUE,echo=TRUE--------------------------------------------------------------------------------------------------------------------------
#performance of lm 
#Root mean squared error
rmse<- mmetric(test_set[,14],y_pred,"RMSE")
rmse
#mean absolute percentage error
mape<- mmetric(test_set[,14],y_pred,"MAPE")
mape
#R squared
r2<- mmetric(test_set[,14],y_pred,"R2")
r2

# #performance of the ridgereg function
# rmse_vec # root mean squared errors
# mape_vec # mean absolute percentage error
# r2_vec # R squared
# mse_vec # mean squared errors
plot(lamda,mse_vec)
plot(lamda,rmse_vec)
plot(lamda,mape_vec)
plot(lamda,r2_vec)


#performance of 10 cross fold validation after predicting on test data
pre<-predict(ridgefit,test_set)
#Root mean squared error
rmse_10<- mmetric(test_set[,14],pre,"RMSE")
rmse_10
#mean absolute percentage error
mape_10<- mmetric(test_set[,14],pre,"MAPE")
mape_10
#R squared
r2_10<- mmetric(test_set[,14],pre,"R2")
r2_10


