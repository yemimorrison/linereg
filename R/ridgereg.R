#' Ridgereg Class calculates Ridge Regression Model For Data frame.
#'
#' @field formula (formula) Dependent and independent columns in data frame
#' @field data (data.frame) Data frame for ridge regression models
#' @field coeffs_ridge (vector) calculates Beta ridge coefficients
#' @field pred_val (matrix) Estimated dependent values
#' @field X (matrix) model matrix which is scaled later
#' @field df_name (character) Data Frame name
#' @field lamda (numeric) hyper parameter value
#'
#' @return a vector of predicted values
#' @exportClass ridgereg
#' @export ridgereg
#' @importFrom MASS lm.ridge

ridgereg<-setRefClass('ridgereg', 
                      fields=list(formula='formula',
                                  data ='data.frame',
                                  coeffs_ridge='vector',
                                  pred_val='matrix',
                                  X='matrix',
                                  df_name='character',
                                  lamda='numeric'),
                      
                      #creating class and adding all the attributes used inside field list.
                      
                      #adding the methods list, first initiliaze function with formula,data.
                      
                      methods=list(
                        initialize=function(formula=formula(),data=data.frame(),lamda=numeric()){
                          
                          .self$formula<<-formula # assigning the formula and data to attributes
                          .self$data<<-data
                          .self$lamda<<-lamda
                          
                          df_name<<-deparse(substitute(data)) # getting data.frame name
                          
                          #creating independent variable matrix
                          X<<-model.matrix(formula,data)
                          X[,-1]<<-(scale(X[,-1]))
                          
                          
                          #getting dependent variable matrix
                          Y<-as.matrix(data[,all.vars(formula)[1]])
                          
                          
                          #calculating estimator coefficients.
                          
                          coeffs_ridge_a<-solve((t(X)%*%X+diag(ncol(X))*lamda))%*%t(X)%*% Y
                          
                          #calculating predicted y values, residuals(error),degree of freedom,
                          #residual variance, variance of the regression coefficients and t value
                          pred_val_a<-X%*%coeffs_ridge_a
                          
                          #converting estimators(B_h) from matrix to vector
                          coeffs_ridge_a<-as.vector(coeffs_ridge_a)
                          names(coeffs_ridge_a)<-colnames(X)# adding names to estimators(B_h) vector
                          
                          .self$coeffs_ridge<<-coeffs_ridge_a
                          .self$pred_val<<-pred_val_a
                        },
                        predict=function(newdata){
                          x_m<-model.matrix(formula,newdata)
                          x_m[,-1]<-(scale(x_m[,-1]))
                          y_f<-x_m %*% coeffs_ridge
                          return(y_f)
                        },
                        
                        #returns coefficients(estimators)
                        coef=function(){
                          return(coeffs_ridge)},
                        
                        show=function(){
                          cat('call:')
                          cat(sep='\n')
                          cat(paste('ridgereg(formula = ',format(formula),', ' ,'data = ',df_name ,')\n\n',sep=''))
                          cat('Coffiecients:\n')
                          print.default(format(coeffs_ridge),
                                        print.gap = 2L,quote=FALSE)}
                      ))

#ridgereg_obj<-ridgereg$new(formula=Petal.Length~Species,data=iris,lamda=4)
#ridgereg_obj
#ridgereg_obj$predict(iris)
#ridgereg_obj$coef()
#print(ridgereg_obj)

