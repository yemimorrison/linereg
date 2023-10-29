#' Linreg Class calculates Multiple Linear Regression Model For Data frame.
#'
#' @field formula Dependent and independent columns in data frame
#' @field data Data frame for linear models
#' @field coeffs Estimators matrix converted to vector
#' @field pred_val (matrix). Estimated dependent values
#' @field e (matrix) Error calculated from actual minus calculated dependent variable
#' @field deg_f degrees of freedom
#' @field var_e (matrix) Residual variance
#' @field var_coeffs variance of the regression coefficients
#' @field t_value (matrix) T value
#' @field df_name (character) Data Frame name
#'
#' @return results from the linreg function
#' @exportClass linreg
#' @export linreg
#' @import ggplot2
#' @import methods
#' @importFrom methods new


linreg<-setRefClass('linreg',fields=list(formula='formula',
                                         data ='data.frame',
                                         coeffs='vector',
                                         pred_val='matrix',
                                         e='matrix',
                                         deg_f='numeric',
                                         var_e='matrix',
                                         var_coeffs='vector',
                                         t_value='matrix',
                                         df_name='character'),#creating class and adding all the attributes used inside field list.
                    
                    #adding the methods list, first initiliaze function with formula,data.
                    
                    methods=list(
                      initialize=function(formula=formula(),data=data.frame()){
                        
                        .self$formula<<-formula # assigning the formula and data to attributes
                        .self$data<<-data
                        
                        df_name_a<-deparse(substitute(data)) # getting data.frame name
                        
                        #creating independent variable matrix
                        X<-model.matrix(formula,data) 
                        
                        #getting dependent variable matrix
                        y<-as.matrix(data[,all.vars(formula)[1]]) 
                        
                        #calculating estimator coefficients.
                        coeffs_a<-round(solve(t(X)%*%X)%*%(t(X)%*% y),3)
                        
                        #calculating predicted y values, residuals(error),degree of freedom,
                        #residual variance, variance of the regression coefficients and t value
                        pred_val_a<-X%*%coeffs_a
                        e_a<-y-pred_val_a
                        deg_f_a<-nrow(X)-nrow(coeffs_a)
                        var_e_a<-(t(e_a)%*%e_a)/deg_f_a
                        var_coeffs_a<-c(var_e_a)*diag(solve(t(X)%*%X))
                        t_value_a<-coeffs_a/sqrt(var_coeffs_a)
                        
                        #converting estimators(coeffs) from matrix to vector
                        coeffs_a<-as.vector(coeffs_a)
                        names(coeffs_a)<-colnames(X)# adding names to estimators(coeffs) vector
                        
                        #assignment to attributes
                        .self$coeffs<<-coeffs_a
                        .self$pred_val<<-pred_val_a
                        .self$e<<-e_a
                        .self$deg_f<<-deg_f_a
                        .self$var_e<<-var_e_a
                        .self$var_coeffs<<-var_coeffs_a
                        .self$t_value<<-t_value_a
                        .self$df_name<-df_name_a
                      },
                      
                      #creating a function to return residuals
                      resid=function(){return(c(e))},
                      
                      #getting predicted values
                      pred=function(){return(pred_val)},
                      
                      #returns coefficients(estimators)
                      coef=function(){return(coeffs)},
                      
                      #overwriting print function to get the output same as lm output.
                      print=function(){
                        cat('call:')
                        cat(sep='\n')
                        cat(paste('linreg(formula = ',format(formula),', ' ,'data = ',df_name ,')\n\n',sep=''))
                        cat('Coffiecients:\n')
                        print.default(format(coeffs),
                                      print.gap = 2L,quote=FALSE)},
                      
                      
                      plot=function(){
                        #install and load ggplot2.gridExtra
                        library(ggplot2)
                        library(gridExtra)
                        
                        #creating plots for residual Vs fitted value
                        
                        plot1=ggplot(data.frame(e,pred_val),aes(y=e,x=pred_val))+
                          geom_point(size=2.5,shape=1)+
                          geom_smooth( method = 'lm',linetype='dotted',se=FALSE)+
                          ggtitle('Residuals vs Fitted')+
                          xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
                          ylab('Residuals')+
                          stat_summary(aes(y=e,x=pred_val,group=1),
                                       fun=median,colour='red',geom='line',group=1)+
                          theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))
                        
                        #creating plot for fitted value vs sqrt of standardized residuals
                        
                        stand_e<-sqrt(abs((e-mean(e))/sd(e)))
                        plot2=ggplot(data.frame(stand_e,pred_val),aes(y=stand_e,x=pred_val,group=1))+
                          geom_point(size=3,shape=1)+
                          ggtitle('Scale Location')+
                          xlab(paste('Fitted values\n','linreg(',format(formula),')',''))+
                          ylab(expression(sqrt('|Standardized Residual|')))+
                          theme(plot.title=element_text(hjust=0.5),panel.background = element_rect(fill='white',color = 'black'))+
                          stat_summary(aes(y=stand_e,x=pred_val,group=1),
                                       fun=mean,colour='red',geom='line',group=1)
                        
                        # grid.arrange(plot1,plot2,ncol=2)
                        
                      },
                      
                      #summary returns matrix having estimate, standard error, t value, p value
                      # and also returns residual standard error and degree of freedom
                      
                      summary=function(){
                        n<-length(e)
                        k<-length(coeffs)
                        resid_stand_error<-round(sqrt(sum(e**2)/(n-(k+1))),4)
                        cat('call:')
                        cat(sep='\n')
                        cat(paste('linreg(formula = ',format(formula),', ' ,'data = ',df_name ,')\n\n',sep=''))
                        cat('Coffiecients:\n')
                        coefmatrix<-matrix(coeffs)
                        
                        standard_error<-round(sqrt(var_coeffs),5)
                        p_value<-2*pt(abs(t_value),df=as.numeric(deg_f),lower.tail = FALSE)
                        stars_dashes<-p_value
                        stars_dashes[stars_dashes>0 & stars_dashes<0.001]<- '***'
                        stars_dashes[stars_dashes>0.001 & stars_dashes<0.01]<- '**'
                        stars_dashes[stars_dashes>0.01 & stars_dashes<0.05]<- '*'
                        stars_dashes[stars_dashes>0.05 & stars_dashes<0.1]<- '.'
                        stars_dashes[stars_dashes>0.1 & stars_dashes<1]<- ''
                        coefmatrix<-cbind(coefmatrix,standard_error,round(t_value,2),p_value,stars_dashes)
                        colnames(coefmatrix)<-c('Estimate','Std.Error','t value','Pr(>|t|)','')
                        print.default(coefmatrix,quote=FALSE)
                        cat('\nResidual standard error:',format(resid_stand_error),'on',
                            deg_f,'degrees of freedom')
                      }
                    ))

# creating and initializing the object
# linreg_obj<-linreg$new(formula=Petal.Length ~ Species,data=iris)
# linreg_obj$formula
# linreg_obj$t_value
# linreg_obj$resid()
# linreg_obj$pred_val()
# linreg_obj$coeffs()
# linreg_obj$print()
# linreg_obj$plot()
# linreg_obj$summary()
# linreg_obj$var_coeffs
