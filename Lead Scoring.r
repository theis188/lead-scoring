
### Code (C) Matthew Theisen 2018. Contact at mtheisen@ucla.edu.

library(ISLR)
library(glmnet)

split_data <- function(df,test_frac) {
  mask <- runif(nrow(df))<test_frac
  train_x = scale( df[!mask,colnames(df)!='Purchase'] ) ## Scaling so that regularization works properly.
  test_x = scale( df[mask,colnames(df)!='Purchase'], 
                  center=attr(train_x,"scaled:center"),
                  scale=attr(train_x,"scaled:scale")) ## Should be scaled separately, since otherwise we are using information from the test set for the train set! A form of data leakage.
  ret <- list(train_x=train_x,
              train_y=df[!mask,c('Purchase')],
              test_x=test_x,
              test_y=df[mask,c('Purchase')])
  return (ret)
}

expit <- function(x) {
  return ( 1/(1+exp(-x)) )
}

logit <- function(x) {
  return( log( x/(1-x) ) )
}

matrix_from_df <- function(x) {
  return (Matrix(as.matrix(x), sparse = FALSE))
}

test_splits <- function(splits) {
  print("Training Rows")
  print(nrow(splits$train_x))
  print("Testing Rows")
  print(splits$test_x)
  print("Training Means")
  print(colMeans(splits$train_x) )
  print("Test Means") 
  print(colMeans(splits$test_x) )
  print("Test SDs")
  print(apply(splits$test_x,2,sd))
  print("Train SDs")
  print(apply(splits$train_x,2,sd))
}

get_confusion_matrix <- function(test,pred) {
  df=data.frame(test=test,pred=pred)
  df$pred <- as.factor(df$pred)
  levels(df$pred)<-c("FALSE","TRUE")
  return( table(df) )
}

get_conf_stats <- function(cm) {
  df <- data.frame(cm)
  precision <- df[( (df$test=="Yes") & (df$pred==TRUE) ),]$Freq / sum( df[( (df$pred==TRUE) ),]$Freq )
  recall <- df[( (df$test=="Yes") & (df$pred==TRUE) ),]$Freq / sum( df[( (df$test=="Yes") ),]$Freq )
  f1 <- 2/(1/precision + 1/recall)
  accuracy <- sum(df[( (df$test=="Yes") == (df$pred==TRUE) ),]$Freq) / sum(df$Freq)
  cost_of_one <- 1
  profit_of_one <- 10
  overall_profit <- profit_of_one*df[( (df$test=="Yes") & (df$pred==TRUE) ),]$Freq - cost_of_one*sum( df[( (df$pred==TRUE) ),]$Freq )
  return (list(precision=precision, recall=recall, f1=f1, accuracy=accuracy, profit=overall_profit))
}


lin_reg_fit <- function(splits) {
  #### Fit & Predict
  fit <- glmnet( matrix_from_df(splits$train_x), splits$train_y, family='binomial')
  train_predict <- predict(fit,newx=matrix_from_df(splits$train_x) )
  test_predict <- predict(fit,newx=matrix_from_df(splits$test_x) )
  
  #### Do quick first tests
  get_confusion_matrix(splits$test_y,test_predict[,100]>0)
  colSums( test_predict > 0 )
  
  #### Find good lambdas at decision boundary = 0.1
  num_pts <- 91
  graphing_df <- data.frame(lambda=rep(0,num_pts), train_f1=rep(0,num_pts), test_f1=rep(0,num_pts) )
  multiplier <- as.integer(100/num_pts)
  i=0
  for ( row in 10:100 ) {
    # row <- multiplier*i
    i=i+1
    test_pred_y <- test_predict[,row] > logit(0.1)
    test_conf_mat <- get_confusion_matrix( splits$test_y,test_pred_y  )
    test_conf_stats <- get_conf_stats(test_conf_mat)
    
    train_pred_y <- train_predict[,row] > logit(0.1)
    train_conf_mat <- get_confusion_matrix( splits$train_y,train_pred_y  )
    train_conf_stats <- get_conf_stats(train_conf_mat)
    
    graphing_df$lambda[i] <- fit$lambda[row]
    graphing_df$test_f1[i] <- test_conf_stats[['f1']]
    graphing_df$train_f1[i] <- train_conf_stats[['f1']]
  }
  plot(graphing_df$lambda,graphing_df$test_f1,xlim=c(0,0.015),ylim=c(0.15,0.3),xlab='Lambda', ylab='Test F1',
       type="l")
  
  #### MOST SIGNIFICANT FACTORS
  row <- 10
  sig_names <- rownames( coef(fit) ) [ coef(fit)[,row]!=0 ]
  coefs <- coef(fit)[,row][sig_names]
  print(coefs)
  
  #### Find Most Profitable Lambdas By Cutoff

  fit_rows <- c(100,50,10)
  cutoffs <- (1:20)/50
  data_storage <- list(low=data.frame(cut=cutoffs,profit=rep(0,20)),
                       middle=data.frame(cut=cutoffs,profit=rep(0,20)),
                       high=data.frame(cut=cutoffs,profit=rep(0,20)) )
  list_labels <- c('low','middle','high')
  for (i in 1:20) {
    cutoff <- cutoffs[i]
    for (k in 1:3) {
      
      row <- fit_rows[k]
      test_pred_y <- test_predict[,row] > logit(cutoff)
      test_conf_mat <- get_confusion_matrix( splits$test_y,test_pred_y  )
      test_conf_stats <- get_conf_stats(test_conf_mat)
      
      data_storage[[list_labels[k]]]$profit[i] <- test_conf_stats[['profit']] 
      
    }
  }
  colors = list(low='red',middle='blue',high='green')
  plot(1,type='n',xlim=c(0,0.4),ylim=c(-200,200),xlab='Probability Cutoff','Profit ($)')
  for (label in list_labels) {
    lines( data_storage[[label]]$cut, data_storage[[label]]$profit, col=colors[[label]] )
  }
  legend(0.3,y=200,legend=c('low','medium','high'),fill=c('red','blue','green'))
}

splits <- split_data(Caravan,0.2)
test_splits(splits)
lin_reg_fit(splits)
