library(MASS)
data(Boston)
attach(Boston)
library(Metrics)
library(caTools)


set.seed(0819)
split = sample.split(medv, SplitRatio = 0.8)
training_set = subset(Boston, split == TRUE)
test_set = subset(Boston, split == FALSE)

min_lstat <- 0
mean_l_lstat <- 0
mean_r_lstat <- 0
y_pred_lstat <- c()
mse_lstat <- 0
y_pred_rm <- c()
mse_rm <-0
min_rm <- 0
mean_l_rm <- 0
mean_r_rm <- 0


lstat_split <- function(){

  df_inc_lstat = training_set
  df_inc_lstat <- df_inc_lstat[order(df_inc_lstat$lstat),]
  min_rss = 99999999
  for(i in 2:nrow(df_inc_lstat)){

    mean_l <- mean(df_inc_lstat['medv'][1:i-1,])
    mean_r <- mean(df_inc_lstat['medv'][i:nrow(training_set),])

    sum_l = 0
    sum_r = 0

    for(j in 2:i-1){
      sum_l = sum_l + (df_inc_lstat['medv'][j:j,]-mean_l)^2
    }
    
    for(j in i:nrow(df_inc_lstat)){
      sum_r = sum_r + (df_inc_lstat['medv'][j:j,]-mean_r)^2
    }

    if(sum_l+sum_r < min_rss){
      min_rss = sum_l+sum_r
      min_lstat <<- df_inc_lstat['lstat'][i:i,]
      mean_l_lstat<<-mean_l
      mean_r_lstat<<-mean_r
    }
    
  }
  
}



rm_split <- function(){
  
  df_inc_rm = training_set
  df_inc_rm <- df_inc_rm[order(df_inc_rm$rm),]
  min_rss = 99999999

  for(i in 2:nrow(df_inc_rm)){
    
    mean_l <- mean(df_inc_rm['medv'][1:i-1,])
    mean_r <- mean(df_inc_rm['medv'][i:nrow(training_set),])
    
    sum_l = 0
    sum_r = 0
    
    for(j in 2:i-1){
      sum_l = sum_l + (df_inc_rm['medv'][j:j,]-mean_l)^2
    }
    
    for(j in i:nrow(df_inc_rm)){
      sum_r = sum_r + (df_inc_rm['medv'][j:j,]-mean_r)^2
    }
    
    if(sum_l+sum_r < min_rss){
      min_rss = sum_l+sum_r
      min_rm <<- df_inc_rm['rm'][i:i,]
      mean_l_rm<<-mean_l
      mean_r_rm<<-mean_r
    }
    
  }
  
}

predict_rm <- function(df){
  rm_split()
  y_pred <- c()
  for(i in 1:nrow(df)){
    if(df['rm'][i:i,]<min_rm){
      y_pred <- append(y_pred,mean_l_rm)
    }else{
      y_pred <- append(y_pred,mean_r_rm) 
    }
  }
  y_pred_rm <<- y_pred
  mse_rm <<- mse(df$medv,y_pred)
  print("mse for decison tree based on rm was")
  print(mse_rm)

}


predict_lstat <- function(df){
  lstat_split()
  y_pred <- c()
  for(i in 1:nrow(df)){
    if(df['lstat'][i:i,]<min_lstat){
      y_pred <- append(y_pred,mean_l_lstat)
    }else{
      y_pred <- append(y_pred,mean_r_lstat) 
    }
  }
  y_pred_lstat <<- y_pred
  mse_lstat <<- mse(df$medv,y_pred)
  
  print("mse for decison tree based on lstat was")
  print(mse_lstat)
  
}


predict_lstat(test_set)
predict_rm(test_set)





