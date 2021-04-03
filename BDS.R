library(MASS)
data(Boston)
attach(Boston)
library(Metrics)
library(caTools)


set.seed(0819)
split = sample.split(medv, SplitRatio = 0.8)
training_set = subset(Boston, split == TRUE)
test_set = subset(Boston, split == FALSE)



#f(x) vector
ans_value = c()
for(i in 1:nrow(training_set)){
  ans_value = append(ans_value,0)
}


#residual vector
last_residual = c(training_set['medv'])

df_inc_lstat = training_set
df_inc_lstat <- df_inc_lstat[order(df_inc_lstat$lstat),]



df_inc_rm = training_set
df_inc_rm <- df_inc_rm[order(df_inc_rm$rm),]



min_lstat = 0
mean_l_lstat = 0
mean_r_lstat = 0
y_pred_lstat = c()
mse_lstat = 0
y_pred_rm = c()
mse_rm =0
min_rm = 0
mean_l_rm = 0
mean_r_rm = 0


lstat_split <- function(){
  

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
  
}


#is_rm,least_rss_val,left_val,right_val
ds <<- matrix(nrow=B,ncol=4,byrow = TRUE )








train_bds <- function(B){
  for(i in 1:B){
    print("starting iteration")
    print(i)
    predict_lstat(training_set)
    predict_rm(training_set)
  
  
    if(mse_rm<mse_lstat){
      ds[i,1] <<- 1
      ds[i,2] <<- min_rm
      ds[i,3] <<- mean_l_rm
      ds[i,4] <<- mean_r_rm
      for(i in 1:nrow(training_set)){
        ans_value[i] <<- (ans_value[i] +  0.01*y_pred_rm[i])
        
        training_set$medv[i] <<- (training_set$medv[i] -0.01*y_pred_rm[i])
        #if(i<10){
        #print(training_set$medv[i])
        #print(0.01*y_pred_rm[i])
        #print(training_set$medv[i])
        #}
      }
    
    
    }else{
      ds[i,1] <<- 0
      ds[i,2] <<- min_lstat
      ds[i,3] <<- mean_l_lstat
      ds[i,4] <<- mean_r_lstat
    
    
      for(i in 1:nrow(training_set)){
        ans_value[i] <<- (ans_value[i] +  0.01*y_pred_lstat[i])
        training_set$medv[i] <<- (training_set$medv[i] -0.01*y_pred_lstat[i])
        #print(i)
        #print(training_set$medv[i])
        #print(0.01*y_pred_lstat[i])
       # print(training_set$medv[i])
        #}
      }
    }
  }

}
train_bds(10)

y_pred_bds <- c()
for(i in 1:nrow(test_set)){
  y_pred_bds <- append(y_pred_bds,0)
}


predict_bds <- function(B){
  for(i in 1:nrow(test_set)){
    for(j in 1:B){
      if(ds[j,1]==0){
        if(test_set$lstat<ds[j,2]){
          y_pred_bds[i] <<- (y_pred_bds[i]+0.01*ds[j,3])
        }else{
          y_pred_bds[i] <<- (y_pred_bds[i]+0.01*ds[j,4])
        }
      }else{
        if(test_set$rm<ds[j,2]){
          y_pred_bds[i] <<- (y_pred_bds[i]+0.01*ds[j,3])
        }else{
          y_pred_bds[i] <<- (y_pred_bds[i]+0.01*ds[j,4])
        }
      }
    }
  }
  mse_bds <<- mse(test_set$medv,y_pred_bds)
  print(mse_bds)
}
predict_bds(10)


