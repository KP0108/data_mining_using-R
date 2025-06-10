##Git Test - Installed required libs

##adding second line to see whether I am doing the Push correctly

##Test on the next day


##### Function to perform change point for each day, adopted from Jun Li###
##Note, the function is created considered the equal change point for each data. to be specific, for each day, 4 changepoints will be detetcted using th below function
###User should have a good understanding of the data to predefine the number of changepoints. 
###To increase the changepoints, the code must be changed accordingly

cpt_evaluation <- function(data,num_cpt,cpt_method,penalty_method){
  colnames(data) <- c('day_index_num','value')
  day_index_all <- unique(data$day_index_num)
  
  if(cpt_method=='mean'){####calculated mean value
    df <- matrix(nrow = length(day_index_all),ncol = (num_cpt+1)*2)
    for(i in 1:length(day_index_all)){
      ts_profile <- data$value[which(data$day_index_num==day_index_all[i])]
      set.seed(1)
      x_binseg <- changepoint::cpt.mean(ts_profile,method='BinSeg',Q=num_cpt,penalty= penalty_method)
      cpts_pos <- cpts(x_binseg)
      cpts_param <- unlist(param.est(x_binseg), use.names=FALSE)
      
      if(length(cpts_pos)==num_cpt){
        df[i,] <- c(i, cpts_pos, cpts_param)
      }else{
        len_add_cpts <- num_cpt-length(cpts_pos)
        cpts_pos <- c(cpts_pos, rep(NA, len_add_cpts))
        cpts_param <- c(cpts_param,rep(NA, len_add_cpts))
        df[i,] <- c(i, cpts_pos, cpts_param)
      }
    }
  }else if(cpt_method=='meanvar'){###calculates mean and variance
    df <- matrix(nrow = length(day_index_all),ncol = (num_cpt+1)*3)
    for(i in 1:length(day_index_all)){
      ts_profile <- data$value[which(data$day_index_num==day_index_all[i])]
      set.seed(1)
      x_binseg <- changepoint::cpt.meanvar(ts_profile,method='BinSeg',Q=num_cpt,penalty= penalty_method)
      cpts_pos <- cpts(x_binseg)
      cpts_param_1 <- unlist(param.est(x_binseg)$mean, use.names=FALSE)
      cpts_param_2 <- unlist(param.est(x_binseg)$variance, use.names=FALSE)
      
      if(length(cpts_pos)==num_cpt){
        df[i,] <- c(i, cpts_pos, cpts_param_1,cpts_param_2)
      }else{
        len_add_cpts <- num_cpt-length(cpts_pos)
        cpts_pos <- c(cpts_pos, rep(NA, len_add_cpts))
        cpts_param_1 <- c(cpts_param_1,rep(NA, len_add_cpts))
        cpts_param_2 <- c(cpts_param_2,rep(NA, len_add_cpts))
        df[i,] <- c(i, cpts_pos, cpts_param_1,cpts_param_2)
      }
    }
  }else if(cpt_method=='var'){####calculates only variance
    df <- matrix(nrow = length(day_index_all),ncol = (num_cpt+1)*2+1)
    for(i in 1:length(day_index_all)){
      ts_profile <- data$value[which(data$day_index_num==day_index_all[4])]
      set.seed(1)
      x_binseg <- changepoint::cpt.var(ts_profile,method='BinSeg',Q=num_cpt,penalty= penalty_method)
      cpts_pos <- cpts(x_binseg)
      cpts_param_1 <- unlist(param.est(x_binseg)$variance, use.names=FALSE)
      cpts_param_2 <- unlist(param.est(x_binseg)$mean, use.names=FALSE)
      
      if(length(cpts_pos)==num_cpt){
        df[i,] <- c(i, cpts_pos, cpts_param_1,cpts_param_2)
      }else{
        len_add_cpts <- num_cpt-length(cpts_pos)
        cpts_pos <- c(cpts_pos, rep(NA, len_add_cpts))
        cpts_param_1 <- c(cpts_param,rep(NA, len_add_cpts))
        
        df[i,] <- c(i, cpts_pos, cpts_param_1,cpts_param_2)
      }
    }
  }
  return(df)
}

## Function to count the frequency of a cpt occurs in different sequence. 
cpt_count <- function(data, num_cpt,ts_len){
  cpt_seq <- seq(1,ts_len,1)
  df_count <- matrix(nrow = ts_len, ncol = num_cpt+1)
  df_count[,1] <- cpt_seq
  for(i in 1:num_cpt){
    cpt_count <- sapply(1:ts_len,function(k){length(which(data[,i+1]==k))})
    df_count[,i+1] <- cpt_count
  }
  count_NA <- sapply(1:num_cpt,function(j){length(which(is.na(data[,j+1])))})
  count_NA <- c(NA, count_NA)
  df_count <- rbind(df_count,count_NA)
  return(df_count)
}


library(changepoint)

##Test the cpt_evaluation function and cpt_count function on the dataset. In the following, changepoint is set to 4 (num_cpt=4)
M_162_cpt_jun <- cpt_evaluation(A_162_Nonzerodays_weekdays[c(1,35)], num_cpt = 4,cpt_method = 'mean', penalty_method = 'AIC')


M_162_RF <- subset(M_162_cpt_jun, select = c(2:5))

####Finding the frequency of changepoints each day

rel_freq_daily <- table(M_162_RF)

freq_daily <- table(M_162_RF)/length(M_162_RF)

barplot(freq_daily, xlab = "Time of the day (hrs)", ylab = "Frequency", ylim = c(0.00, 0.16), col = "green", main = "Occupancy Movement - Weekdays - 162", las=2) 
box()

