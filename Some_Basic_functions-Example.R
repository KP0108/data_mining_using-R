###some basic functions for data transformation in R

###Function for minmax normalization
 Func.minmax <- function (ts){
   ts.min <- min(ts, na.rm = TRUE)
     ts.max <- max(ts, na.rm = TRUE)
     (ts-ts.min)/(ts.max-ts.min)
 }

 ####Function for season split in a year
 
 #season analysis
 
 Func.season <- function(month){
    
    Winter <- c(1,2,3)
    
    Spring <- c(3,4,5)
    
    Summer <- c(6,7,8)
    
    Autumn <- c(9,10,11)
    
    season_division <- c("Winter","Spring", "Summer", "Autumn")
    
    x <- vector()
    
    
    
    for(i in 1:length(month)){
       
       if(month[i] %in% Winter){
          
          x[i] = season_division[1]
          
       }else if(month[i] %in% Spring){
          
          x[i] = season_division[2]
          
       }else if(month[i] %in% Summer){
          
          x[i] = season_division[3]
          
       }else{
          
          x[i] = season_division[4]
          
       }
       
    }
    
    
    
    return(x)
    
 }
 

 #####Farenheit to Celsius
 convert_fahr_to_celsius <- function(temp) {
 Celsius <- ((temp - 32) * (5 / 9))
 return(Celsius)
 }
 
 
 #####Weekday and weekend
 weekday1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

Data_new <- Data %>%
  
  mutate(Weekday_weekend=factor((DayOfWeek %in% weekday1),
                                
                                levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))

### Function to split time in a day

Func.Day <- function(time){
  night <- c("0","1","2","3","4","5","22","23")
  morning <- c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21")
  day_division <- c("Off.Peak","Peak")
  x <- vector()
  for(i in 1:length(time)){
    if(time[i] %in% night){
      x[i]= day_division[1]
    }else{ x[i]= day_division[2]}
  }
  return(x)
}


 
 
