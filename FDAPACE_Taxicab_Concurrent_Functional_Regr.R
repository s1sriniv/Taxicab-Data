  ### OBJECTIVE: Perform functional regression to predict daily intensity function by zone
  ### given covariates in CSV file.


  ## YOU DO NOT NEED TO SORT/RE-ORDER VALUES WITHIN 2016_data.rds, 2017_data.rds, 
  ## and 2017_dropoff_data_zone161.rds FILES! 
  ## I HAVE ALREADY DONE THIS!!! (Sorted in order by day, from 1-01 to 12-31)
  ## 2017_data.rds STILL NEEDS TO BE CLEANED - 2018 VALUES!! (NOT A PROBLEM W/ DROPOFF!!!)

  library(fdapace)

  data_2017 <- readRDS("C:/Users/s1sri/Desktop/NY TAXICAB DATA 2016/2017_data.rds")
  
  zone_ID <- 161
  data_2017 <- data_2017[data_2017$PULocationID == zone_ID, 1]
  data_2017 <- data_2017[substr(data_2017, 3, 4) == "17"] # Clean out '16/18/19 values JUST IN CASE
  data_2017 <- data_2017[weekdays(as.Date(data_2017)) == "Wednesday"] # USING ONLY WEDNESDAY DATA
  
  t_values <- vector("list", 52)
  y_values <- vector("list", 52)
  
  i <- 1
  
  while (NROW(data_2017) > 0) {
    ch <- substr(data_2017[1], 6, 10)
    single_day_ind <- substr(data_2017, 6, 10) == ch
    
    data_2017_extract <- data_2017[which(single_day_ind)]
    
    data_2017_extract <- as.integer(substr(data_2017_extract, 12, 13))*60 + 
      as.integer(substr(data_2017_extract, 15, 16)) # Minutes 1-1440 in a day
    
    t_values[[i]] <- seq(from=1, to=144, by=1)
    y_values[[i]] <- rep(0, 144)
    
    j <- 1
    for (k in seq(from=1, to=1440, by=10)) { # Binning by 10-minute intervals for function
      y_values[[i]][j] <- (length(which(data_2017_extract >= k & 
                                        data_2017_extract <= k+9)))
      j <- j+1
    }
    
    #functional_data_by_day[[d]][i, ] <- smooth.spline(1:720, functional_data_by_day[[d]][i,], 
    #cv=TRUE)$y
    
    #functional_data_by_dat[[d]][i,] <- data.table(data_2016_f_extract)[, .N, 
    #keyby=data_2016_f_extract]$N
    
    data_2017 <- data_2017[which(!single_day_ind)]
    i <- i+1
    print("WHILE LOOP")
  }

  rm(data_2017)
  
Ys <- list(Ly=y_values, Lt=t_values)






### THIS PART IS FOR (SHIFTED) DROPOFF FUNCTIONAL COVARIATE!!!!
# DROPOFF RDS ONLY CONTAINS ZONE 161 DATA SO NO ZONE COLUMN!!

data_2017 <- readRDS("C:/Users/s1sri/Desktop/NY TAXICAB DATA 2016/2017_dropoff_data_zone161.rds")
data_2017 <- data_2017[substr(data_2017, 3, 4) == "17"] # Clean out '16/18/19 values JUST IN CASE
data_2017 <- data_2017[weekdays(as.Date(data_2017)) == "Tuesday" | 
                         weekdays(as.Date(data_2017)) == "Wednesday"]
# Extracting both Tuesday AND Wednesday data instead of just Wednesday (like with response)
# because we are shifting back this predictor, so that data from end of preceding Tuesday
# is needed (think about this carefully if you still don't get it!)


dropoff_values <- vector("list", 104) # 52 x 2 days for both Tuesday/Wednesday dropoff data

i <- 1

# COUNTING DROPOFF BY DAY!!!
while (NROW(data_2017) > 0) {
  ch <- substr(data_2017[1], 6, 10)
  single_day_ind <- substr(data_2017, 6, 10) == ch
  
  data_2017_extract <- data_2017[which(single_day_ind)]
  
  data_2017_extract <- as.integer(substr(data_2017_extract, 12, 13))*60 + 
    as.integer(substr(data_2017_extract, 15, 16))
  
  dropoff_values[[i]] <- rep(0, 144)
  
  j <- 1
  for (k in seq(from=1, to=1440, by=10)) {
    dropoff_values[[i]][j] <- (length(which(data_2017_extract >= k & 
                                       data_2017_extract <= k+9)))
    j <- j+1
  }
  
  #functional_data_by_day[[d]][i, ] <- smooth.spline(1:720, functional_data_by_day[[d]][i,], 
  #cv=TRUE)$y
  
  #functional_data_by_dat[[d]][i,] <- data.table(data_2016_f_extract)[, .N, 
  #keyby=data_2016_f_extract]$N
  
  data_2017 <- data_2017[which(!single_day_ind)]
  i <- i+1
  print("WHILE LOOP")
}

# CONCATENATING ALL DAILY FUNCTIONAL OBSERVATIONS INTO ONE LONG VECTOR FOR "SHIFT"
# "TIME CONVOLUTION"!
dropoff_concat <- vector("integer", 0)
for (j in seq(from = 1, to = 104, by = 1)) {
  dropoff_concat <- c(dropoff_concat, dropoff_values[[j]])
}

dropoff_values <- vector("list", 52)
shift <- 6 # THIS IS HOW MUCH DROPOFF FUNCTIONALS ARE SHIFTED IN TIME BY!!! (6*10 min = 1 hr)
index <- 145 - shift

for (k in seq(from = 1, to = 52, by = 1)) {
  dropoff_values[[k]] <- dropoff_concat[index:(index+143)]
  
  index <- index + 288
}
  
Ds <- list(Ly=dropoff_values, Lt=t_values)
    

training_set_ind <- c(1, 2, 3, 4, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 21, 22, 
                      23, 24, 26, 27, 28, 29, 31, 32, 33, 34, 36, 37, 38, 39, 41, 42,
                        43, 44, 46, 47, 48, 49, 51, 52)

covariates_2017_CSV <- read.csv("C:/Users/s1sri/Downloads/2017 NY TAXICAB COVARIATES CSV.csv")
vars <- list(Mon=covariates_2017_CSV[,1], Tue=covariates_2017_CSV[,2], Wed=covariates_2017_CSV[,3], Thu=covariates_2017_CSV[,4], Fri=covariates_2017_CSV[,5], Sat=covariates_2017_CSV[,6], Sun=covariates_2017_CSV[,7], Jan=covariates_2017_CSV[,8], Feb=covariates_2017_CSV[,9], Mar=covariates_2017_CSV[,10], Apr=covariates_2017_CSV[,11], May=covariates_2017_CSV[,12], Jun=covariates_2017_CSV[,13], Jul=covariates_2017_CSV[,14], Aug=covariates_2017_CSV[,15], Sep=covariates_2017_CSV[,16], Oct=covariates_2017_CSV[,17], Nov=covariates_2017_CSV[,18], Dec=covariates_2017_CSV[,19], Holiday=covariates_2017_CSV[,20], Meantemp=covariates_2017_CSV[,21], Maxtemp=covariates_2017_CSV[,22], Mintemp=covariates_2017_CSV[,23], Y=Ys)
with_error_2D <- FCReg(vars, 0.1, 0.1, seq(from=1, to=144, by=1))

# FOR COMPARING ERROR BETWEEN PREDICTED AND ACTUAL FUNCTIONS, BRUTE FORCE (NOT USING L2)
plot(c(1:144), abs(Ys$Ly[[32]] - (with_error_2D$beta0 + with_error_2D$beta[1,]*covariates_2017_CSV[32,22] + with_error_2D$beta[2,]*covariates_2017_CSV[32,23] + with_error_2D$beta[3,]*covariates_2017_CSV[32,24] + with_error_2D$beta[4,]*Ds$Ly[[32]])), ylab="")
