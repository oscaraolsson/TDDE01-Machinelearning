set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv", fileEncoding = "latin1")
st <- merge(stations,temps,by="station_number")

# Start with a value, plot for different values. 
h_distance <- 100
h_date <- 20
h_time <- 3

# Target point of which you want to make an estimate for
a = 55.3836
b = 12.8203
target = t(c(b,a))
date <-  "2004-05-28" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", 
"10:00:00", "12:00:00", "14:00:00", "16:00:00", 
"18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))


####################  KERNEL FOR DISTANCE  ####################
dist_kernel = function(st, target) {
  dist = distHaversine(data.frame(st$longitude,st$latitude), target)
  u = dist/h_distance
  return(exp(-(u)**2)/sum(exp(-(u)**2)))
}

######################  KERNEL FOR DAY  #######################  
day_kernel = function(st, date) {
  diff = as.numeric(as.Date(st$date)-as.Date(date), unit="days")
  diff = diff %% 365
  diff = ifelse(diff > 182, 365-diff, diff)
  u = diff/h_date
  return(exp(-(u)**2)/sum(exp(-(u)**2)))
}




######################  KERNEL FOR TIME  ######################  
time_kernel = function(times, target) {
  diff = difftime(strptime(times, format="%H:%M:%S"), 
                              strptime(target, format="%H:%M:%S"))
  u = as.numeric(diff/3600)/h_time
  return(exp(-(u)**2)/sum(exp(-(u)**2)))
}

##############  DECIDE BEST H BY PLOTTING SCORES  #############  
# Plot scores for distance kernel
grid = data.frame(longitude=0, latitude=seq(0,0.005,by=0.0001))
dist = distHaversine(grid, t(c(0,0)))
dist_grid = dist_kernel(grid, t(c(0,0)))
plot(dist, dist_grid, type = "l", 
    xlab="Distance [km]", 
    ylab="Score from distanc kernel")
title("Scores for distance kernel, h = 100")

# Plot scores for day kernel
grid = data.frame(date=seq(as.Date("1990/1/1"), 
                    as.Date("1991/1/1"), "day"))
diff = as.numeric(as.Date(grid$date)-as.Date(date), unit="days")
diff = diff %% 365
diff = ifelse(diff > 182, 365-diff, diff)
day_grid = day_kernel(grid, date)
plot(diff, day_grid, type = "l", xlab="Day difference [days]", 
ylab="Score from day kernel")
title("Scores for day kernel, h = 20")

# Plot scores for time kernel 
grid <-seq(
  from=as.POSIXct("2012-1-1 0:00", tz="UTC"),
  to=as.POSIXct("2012-1-1 23:00", tz="UTC"),
  by="min"
) 
grid <- c(substr(grid, 12, 19))
diff = difftime(strptime(grid, format="%H:%M:%S"), 
                strptime("00:00:00", format="%H:%M:%S"))
diff = as.numeric(diff/3600)
score = time_kernel(grid, "00:00:00")
plot(diff, score, type = "l", xlab="Time difference [h]", 
ylab="Score from time kernel")
title("Scores for time kernel, h = 3")



###################  SCORES FROM ALL KERNELS  ################### 
# Filters posterior time
st = st[!(as.Date(st$date) >= as.Date(date)),]

# Scores from every kernel
score_dist = dist_kernel(st, target)
score_day = day_kernel(st, date)
score_time = matrix(data=NA, nrow=dim(st)[1], ncol=11)
j=1
for (i in times) {
  score_time[,j] = time_kernel(st$time, i)
  j=j+1
}

#######################  ADDING KERNELS  #######################  
for (i in 1:length(temp)) {
score_add = (score_dist + score_day + score_time[,i])/3
pred_add = score_add %*% st$air_temperature
temp[i] = sum(pred_add)
}
plot(c(4,6,8,10,12,14,16,18,20,22,24), temp, type="l", 
    xlab="Time [h]", ylab="Predicted temperature")
title("Predicted temperatures from 04:00 
      to 24:00 with sum of kernels")

#####################  PRODUCT OF KERNELS  ##################### 
for (i  in 1:length(temp)){
score_mult = (score_dist * score_day * score_time[,i])
score_mult = score_mult/sum(score_mult)
pred_mult = score_mult %*% st$air_temperature
temp[i] = sum(pred_mult)
}
plot(c(4,6,8,10,12,14,16,18,20,22,24), temp, 
     type="l", xlab="Time [h]", ylab="Predicted temperature")
title("Predicted temperatures from 04:00 
      to 24:00 with product of kernels")