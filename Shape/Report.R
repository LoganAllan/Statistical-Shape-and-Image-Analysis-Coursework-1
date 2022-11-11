
rm(list = ls()) 

library(shapes) # Load in the shapes package

raw_data <- read.table("data5.txt") # Import the given data

data <- array(0,c(10,2,30)) # Initialise the data with the correct dimensions

# Format the data

for (i in 1:30) {
  for (j in 1:10) {
    for (k in 1:2) {
      data[j,k,i] <- raw_data[j+(i-1)*10,k]
    }
  }
}

data # Check data is in correct format

plotshapes(data,joinline=1:10) # Plots all 30 observations. Very messy but we 
                               # can see an obvious outlier

par(mfrow=c(1,2))
plotshapes(data[,,10],joinline=1:10) # Plots the 10th observation
plotshapes(data[,,5],joinline=1:10) # Plots the observation containing the outlier

# Import and format data without the outlier observation

raw_data2 <- read.table("data5.5.txt") # Import the given data

data2 <- array(0,c(10,2,29)) # Initialise the data with the correct dimensions

for (i in 1:29) {
  for (j in 1:10) {
    for (k in 1:2) {
      data2[j,k,i] <- raw_data2[j+(i-1)*10,k]
    }
  }
}

data2

# Centroid Size

centroid_size1 <- rep(0,times=30)
for (i in 1:30){
  centroid_size1[i] <- centroid.size(data[,,i])
}
centroid_size2 <- rep(0,times=29)
for (i in 1:29){
  centroid_size2[i] <- centroid.size(data2[,,i])
}

centroid_size1 # Table of the Centroid Sizes of each observation
centroid_size2

summary(centroid_size1) 
summary(centroid_size2) 

par(mfrow= c(1,4))
plot(centroid_size1, main="Centroid Sizes", ylab="Centroid Size")
hist(centroid_size1, main="Histogram of Centroid Size", xlab="Centroid Size")
qqnorm(centroid_size1) # Q-Q plot is fairly linearly so we can assume normality
qqnorm(log(centroid_size1), main="Lognormal Q-Q Plot") # Lognormal Q-Q plot is 
                                        # linear so we can assume lognormality

# Bookstein shape variables

bookst <- array(0,c(10,2,30))
bookst2 <- array(0,c(10,2,29))

for (i in 1:30){
  bookst[,,i] <- bookstein.shpv (data[,,i])
}
for (i in 1:29){
  bookst2[,,i] <- bookstein.shpv (data2[,,i])
}

par(pty="s")
par(mfrow=c(1,2))
plot(bookst[,,1], xlim=c(-1,70), ylim=c(-50,10), xlab="x", ylab="y", main="Bookstein with Outlier")
for (i in 1:30){
  points(bookst[,,i])
  lines(bookst[,,i])
}
plot(bookst2[,,1], xlim=c(-1,70), ylim=c(-5,10), xlab="x", ylab="y", main="Bookstein without Outlier")
for (i in 1:29){
  points(bookst2[,,i])
  lines(bookst2[,,i])
}

# Calculate and plot mean shapes

par(pty="s")
par(mfrow=c(1,2))
plotshapes(bookstein2d(data)$mshape, joinline=1:10) 
plotshapes(bookstein2d(data2)$mshape, joinline=1:10)

# General Procrustes Analysis

proc <- procGPA(data) 
proc2 <- procGPA(data2)
pp <- bookstein.shpv(proc$mshape) 
pp2 <- bookstein.shpv(proc2$mshape) 

# Plot GPA Mean Shapes

par(pty="s")
par(mfrow=c(1,2))
plotshapes(pp, joinline=1:10)
plotshapes(pp2, joinline=1:10)
