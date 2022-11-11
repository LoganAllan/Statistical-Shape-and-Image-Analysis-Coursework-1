
rm(list = ls())

# Incorrect Code

n<-10
sigma<-0.1
mu<- matrix( (-0.5,0,0.5,0,0,sqrt(3)/2) , 2, 3)
mu <- t(mu)
x <- array( 0, c(3,2,n) )
plotshapes (mu)
for (i in 1:n){
  x[,,i] <- mu + matrix( sigma*rnorm(6) , 3 ,2)
}
plotshapes(x)
bb <- bookstein2d( x )
plotshapes(bb)
ans <- procGPA( x )
pp -> bookstein.shpv( ans )
lines( pp[c(1,2,3,1),] , col=2)
lines( mu[c(1,2,3,1),] , col=1)
lines( bb$mshape[c(1,2,3,1),] , col=3)

# Correct Code

library(shapes)
n <- 10
sigma <- 0.1
mu <- matrix( c(-0.5,0,0.5,0,0,sqrt(3)/2) , 2, 3)
mu <- t(mu)
plotshapes (mu)
x <- array( 0, c(3,2,n) )
for (i in 1:n){
  x[,,i] <- mu + matrix( sigma*rnorm(6) , 3 ,2)
}
plotshapes(x)
bb <- bookstein2d( x )
plotshapes(bb$mshape)
ans <- procGPA( x )
pp <- bookstein.shpv( ans$mshape )
lines( mu[c(1,2,3,1),] , col=1)
lines( pp[c(1,2,3,1),] , col=2)
lines( bb$mshape[c(1,2,3,1),] , col=3)

# Correct Code with Comments

library(shapes) # Load in the shapes package

# Define initialising variables
n <- 10
sigma <- 0.1
# Define meanshape
mu <- matrix( c(-0.5,0,0.5,0,0,sqrt(3)/2) , 2, 3)
mu <- t(mu)

plotshapes (mu) # Plot meanshape

# Define the array of coordinates of size 3 x 2 x 10, and we have k=3 landmarks 
# in m=2 dimensions for each of n=10 observations
x <- array( 0, c(3,2,n) )
for (i in 1:n){
  x[,,i] <- mu + matrix( sigma*rnorm(6) , 3 ,2)
}
plotshapes(x) # Plot all 10 observations

bb <- bookstein2d( x ) # Carries out Bookstein's baseline registration and 
                       # calculates a mean shape
plotshapes(bb$mshape) # Plots the Bookstein's mean shape

ans <- procGPA( x ) # Carries out Generalised Procrustes analysis of x
pp <- bookstein.shpv( ans$mshape ) # Calculates the bookstein coordinates of the
                                   # mean shape of ans

# Plot the respective mean shapes:
lines( mu[c(1,2,3,1),] , col=1) # Mean Shape of given coordinates
lines( pp[c(1,2,3,1),] , col=2) # GPA Mean Shape
lines( bb$mshape[c(1,2,3,1),] , col=3) # Bookstein's Mean Shape

