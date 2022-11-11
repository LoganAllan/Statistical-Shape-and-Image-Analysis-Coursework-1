
library(shapes)

# Flat Triangles

A <- matrix(c(-1,0,1,1,1,1),3,2)
B <- matrix(c(-1,-0.5,1,-1,0,3),3,2)
C <- matrix(c(-1,-0.8,1,-1,0.5,14),3,2)

# Plot Triangles 

par(mfrow= c(1,3) )
plotshapes(A,joinline=c(1:3,1))
plotshapes(B,joinline=c(1:3,1))
plotshapes(C,joinline=c(1:3,1))

# Bookstein Coordinates

bookst <- array(0,c(3,2,3))
bookst[,,1] <-  bookstein.shpv(A)
bookst[,,2] <-  bookstein.shpv(B)
bookst[,,3] <-  bookstein.shpv(C)

# Plot Bookstein Coordinated Triangles

par(mfrow= c(1,3) )
for (i in 1:3){
  plotshapes(bookst[,,i],joinline=c(1:3,1))
}


