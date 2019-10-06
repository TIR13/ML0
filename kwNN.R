colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

euclideanDistance <- function(u, v)

{

sqrt(sum((u - v)^2))

}

sortObjectsByDist <- function(xl, z, metricFunction =

euclideanDistance)

{

l <- dim(xl)[1]

n <- dim(xl)[2] - 1

distances <- matrix(NA, l, 2)

for (i in 1:l)

{

distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))

}

orderedXl <- xl[order(distances[, 2]), ]
return (orderedXl);

}
kwNN <- function(xl, z, k, q) 
{
  n <- dim(orderedXl)[2] - 1
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)  
  classes <- orderedXl[1:k, n + 1] 
  counts <- table(classes)
   name <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:k ){
    w <- q ^ i
    name[[classes[i]]] <- name[[classes[i]]] + w
  }
  class <- names(which.max(name))
  return (class)
}
z <- c(2.7, 1) 

xl <- iris[, 3:5] 

orderedXl <- sortObjectsByDist(xl, z)
class <- kwNN(xl, z,6,1)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 

Loo <- function(k,q,xl)

   {
    sum =0
    for(i in 1:dim(xl)[1])
       {
        tmpXL <- rbind(xl[1:i-1, ],
        xl[i+1:dim(xl)[1],])
        xi <- c(xl[i,1], xl[i,2])
        class <-kwNN(tmpXL,xi,k,q)
        if(class != xl[i,3])
        sum=sum+1
       }
   sum=sum/dim(xl)[1]
   return(sum)
  }
y <- rep(0,21)
kk <- 0
min <- 30
x <- 0
xx <- rep(0,21)
for(k in 1:21){
xx[k] <- x
y[k] <- Loo(6,x,xl)
if(y[k]<min){
min <- y[k]
kk <- k
}
x <- x+0.05
}
plot(xx,y,type="l")
label = paste("   K = ", 6, "\n","q = ",xx[kk],"\n   LOO = ", round(y[kk],3), sep = "")
text(0.9,y[kk],labels=label,pos=3)
