#ML0

colors <- c("setosa" = "red", "versicolor" = "green3",

"virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],

col = colors[iris$Species])

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
weightsKWNN = function(i, k)
{
  (k + 1 - i) / k
}

kwNN <- function(xl, z, k,orderedXl)
{
  
  n <- dim(orderedXl)[2] - 1
  weights = rep(0,3)
  names(weights) <- c("setosa", "versicolor", "virginica")
  classes <- orderedXl[1:k, n+1]
  
  for(i in 1:k)
  {
    weights[classes[i]]<-weightsKWNN(i,k)+weights[classes[i]];
  }
  class <- names(which.max(weights))
  return (class)
}


colors <- c("setosa" = "red", "versicolor" = "green3",

"virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col

= colors[iris$Species], asp = 1)

z <- c(2.7, 1) 

xl <- iris[, 3:5] 

 orderedXl <- sortObjectsByDist(xl, z)
      class <- kwNN(xl, z,6,orderedXl)


points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 

Loo <- function(k,xl)

   {
n = dim(xl)[1]
    sum =0
    for(i in 1:(n)){
      X=xl[-i, 1:3]
      u=xl[i, 1:2]
      orderedXl <- sortObjectsByDist(X, u)
      
      for(k in 1:(n-1)){
        test=kwNN(X,u,k,orderedXl)
        if(test != xl[i,3]){

        sum=sum+1
       }
}
}
   sum=sum/dim(xl)[1]
   return(sum)
  }
y <- rep(0,150)
kk <- 0
min <- 150
for(k in 1:150){
x <- c(1:150)
y[k] <- Loo(k,xl)
if(y[k]<min){
min <- y[k]
kk <- k
}
}
plot(x,y,type="l")
label = paste("   K = ", kk, "\n", "   LOO = ", y[kk], sep = "")
text(x[kk],y[kk],labels=label,pos=4)

x=0.5

while(x<7.5)

{

y=-1

while(y<4)

{

z <- c(x,y)

x1 <- iris[,3:5]
      orderedXl <- sortObjectByDist(xl, z)
      class <- kwnn(xl, z,k,orderedXl)

points(z[1], z[2], pch = 1, col = colors[class], asp = 1)

y=y+0.1

}

x=x+0.1

}