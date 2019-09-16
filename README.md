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

kNN <- function(xl, z, k)

{

orderedXl <- sortObjectsByDist(xl, z)

n <- dim(orderedXl)[2] - 1

classes <- orderedXl[1:k, n + 1]

21counts <- table(classes)

class <- names(which.max(counts))

return (class)

}

colors <- c("setosa" = "red", "versicolor" = "green3",

"virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col

= colors[iris$Species], asp = 1)

z <- c(2.7, 1)

xl <- iris[, 3:5]

class <- kNN(xl, z, k=6)

points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
