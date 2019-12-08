library(MASS)

get_mu <- function(xl)
{

	m <- dim(xl)[2]
	mu <- matrix(NA, 1, m)
	
	for(i in 1:m)
	{
		mu[1,i] <- mean(xl[,i])
	}
	
	return(mu)
	
}

get_matrix <- function(xl,mu)
{

	n <- dim(xl)[1]
	m <- dim(xl)[2]
	sigma <- matrix(0, m, m)
	xl <- as.matrix(xl)
	for(i in 1:n)
	{
		sigma <- sigma + (t(xl[i,]-mu) %*% (xl[i,]-mu))
	}
	
	return(sigma/(n-1))

}

get_coef <- function(mu1,mu2,sigma1,sigma2)
{
  d1 <- det(sigma1)
  d2 <- det(sigma2)
  invs1 <- solve(sigma1)
  invs2 <- solve(sigma2)
  
  a <- invs1 - invs2
  b <- invs1 %*% t(mu1) - invs2 %*% t(mu2)
  
  A <- a[1,1] # x^2
  B <- a[2,2] # y^2
  C <- 2 * a[1, 2] # xy
  D <- -2 * b[1, 1] # x
  E <- -2 * b[2, 1] # y
  G <- c(mu1 %*% invs1 %*% t(mu1) - mu2 %*% invs2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))
  
  func <- function(x, y) {
    x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
  }
  
  return(func)
}

xl <- iris[,3:5]

# Рисуем обучающую выборку
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

# Оценивание
f <- xl[xl[,3] == "setosa",1:2]
s <- xl[xl[,3] == "versicolor",1:2]
fs <- xl[xl[,3] == "virginica",1:2]
mu1 <- get_mu(f)
mu2 <- get_mu(s)
mu3 <- get_mu(fs)

sigma1 <- get_matrix(f, mu1)
sigma2 <- get_matrix(s, mu2)
sigma3 <- get_matrix(fs, mu3)

mu <- rbind(mu1,mu2,mu3)
sigma <- rbind(sigma1,sigma2,sigma3)


classif <- function(l,sigma,mu,classes,lamda=1,P=0.5){
	
	m <- length(classes)
	max  <- -100000 
	class <- "unknown"
	for(i in 1:m){
		k <- log(lamda*P)-0.5*t(l-mu[i,]) %*% solve(sigma[(2*i-1):(2*i),1:2]) %*% (l-mu[i,])-0.5*log(abs(det(sigma[(2*i-1):(2*i),1:2])))
	
		if( k > max ){
			max <- k
			class <- classes[i]
		}
	}
	return(class)
}

for(i in seq(0,8,0.1)){
	for(j in seq(0,2.5,0.1)){
		l<-c(i,j)
		class <- classif(l,sigma,mu,colors)
		points(l[1],l[2],pch = 21, col=class, asp = 1)
	}
}


func <- get_coef(mu1,mu3,sigma1,sigma3)
# Рисуем дискриминантую функцию
y <- seq(0, 3, len = 100)
x <- seq(0, 8, len = 100)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "green", add = TRUE)

func <- get_coef(mu1,mu2,sigma1,sigma2)

# Рисуем дискриминантую функцию
y <- seq(0, 3, len = 100)
x <- seq(0, 8, len = 100)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "blue", add = TRUE)

func <- get_coef(mu2,mu3,sigma2,sigma3)

# Рисуем дискриминантую функцию
y <- seq(0, 3, len = 100)
x <- seq(0, 8, len = 100)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "red", add = TRUE)




library(MASS)

get_mu <- function(xl)
{

	m <- dim(xl)[2]
	mu <- matrix(NA, 1, m)
	
	for(i in 1:m)
	{
		mu[1,i] <- mean(xl[,i])
	}
	
	return(mu)
	
}
get_matrix <- function(xl1,xl2,xl3,mu1,mu2,mu3)
{
	xl1 <- as.matrix(xl1)
	xl2 <- as.matrix(xl2)
	xl3 <- as.matrix(xl3)
	n <- dim(xl1)[1]
	m <- dim(xl2)[1]
	d <- dim(xl3)[1]
	nm <- n+m+d
	col <- dim(xl1)[2]
	sigma <- matrix(0, col, col)
	for(i in 1:n)
	{
		sigma <- sigma + (t(xl1[i,]-mu1) %*% (xl1[i,]-mu1))
	}

	for(i in 1:m)
	{
		sigma <- sigma + (t(xl2[i,]-mu2) %*% (xl2[i,]-mu2))
	}
	for(i in 1:d)
	{
		sigma <- sigma + (t(xl3[i,]-mu3) %*% (xl3[i,]-mu3))
	}
	return(sigma/(nm+3))

}

get_coef <- function(mu1,mu2,sigma1,sigma2)
{
	d1 <- det(sigma1)
  	d2 <- det(sigma2)
  	invs1 <- solve(sigma1)
  	invs2 <- solve(sigma2)
  
  	a <- invs1 - invs2
  	b <- invs1 %*% t(mu1) - invs2 %*% t(mu2)
  
 	D <- -2 * b[1, 1] # x
  	E <- -2 * b[2, 1] # y
  	G <- c(mu1 %*% invs1 %*% t(mu1) - mu2 %*% invs2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))
  
  	func <- function(x, y) {
    		x*D + y*E + G
  	}
	return(func)
}


xl <- iris[,3:5]

colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

# Оценивание
f <- xl[xl[,3] == "setosa",1:2]
s <- xl[xl[,3] == "versicolor",1:2]
fs <- xl[xl[,3] == "virginica",1:2]

mu1 <- get_mu(f)
mu2 <- get_mu(s)
mu3 <- get_mu(fs)

sigma <- get_matrix(f,s,fs,mu1,mu2,mu3)
mu <- rbind(mu1,mu2,mu3)

classif <- function(l,sigma,mu,classes,lamda=1,P=0.5){
	
	m <- length(classes)
	max  <- -100000 
	class <- "unknown"
	for(i in 1:m){
		k <- log(lamda*P)-0.5*t(mu[i,]) %*% solve(sigma) %*% mu[i,]+t(l) %*% solve(sigma) %*% mu[i,]
		if( k > max ){
			max <- k
			class <- classes[i]
		}
	}
	return(class)
}

for(i in seq(0,8,0.1)){
	for(j in seq(0,2.5,0.1)){
		l<-c(i,j)
		class <- classif(l,sigma,mu,colors)
		points(l[1],l[2],pch = 21, col=class, asp = 1)
	}
}

f <- get_coef(mu1,mu3,sigma,sigma)
x <- seq(0, 7, len = 200)
y <- seq(0, 5, len = 40)
z <- outer(x, y, f)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "green", add = TRUE)

f <- get_coef(mu1,mu2,sigma,sigma)
x <- seq(0, 7, len = 200)
y <- seq(0, 5, len = 40)
z <- outer(x, y, f)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "blue", add = TRUE)

f <- get_coef(mu2,mu3,sigma,sigma)
x <- seq(0, 7, len = 200)
y <- seq(0, 5, len = 40)
z <- outer(x, y, f)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "red", add = TRUE)
