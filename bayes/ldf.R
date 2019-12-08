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

get_matrix <- function(xl1,xl2,mu1,mu2)
{

	n <- dim(xl1)[1]
	m <- dim(xl2)[1]
	nm <- n+m
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
	return(sigma/(nm+2))

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


c <- 200
sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)

mu1 <- c(1, 0)
mu2 <- c(15, 0)

xy1 <- mvrnorm(n=c, mu1, sigma1)
xy2 <- mvrnorm(n=c, mu2, sigma2)
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

colors <- c("gray", "orange")
plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1, main = "Линейный дискриминант Фишера")
first <- xl[xl[,3] == 1, 1:2]
second <- xl[xl[,3] == 2, 1:2]

mu1 <- get_mu(first)
mu2 <- get_mu(second)
mu <- rbind(mu1,mu2)
sigma <- get_matrix(first,second,mu1,mu2)

f <- get_coef(mu1,mu2,sigma,sigma)
x <- y <- seq(-10, 20, len = 100)
z <- outer(x, y, f)

contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "red", add = TRUE)
	

classif <- function(l,sigma,mu,classes,lamda,P){

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

l  <-  c(9,0)

class <- classif(l,sigma,mu,colors,1,0.5)
points(l[1],l[2],pch = 21, col=class, asp = 1)

for(i in seq(-3,20,1)){
	for(j in seq(-10,10,1)){
		l<-c(i,j)
		class <- classif(l,sigma,mu,colors,1,0.5)
		points(l[1],l[2],pch = 21, col=class, asp = 1)
}
}
