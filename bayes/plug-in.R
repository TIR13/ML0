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
	
	for(i in 1:n)
	{
		sigma <- sigma + (t(xl[i,]-mu) %*% (xl[i,]-mu))
	}
	
	return(sigma/(n-1))

}

get_coef <- function(mu1,mu2,sigma1,sigma2)
{
	determ1 <-det(sigma1)
  	determ2 <-det(sigma2)
	  
	a <- c(sigma1[2,2]/determ1,sigma2[2,2]/determ2)
  	b <- c(-sigma1[2,1]/determ1,-sigma2[2,1]/determ2)
  	c <- c(-sigma1[1,2]/determ1,-sigma2[1,2]/determ2)
  	d <- c(sigma1[1,1]/determ1,sigma2[1,1]/determ2)

	A <- a[1]-a[2]
  	B <- d[1]-d[2]
  	C <- b[1]+c[1]+b[2]+c[2]
  	D <- -2*mu1[1]*a[1]-2*mu1[2]*b[1]-mu1[1]*c[1]+2*mu2[1]*a[2]+b[2]*mu2[1]+mu2[2]*c[2]
  	E <- -mu1[1]*b[1]-mu1[1]*c[1]-d[1]*2*mu1[2]+b[2]*mu2[1]+c[2]*mu2[1]+2*mu2[2]*d[2]
	F <-  -log(abs(determ1)) + log(abs(determ2)) + 
		mu1[1]*mu1[1]*a[1]+(b[1]+c[1])*mu1[1]*mu1[2]+d[1]*mu1[2]*mu1[2]-
		mu2[1]*mu2[1]*a[2]-(b[2]+c[2])*mu2[1]*mu2[2]-d[2]*mu2[2]*mu2[2]
	
	func <- function(x, y) {
		x^2*A + y^2*B + x*y*C + x*D + y*E + F
	}
	
	return(func)
	
}


n <- 100

sigma1 <- matrix(c(10,0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0,0, 5), 2, 2)

mu1 <- c(1, 0)
mu2 <- c(15, 0)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)

xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

# Рисуем обучающую выборку
colors <- c("gray", "orange")
plot(xl[ , 1], xl[ , 2], pch = 21, bg = colors[xl[ ,3]], asp = 1, xlab = "x", ylab = "y")

# Оценивание
first <- xl[xl[,3] == 1,1:2]
second <- xl[xl[,3] == 2,1:2]
mu1 <- get_mu(first)
mu2 <- get_mu(second)
sigma1 <- get_matrix(first, mu1)
sigma2 <- get_matrix(second, mu2)
f <- get_coef(mu1,mu2,sigma1,sigma2)

# Рисуем дискриминантую функцию
x <- y <- seq(-10, 20, len = 100)
z <- outer(x, y, f)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "green", add = TRUE)

