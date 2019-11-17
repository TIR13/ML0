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

coef <- function(mu1,mu2,sigma1,sigma2)
{
	  
	invsigma1 <- solve(sigma1)
	invsigma2 <- solve(sigma2)
	a <- invsigma1 - invsigma2
	A <- a[1,1]
	B <- a[2,2]
	C <- 2*a[1,2]
	
	b <- invsigma1%*%t(mu1) - invsigma2%*%t(mu2)
	
	D <- -2*b[1,1]
	E <- -2*b[2,1]
	F <- c(log(det(sigma1)) - log(det(sigma2)) + mu1%*%invsigma1%*%t(mu1) - mu2%*%invsigma2%*%t(mu2))
	
	func <- function(x, y) {
		x^2*A + y^2*B + x*y*C + x*D + y*E + F
	}
	
	return(func)
	
}


n <- 100

sigma1 <- matrix(c(1,0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0,0, 1), 2, 2)

mu1 <- c(5, 5)
mu2 <- c(15, 15)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)

plotxmin <- min(xy1[,1], xy2[,1]) - 1
plotymin <- min(xy1[,2], xy2[,2]) - 1
plotxmax <- max(xy1[,1], xy2[,1]) + 1
plotymax <- max(xy1[,2], xy2[,2]) + 1
plot(xy1[,1],xy2[,2], type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))

colors <- c("gray", "orange")
points(xy1, pch=21, col=colors[1], bg=colors[1])
points(xy2, pch=21, col=colors[2], bg=colors[2])

mu1 <- get_mu(xy1)
mu2 <- get_mu(xy2)

sigma1 <-get_matrix(xy1,mu1)
sigma2 <-get_matrix(xy2,mu2)

X <- seq(plotxmin-5, plotxmax+5, len = 500)
Y <- seq(plotymin-5, plotymax+5, len = 500)

f <- coef(mu1,mu2,sigma1,sigma2)
Z <- outer(X, Y, f)

contour(X, Y, Z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5,col="green")