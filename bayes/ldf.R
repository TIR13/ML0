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

c <- 200
sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)

mu1 <- c(1, 0)
mu2 <- c(15, 0)

xy1 <- mvrnorm(n=c, mu1, sigma1)
xy2 <- mvrnorm(n=c, mu2, sigma2)
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

colors <- c("green3", "yellow")
plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1, main = "Линейный дискриминант Фишера")

first <- xl[xl[,3] == 1, 1:2]
second <- xl[xl[,3] == 2, 1:2]

mu1 <- get_mu(first)
mu2 <- get_mu(second)
sigma <- get_matrix(first,second,mu1,mu2)
a <- solve(sigma) %*% t(mu1-mu2)
b <- ((mu1+mu2)/2) %*% a

abline(b / a[2,1], -a[1,1]/a[2,1], col = "red", lwd = 3) 