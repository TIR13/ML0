library("MASS")


rasp <- function(x,mu,sigma){
	if(det(sigma)==0)sigma[1,1]<-sigma[1,1]+0.01
	return(exp(-0.5*t(x-mu) %*% solve(sigma) %*% (x-mu))/(sqrt(det(sigma))*2*pi))
}

em <- function(xl,k,teta,eps=1e-5){
	n <- dim(xl)[1]
	g <- matrix(0,n,k)
	g0 <- matrix(0,n,k)
	iter <- 0
	repeat{
		g0 <- g
		for(i in 1:n){
			for(j in 1:k){
				g[i,j] <- teta[j]*rasp(xl[i,1:2],teta[(k+2*j-1):(k+2*j)],matrix(teta[(3*k+4*j-3):(3*k+4*j)],2,2))
				sum <- 0
				for(s in 1:k){
					sum <- sum+teta[s]*rasp(xl[i,1:2],teta[(k+2*s-1):(k+2*s)],matrix(teta[(3*k+4*s-3):(3*k+4*s)],2,2))
				}
				g[i,j] <- g[i,j]/sum
			}
		}
		#print(g)
		wx <- 0
		for(j in 1:k){
			wx <- 0
			mx <- 0
			sx <- 0
			for(i in 1:n){
				wx <- wx+g[i,j]
				mx <- mx+g[i,j]*xl[i,1:2]
			}
			teta[j] <- wx/n	
			teta[(k+2*j-1):(k+2*j)] <- mx/(n*teta[j])
			for(i in 1:n){
				sx <- sx+g[i,j]*(xl[i,1:2]-teta[(k+2*j-1):(k+2*j)]) %*% t(xl[i,1:2]-teta[(k+2*j-1):(k+2*j)])
			}
			teta[(3*k+4*j-3):(3*k+4*j)] <- sx/(n*teta[j])
		}
		if(max(g-g0)<eps )break
		iter<-iter+1
	}
	return(teta)
}

em_add <- function(xl,R=1.5,m0=15){
	n <- dim(xl)[1]
	k<-1
	w<-rep(0,n/2)
	mu<-rep(0,n)
	sigma<-rep(0,2*n)
	w[k]<-1
	#g<-matrix(0,n,100)
	for(i in 1:n){
		mu[k:(k+1)] <- mu[k:(k+1)]+xl[i,1:2]/n
	}
	for(i in 1:n){
		sigma[k:(k+3)] <- sigma[k:(k+3)]+t(t(xl[i,1:2]-mu[k:(k+1)])) %*% (xl[i,1:2]-mu[k:(k+1)])/n
	}
	#em(xl,k,c(w[k],mu[k],sigma[k]))
	teta<-c(w[1],mu[1:2],sigma[1:4])
	repeat{
		p <- rep(0,n)
		U <- data.frame()
		for(i in 1:n){
			for(j in 1:k){
				
				p[i]<-p[i]+rasp(xl[i,1:2],mu[(2*j-1):(2*j)],matrix(sigma[(4*j-3):(4*j)],2,2))
			}
			

		}
		
		
		for(i in 1:n){
			if(p[i]<max(p)/R)U <- rbind(U,xl[i,1:3])
			
		}
		u<-dim(U)[1]
		points(xl, pch=21, col=colors[xl[,3]], bg=colors[xl[,3]] )

		points(U, pch=21, col="black",bg="black")
		print(u)
		R=R+10
		if(u<m0 && k>1)break
		k<-k+1
		sum1 <- 0
		w[k]=u/n
		for(i in 1:u){
			mu[(2*k-1):(2*k)]<-mu[(2*k-1):(2*k)]+c(U[i,1],U[i,2])/(u*w[k])
		}
		for(i in 1:u){
			sigma[(4*k-3):(4*k)]<-sigma[(4*k-3):(4*k)]+(c(U[i,1],U[i,2])-mu[(2*k-1):(2*k)]) %*% t(c(U[i,1],U[i,2])-mu[(2*k-1):(2*k)])/(u*w[k])
		}
		for(j in 1:k-1){
			w[j]=w[j]*(1-w[k])
		}
		teta <- em(xl,k,c(w[1:k],mu[1:(2*k)],sigma[1:(4*k)]),1e-3)
		w[1:k] <- teta[1:k]
		mu[1:(2*k)] <- teta[(k+1):(3*k)]
		sigma[1:(4*k)] <- teta[(3*k+1):(7*k)]
	}
	return(teta)


}

n <- 50

sigma1 <- matrix(c(4,0, 0, 2), 2, 2)
sigma2 <- matrix(c(2, 0,0, 3), 2, 2)
sigma3 <- matrix(c(1, 0,0, 1), 2, 2)

mu2 <- c(24, 12)
mu3 <- c(3, 7)
mu1 <- c(15, 14)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)
xy3 <- mvrnorm(n=n, mu = mu3, Sigma = sigma3)

xl1 <- rbind(xy1,xy2,xy3)
xl1 <- cbind(xl1, c(rep(1, 3*n)))

sigma1 <- matrix(c(3,0, 0, 6), 2, 2)
sigma2 <- matrix(c(4, 0,0, 3), 2, 2)
sigma3 <- matrix(c(1, 0,0, 1), 2, 2)

mu2 <- c(3, -2)
mu3 <- c(10, 10)
mu1 <- c(19, 18)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)
xy3 <- mvrnorm(n=n, mu = mu3, Sigma = sigma3)

xl2 <- rbind(xy1,xy2,xy3)
xl2 <- cbind(xl2, c(rep(2, 3*n)))

xl <- rbind(xl1,xl2)
plotxmin <- min(xl[,1], xl[,1]) - 0.3
plotxmax <- max(xl[,1], xl[,1]) + 0.3
plotymin <- min(xl[,2], xl[,2]) - 0.5
plotymax <- max(xl[,2], xl[,2]) + 0.5
colors <- c("1" = "red", "2" = "green3","3" = "blue")
# Рисуем обучающую выборку

plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="RBF")

points(xl, pch=21, col=colors[xl[,3]], bg=colors[xl[,3]] )

l1 <- em_add(xl1)
l2 <- em_add(xl2)

k1 <- length(l1)/7
k2 <- length(l2)/7

w1 <- l1[1:k1]
w2 <- l2[1:k2]

mu1 <- l1[(k1+1):(3*k1)]
mu2 <- l2[(k2+1):(3*k2)]

sigma1 <- l1[(3*k1+1):(7*k1)]
sigma2 <- l2[(3*k2+1):(7*k2)]

plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="RBF")

points(xl, pch=21, col=colors[xl[,3]], bg=colors[xl[,3]] )

for(i in seq(plotxmin,plotxmax,1)){
	for(ij in seq(plotymin,plotymax,1)){
		z<-c(i,ij)
		#print(z)
		p1 <- 0
 		for(j in 1:k1){
			p1 <- p1 + w1[j]*rasp(z,mu1[(2*j-1):(2*j)],matrix(sigma1[(4*j-3):(4*j)],2,2))
		}
		p2 <- 0 
		for(j in 1:k2){
			p2 <- p2 + w2[j]*rasp(z,mu2[(2*j-1):(2*j)],matrix(sigma2[(4*j-3):(4*j)],2,2))
		}
		if(p1>p2)
			class <- 1
		else 
			class <- 2
		points(z[1],z[2], pch=21, col=colors[class],asp=0 )
	}
}

