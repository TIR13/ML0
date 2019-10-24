col <- c("setosa","versicolor" ,"virginica")
colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue","grey"="grey")

dis = function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}

NN <- function(u,U){

	n <- dim(U)[1]
	min <- 10000000
	res <- U[1,1:2]
	
	for (i in 1:n){
		x=U[i,1:2]
		
		if(u[1]!=x[1] & u[2] != x[2]){
			k <- dis(u,x)
			
			if(k < min){
				min <- k
				res <- x
			}
			
		}
	
	}
	return(res)
}



firis <- function(a,u,b){
	S <- (dis(a,b)-dis(a,u))/(dis(a,b)+dis(a,u))
	
	return(S)
}
	
FindEtalon <- function(xl,xy,Etalone){
	n <- dim(xy)[1]
	nn <- dim(xl)[1]
	#print(nn)
	if(n==1)
		return(xy[1,])
	
	x <- xy[,1:2]
	max <- -500
	k <- 0
	sum1 <- 0
	sum2 <- 0
	
	for (i in 1:n){
		sum1 <- 0
		sum2 <- 0
		
		for (j in 1:nn){
			if(xy[i,3]==xl[j,3] & xy[i,1]!=xl[j,1] & xy[i,2]!=xl[j,2]){
				sum1 <- sum1+firis(xl[j,1:2],x[i,],NN(xl[j,1:2],Etalone))
			}
			if(xy[i,3]!=xl[j,3] & xy[i,1]!=xl[j,1] & xy[i,2]!=xl[j,2]){
				sum2 <- sum2+firis(xl[j,1:2],x[i,],NN(xl[j,1:2],Etalone))
			}
		}
		Dx <- sum1/(n-1)
		Tx <- sum2/(nn-n)
		Ex <- 0.5*Dx+0.5*Tx
		#print(nn-n)
		if(max<Ex){
			max <- Ex
			k <- i
		}
	
	}
	
	return(xy[k,1:3])
}

frisstolp <- function(xxl){
	#xxl=unique(iris[,3:5])
	
	x <- xxl[,1:2]
	Etalone1 <- data.frame()
	
	for(i in 1:length(unique(xxl[,3]))){
		Xy <- xxl[(which(xxl[,3]==col[i])),1:3]
		Xl_y <- xxl[(which(xxl[,3]!=col[i])),1:3]
		Etalone1 <- rbind(Etalone1,FindEtalon(xxl,Xy,Xl_y))
	}
	
	Etalone <- iris[1,3:5]
	konk <- Etalone1
	
	for(i in 1:length(unique(xxl[,3]))){
		Xy <- xxl[(which(xxl[,3]==col[i])),1:3]
		dd <- FindEtalon(xxl,Xy,konk)
		if(dd[1]!=0)
			Etalone <- rbind(Etalone,dd)
	}
	
	Etalone <- Etalone[2:dim(Etalone)[1],]
	xl <- xxl
	x <- xl[,1:2]
	Xy <- xl
	n <- dim(xl)[1]
	U <- data.frame()
	k <- 1
	
	while(TRUE){
	
		if(k>n)
			break
		for( i in 1:n){
			if(i!=k){
		
				if(dim(Etalone[which(Etalone[,3]==xl[i,3]),1:3])[1]!=0){
					m1 <- NN(x[i,],Etalone[which(Etalone[,3]==xl[i,3]),1:3])
					m2 <- NN(x[i,],Etalone[which(Etalone[,3]!=xl[i,3]),1:3])
					print(x[k,])
					margin <- firis(x[k,],m1,m2)
		
					if(margin>0.5){
						U <- rbind(U,xl[i,])
					}
		
				}
			}
		}
		
		U=unique(U)
		m=dim(U)[1]
		#print(U)
		
		for(i in 1:m){
		
			xl <- xl[-which(U[i,1]==xl[,1] & U[i,2]==xl[,2]),]
			Xy <- Xy[-which(U[i,1]==Xy[,1] & U[i,2]==Xy[,2]),]
		}
		
		n <- dim(xl)[1]
		x <- xl[,1:2]
		print(n)
		print(m)
		
		if(dim(xl)[1]==0 | dim(U)[1]==0)
			break
			
		U <- data.frame()
		for(i in 1:3){
		
			if(dim(Xy[which(Xy[,3]==col[i]),1:3])[1]!=0){
				l <- FindEtalon(xl,Xy[which(Xy[,3]==col[i]),1:3],Etalone[which(Etalone[,3]!=col[i]),1:3])
				Etalone <- rbind(Etalone,l)
			}
		}
		k <- k+1
	}
	
	return(Etalone)
}

#lx <- iris[,3:5]
Etalone <- frisstolp(lx)

z <- c(1.45,0.2)

plot(lx[,1:2],bg = colors[lx[,3]], pch = 21, asp = 1,main = "FRIS STOLP", ylab = "y ", xlab = "x", col.lab = "red")
class <- parzen(Etalone,z,1,TRUE)
points(z[1],z[2],pch=0,col=colors[class])

plot(lx[,1:2],bg = colors[lx[,3]], pch = 21, asp = 1,main = "STOLP", ylab = "y ", xlab = "x", col.lab = "red")
class <- parzen(lx,z,1,TRUE)
points(z[1],z[2],pch=0,col=colors[class])

for(i in seq(1.30, 1.55, 0.01)){
	for(j in seq(0.05,0.3,0.01)){
	
	    z <- c(i, j)
	    class <- parzen(Etalone,z,1,TRUE)
	    points(z[1], z[2], pch = 1,col=colors[class])
	}
}