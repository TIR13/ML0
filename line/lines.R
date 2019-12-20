library("MASS")
library(plotrix)

normali <- function(xl) {     
	n <- dim(xl)[2]
	for(i in 1:n)      
	{
		min <- min(xl[,i])
		max <- max(xl[,i])         
		xl[, i] <- (xl[, i] - min)/(max-min)
	} 
    
    return(xl) 
} 

loss_ada <- function(xi, yi, w) {
	mi <-sum(w*xi) * yi
	l <- (mi - 1)^2
	return(l)
}

# дельта правило обновления для ADALINE
upd_ada <- function(xi, yi, w, eta) {
	wx <- sum(w * xi)
	ld <- (wx - yi) * xi
	W <- w - eta * ld
	return(W)
}

# Кусочно-линейную функцию потерь для Хебба
loss_hab <- function(xi, yi, w) {
	mi <- sum(w * xi) * yi
	return (max(-mi, 0))
}

# правило Хебба для весов
upd_hab <- function(xi, yi, w, eta) {
	W <- w + eta * yi * xi
	return(W)
}
sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
}

loss_Log <- function(xi, yi, w) {
     mi <- c(crossprod(w, xi)) * yi
     l <- log2(1+exp(-mi))
     return(l) 
} 
upd_Log <- function(xi, yi, w, eta) {
     W <- w+eta*xi*yi*sigmoid(-sum(w*xi)*yi)
     return(W)

}  
sg <- function(xl,loss,upd,coll, eta = 1, lambda = 1/6, eps = 1e-5) {     
	l <- dim(xl)[1]     
	n <- dim(xl)[2] - 1        
	w <- rep(0.5, n)     
	iterCount <- 0
	        
	## initialize Q     
	Q <- 0     
	
	for (i in 1:l)     
	{         
		## calculate the scalar product <w,x>         
		xi <- xl[i, 1:n]
		yi <- xl[i, n + 1] 
		          
		Q <- Q + loss(xi, yi, w)        
	}  
	  
	iterCount <- 0    
	repeat     
	{         
		if (iterCount > 1000) {
      		
      		break
    		}
    
		## calculate the margins for all objects of the training sample         
		margins <- array(dim = l)           
		for (i in 1:l){         
			xi <- xl[i, 1:n]             
			yi <- xl[i, n + 1]                          
<<<<<<< HEAD:line/lines.R
			margins[i] <-  crossprod(w , xi) * yi          
=======
			margins[i] <-  sum(w * xi) * yi          
>>>>>>> ac42e125b938bf3cbbc377fb4ce22517eaeee7c6:line/ada_hab.R
		}  
		            
		## select the error objects         
		errorIndexes <- which(margins <= 0)              
		if (length(errorIndexes) > 0)         
		{             
			# select the random index from the errors             
			i <- sample(errorIndexes, 1)             
			iterCount <- iterCount + 1                           
			xi <- xl[i, 1:n]             
			yi <- xl[i, n + 1]               
			           
			                      
			## calculate an error             
			ex <- loss(xi, yi, w)
<<<<<<< HEAD:line/lines.R
	                eta <- 1 / sqrt(sum(xi * xi))
			w <- upd(xi, yi, w, eta)    
			if(coll!="unknown"){
				x <- seq(-2, 2, len = 100)
				f <- function(x) {
					return( - x*w[1]/w[2] + w[3]/w[2] )
				}
				y <- f(x)
				lines(x, y, type="l",col=coll)
			}                      
=======
	                  eta <- 1/iterCount
			w <- upd(xi, yi, w, eta)    
			x <- seq(-2, 2, len = 100)
			f <- function(x) {
				return( - x*w[1]/w[2] + w[3]/w[2] )
			}
			y <- f(x)
			lines(x, y, type="l",col=coll)                      
>>>>>>> ac42e125b938bf3cbbc377fb4ce22517eaeee7c6:line/ada_hab.R
			## Calculate a new Q             
			Qprev <- Q             
			Q <- (1 - lambda) * Q + lambda * ex
			if (abs(Q - Qprev) < 1e-5) {
			      break
			} 
     
		}         
		else         
		{             
			break         
		}    
		
	}  
    return(w) 
} 


n <- 100

sigma1 <- matrix(c(2,0, 0, 2), 2, 2)
sigma2 <- matrix(c(2, 0,0, 2), 2, 2)

<<<<<<< HEAD:line/lines.R
mu1 <- c(4, 4)
mu2 <- c(12, 4)
=======
mu1 <- c(4, 0)
mu2 <- c(0, 4)
>>>>>>> ac42e125b938bf3cbbc377fb4ce22517eaeee7c6:line/ada_hab.R

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)
xl <- rbind(xy1,xy2)
xl <- normali(xl)
xl <- cbind(xl, rep(-1, n+n))
xl <- cbind(xl, c(rep(-1, n), rep(1, n)))


plotxmin <- min(xl[,1], xl[,1]) - 0.3
plotxmax <- max(xl[,1], xl[,1]) + 0.3
plotymin <- min(xl[,2], xl[,2]) - 0.5
plotymax <- max(xl[,2], xl[,2]) + 0.5

# Рисуем обучающую выборку
colors <- c("gray", "orange")
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE & HEBB")

points(xl, pch=21, col=colors[ifelse(xl[,4] == -1, 1, 2)], bg=colors[ifelse(xl[,4] == -1, 1, 2)])
ada_res <- sg(xl, loss = loss_ada, upd=upd_ada,coll="green")
abline(a = ada_res[3] / ada_res[2], b = -ada_res[1] / ada_res[2], lwd = 3, col = "green3")
<<<<<<< HEAD:line/lines.R

hab_res <- sg(xl, loss = loss_hab, upd=upd_hab,coll="red")
abline(a = hab_res[3] / hab_res[2], b = -hab_res[1] / hab_res[2], lwd = 3, col = "red3")


plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="LOGG")

points(xl, pch=21, col=colors[ifelse(xl[,4] == -1, 1, 2)], bg=colors[ifelse(xl[,4] == -1, 1, 2)])
Log_res <- sg(xl, loss = loss_Log, upd=upd_Log,coll="unknown")
abline(a = Log_res[3] / Log_res[2], b = -Log_res[1] / Log_res[2], lwd = 3, col = "blue")


ver <- function(x,w){
	return(sigmoid(c(crossprod(w,x)) * (-1)) - sigmoid(c(crossprod(w,x)) * 1))
}

for (i in seq(len=50, plotxmin, plotxmax)) {
	for (j in seq(len=50, plotymin, plotymax)) {
	    p <- ver(c(i,j,-1),Log_res)
	    coll <- adjustcolor(colors[ifelse(p>0,1,2)], abs(p))
	    rect(i,j,i+0.05,j+0.05,col = coll, border = coll)
  }
}


=======
>>>>>>> ac42e125b938bf3cbbc377fb4ce22517eaeee7c6:line/ada_hab.R
