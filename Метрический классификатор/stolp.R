# your code goes here
euclideanDistance <- function(u, v)
{
	return (sqrt(sum((u - v)^2)))
}
gaus <- function(r){
	return (((2*pi)^(-1/2)) * exp(-1/2*r^2))
}

parzen <- function(xl,y,h,map)
{
	n <- dim(xl)[1]
	weights = rep(0,3)
	names(weights) = c("setosa", "versicolor", "virginica")
	for(i in 1:n)
	{
	    x <- xl[i,1:2]
	    class <- xl[i,3]
	    r <- euclideanDistance(x,y)/h
	    weights[class] <- gaus(r)+weights[class]
	}
	class <- names(which.max(weights))
	if(max(weights)==0){
    	if(map==TRUE)
    		return("silver") 
    	else
    		return(0)
	}
	else{
    	if(map==TRUE)
    		return(class) 
    	else
    		return(max(weights))
    
	}
}

margin <- function(xl,classes,z,class){

	class1 <- xl[which(classes==class), ]
	class2 <- xl[which(classes!=class), ]
	margin <- parzen(class1,z[1:2],1,FALSE) - parzen(class2,z[1:2],1,FALSE)
	
	return(margin)
  
}

stolp <- function(xl, classes,errors) {

	n <- length(classes)
	margins <- rep(0, n)
	for (i in 1:n){
		margins[i] <- margin(xl, classes, xl[i,], classes[i])
	}
	
	wrong <- which(margins < 0)
	pointsWE <- xl[-wrong,]
	classes <- classes[-wrong]
	n <- n - length(wrong)
	etalone <- data.frame()
	  
	for (class in unique(classes)) {
		print(class)
		ind <- which(class == classes )
	    margins <- sapply(ind, function(i) margin(pointsWE, classes, pointsWE[i,], class))
	    max_marg <- ind[which.max(margins)]
	    etalone <- rbind(etalone, pointsWE[max_marg,])
	    pointsWE <- pointsWE[-max_marg,]
	    classes <- classes[-max_marg]
	    n <- n-1
	}
	names(etalone) <- names(xl)
	while(n!=length(etalone)){
	    count <- 0
	    margins <- c()
	    index <- c()
	    for(i in 1:n)
	    {
	    	m <- margin(etalone, etalone[,3], pointsWE[i,], classes[i])
	    	if(m<=0){
	        	count <- count+1;
	        	margins <- c(margins, m)
	        	index <- c(index,i)
	    	}
	    }
	    print(count)
	    
	    if( count < errors )
	    {
	    	plot(pointsWE[,1:2],col = colors[classes], pch = 21, asp = 1,
	           main = "STOLP для Гаусовского ядра", ylab = "y ", xlab = "x", col.lab = "red")
	    	points(etalone[,1:2], bg = colors[etalone[,3]], pch = 21)
	    	print(etalone)
	    	break;
	    }
	 
	    min_marg <- index[which.min(margins)]
	    etalone <- rbind(etalone, pointsWE[min_marg,])
	    pointsWE <- pointsWE[-min_marg,]
	    classes <- classes[-min_marg]
	    n <- n - 1
	    
	    }
        return(etalone)
}
colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue","grey"="grey")
xl <- iris[, 3:5] 
classes <- iris[, 5]
etalone=stolp(xl,classes,3)

plot(etalone[,1:2],bg = colors[etalone[,3]], pch = 21, asp = 1,
           main = "Карта Классификации STOLP", ylab = "y ", xlab = "x", col.lab = "red")
start <- Sys.time()
     
for(i in seq(0, 7, 0.1)){
	for(j in seq(-1,3.3,0.1)){
	    z <- c(i, j)
	    class <- parzen(xl,z,1,TRUE)
	    points(z[1], z[2], pch = 1,col=colors[class])
	}
}

print(Sys.time() - start)

start <- Sys.time()
     
for(i in seq(0, 7, 0.1)){
	for(j in seq(-1,3.3,0.1)){
	    z <- c(i, j)
	    class <- parzen(etalone[,1:2],z,1,TRUE)
	    points(z[1], z[2], pch = 1,col=colors[class])
	}
}

print(Sys.time() - start)

n <- length(classes)
margins <- rep(0, n)
for (i in 1:n){
	margins[i] <- margin(xl, classes, xl[i,], classes[i])
}
	
wrong <- which(margins < 0)
wrong_margin <- rep(0,length(wrong))
for (i in 1:length(wrong)){
	wrong_margin[i]=margins[wrong[i]]
}
  
wrong_margin = rev(sort(wrong_margin))
for (i in 2:length(wrong)){
    vibros<-0
    if(abs(wrong_margin[i]-wrong_margin[i-1])>1)
    {
    	vibros=i
    }
  }

plot(1:n, sort(margins), col="white", bg="blue",pch=20,
       main = "Отступы для Парзеновского окна",
       ylab = "Отступ ", xlab = "Данные", col.lab = "red")
m_sort <- sort(margins) 
violet = rgb(0.4, 0, 0.6)
require("plotrix")
col<-"black"
for( i in 1:149){
	#print(m_sort[i])
	if(m_sort[i]==wrong_margin[vibros] )
		color="red"
	if(m_sort[i]>=wrong_margin[vibros-1] & m_sort[i] < -0.2)
		color=adjustcolor(violet, alpha = 0.5)
	if(abs(m_sort[i])<=0.2)
		color="yellow"
	if(m_sort[i]<100 & m_sort[i]>0.2)
		color=adjustcolor("green", alpha = 0.3)
	if(m_sort[i]>=100)
		color="darkgreen"
	if(m_sort[i+1] > 13 & color!="darkgreen"){
		color="darkgreen"
		lines(x, y, lwd = 2, col = "darkgreen")
	}
	x=iris[1,3:4]
	x[1,1:2] <- c(i,i+1)
	y=iris[1,3:4]
	y[1,1:2] <- c(m_sort[i],m_sort[i+1])
	lines(x, y, lwd = 2, col = color)
	if(m_sort[i]<0 & m_sort[i+1]>0 & color!="yellow"){
		x[1,1:2] <- c(i,i+1)
		y[1,1:2] <- c(-0.2,0.2)
		lines(x, y, lwd = 2, col = "yellow")
	}
	
	if(col!=color){
		draw.circle(i,m_sort[i],1,30,border="black",col="black")
		col=color
	}
}
lines(c(1, n), c(0, 0), col = "grey", lwd = 2)
  
label = paste("   Ошибочный объект \n  Margin = ", round(wrong_margin[vibros-1],3), sep = "")
text(vibros-1, wrong_margin[vibros-1], labels = label, pos=4, col = "red")
  
label = paste("   Выброс \n  Margin = ", round(wrong_margin[vibros],3), sep = "")
text(vibros, wrong_margin[vibros]+0.2, labels = label, pos=4, col = "red")
  
  
  
  
  
  
