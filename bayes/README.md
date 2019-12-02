# Байесовские алгоритмы классификации
- [Линии уровня нормального распределения](#Линии-уровня-нормального-распределения)
- [Наивный байесовский классификатор](#Наивный-байесовский-классификатор)
- [Plug-in алгоритм](#Plug-in-алгоритм)
- [LDF](#Линейный-Дискриминант-Фишера)
---
## Линии уровня нормального распределения
Вероятностное распределение с плотностью ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/tex.png) 
называется n-мерным многомерным нормальном распределением
с математическим ожиданием (центром) ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/muinr.png)
и ковариационной матрицей ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/sigmainr.png) 
 (матрица симметрична, невырожденная, положительно определенная).
 
Реализация 
```R
line <- function(m,A)
{
	determ<-det(A)
	
	a <- A[2,2]/determ
	b <- -A[2,1]/determ
	c <- -A[1,2]/determ
	d <- A[1,1]/determ
	
	x0 <- m[1]
	y0 <- m[2]
  
	x <- seq(-2.5, 2.5, 0.1)
	y <- seq(-2.5, 2.5, 0.1)
	
	A <- d
	B <- a
	C <- -c-b
	D <- -2*d*x0+y0*(c+b)
	E <- -2*a*y0+x0*(c+b)
	F <- d*x0^2+a*y0^2+x0*y0*(-c-b)
	
	func <- function(x, y) {
    	1/(2*pi*sqrt(determ))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
	}
	
	z <- outer(x, y, func)
  
	contour(x, y, z)
}
```

Признаки некорелированы, то линия уровня плотности распределения имеют форму элипсоидов.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/2.png) 

Признаки имеют одинаковые дисперсии, то элипсоиды являются сферами.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/1.png) 

Признаки корелированы, то матрица не диагональна и линии уровня имеют форму элипсоидов, оси которых повернуты относительно исходной системы координат.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/3.png) 

## Наивный байесовский классификатор
---
### Формула Байеса 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/1b.gif) 

1.  ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/2b.gif) - - Апостериорная вероятность, т.е. вероятность того, что объект x принадлежит классу y.
2. ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/4b.gif) - функция правдободобия. 
3. ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/3b.gif) - Априорная вероятность, т.е. вероятность появления класса.

### Наивный байесовский классификатор

Будем полагать, что все объекты описываются n числовыми признаками. Обозначим через ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/5b.gif) произвольный элемент пространства объектов ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/6b.gif). Предположим, что все признаки ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/9b.gif). являются независимыми случайными величинами. Следовательно, функции правдоподобия классов представимы в виде, 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/7b.gif). где ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/8b.gif). - плотность распределений значений jго признака для класса y. 

Оценивать n одномерных плотностей гораздо проще, чем одну n-мерную плотность. Однако данное предположение крайне редко работает на практике, поэтому алгоритмы, использующий его, называют наивным байесовким методом. 

Решающее правило принимает вид:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/naivv.gif)

Реализация

```R
naiv <- function(x, mu, sigma, lamda, P){
	n <- 2
	res <- log(lamda*P)
	
	for(i in 1 : n){
		pyj <- (1/(sigma[i]*sqrt(2*pi))) * exp(-1 * ((x[i] - mu[i])^2)/(2*sigma[i]^2))
    	res <- res + log(pyj)
	}
	
	return(res)
}

```

Математическое ожидание для (0;0) (4;4):
```
(0.009166359; 0.062009774)
(4.040577; 4.042877)
```
Дисперсия

```
1.919682 1.912058

1.0718737 0.8965156
```

### Пример
Имеется выборка 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/naiv.png)

Построим карту классификации для наивного байесовского алгоритма 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/map_naiv.png)

### Плюсы:

- Простота реализации.
 - Низкие вычислительные затраты при обучении и классификации.
 - Если признаки независимы, то алгоритм оптимален.

### Минусы:

В общем случае - низкое качество классификации

## Plug-in алгоритм

Если восстанавливать параметры нормального распределения ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/muinr.png), ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/sigmainr.png)  для каждого класса и подставляя в формулу оптимального байесовского классификатора восстановленные плотности, получим подстановочный (plug-in) алгоритм классификации. Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/plug_in.png)

Разделяющая поверхность между двумя классами s и t задаётся следующим образом:

![raspr](https://camo.githubusercontent.com/55e2315c9f2b500bd677580e4eba7a90990bbee3/68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f5c6c616d6264615f73505f735c72686f5f732878292673706163653b3d2673706163653b5c6c616d6264615f74505f745c72686f5f74287829)

Прологарифмируя обе части выражения и проведя преобразования получим уровнение разделяющей поверхности.
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/plugin.gif)

Реализация:

```R
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


```

## Пример работы

Классификация при помощи подстановочного алгоритма:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/pluginn.png)

### Линия

Для мат ожидания в точке (5;5) и (15;15) с матрицей (1,0,0,1) и (1,0,0,1)

```
mu1:
4.890255 5.012199

mu2:
15.0881 15.10856

sigma1:
0.76108872 0.05076374
0.05076374 1.25389165

sigma2:
 0.950205993 -0.004565248
-0.004565248  1.074333608
```
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/pl_3.png)

### Эллипс

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/pl_1.png)

### Гипербола 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/pl_2.png)

## Линейный Дискриминант Фишера
---

Теперь рассмотрим линейный дискриминант Фишера (ЛДФ), который, в отличии от подстановочного алгоритма, при построении предполагает, что ковариационные матрицы классов равны, и для их восстановления нужно использовать все объекты обучающей выборки.
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/ldf1.png)

Реализация восстановления матрицы

```R
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
```

### Пример
Классификация 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/ldf_klass.png) 


Для мат ожидания в точке (1;0) и (15;0) с матрицей (2,0,0,2)

```
mu1:
0.9304235 -0.06200572

mu2:
15.06322 -0.1762103

sigma:
1.953001883 -0.001501422
-0.001501422  1.83128977965
```
