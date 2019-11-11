# Байесовские алгоритмы классификации
- [Линии уровня нормального распределения](#Линии-уровня-нормального-распределения)
- [Наивный байесовский классификатор](#Наивный-байесовский-классификатор)
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

## Геометрический смысл:

1.Если признаки некорелированы, то линия уровня плотности распределения имеют форму элипсоидов.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/2.png) 

2.Если признаки имеют одинаковые дисперсии, то элипсоиды являются сферами.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/1.png) 

3.Если признаки корелированы, то матрица не диагональна и линии уровня имеют форму элипсоидов, оси которых повернуты относительно исходной системы координат.

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
