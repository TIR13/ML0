# Линейные алгоритмы классификации

- [Адаптивны линейный элемент](#Адаптивны-линейный-элемент)
- [Персептрон Розенблатта](#Персептрон-Розенблатта)
- [Логистическая регрессия](#Логистическая-регрессия)
---

## Линейные алгоритмы классификации

Линейным классификатором называется алгоритм классификации вида:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/klass.gif), где 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/wj.gif) — вес j-го признака,  ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/w0.gif)  — порог принятия решения, ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/w.gif)  — вектор весов, ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/scall.gif)  — скалярное произведение признакового описания объекта на вектор весов.

Величина 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/margin.gif) называется отступом объекта x. Если отступ отрицателен, то алгоритм допускает ошибку на выбранном элементе.

Минимизацию суммарных потерь можно рассматривать как приближeнный метод минимизации эмпирического риска — числа ошибок на обучающей выборке:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/Q.png)

## Адаптивны линейный элемент

Адаптивны линейный элемент (ADALINE) - линейный алгоритм классификации, 
 в котором используется квадратичная функция потерь: ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/ada_loss.png), 
обновление весов происходит по формуле: ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/ada_upd.png)

Реализация 

```R
loss_ada <- function(xi, yi, w) {
	mi <- c(crossprod(w, xi)) * yi
	l <- (mi - 1)^2
	return(l)
}

upd_ada <- function(xi, yi, w, eta) {
	wx <- c(crossprod(w, xi))
	ld <- (wx - yi) * xi
	W <- w - eta * ld
	return(W)
}
```
Результат работы алгоритма при помощи ADALINE

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/map_adaline.png)

## Персептрон Розенблатта

Персептрон Розенблатта (Правило Хэбба) - линейный классификатор, обучаемый с помощью стохастического градиента с правилом Хэбба и кусочно-линейной функции потерь: ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/hab_loss.png), 
обновление весов происходит по формуле: ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/hab_upd.png)

Реализация 

```R
loss_hab <- function(xi, yi, w) {
	mi <- c(crossprod(w, xi)) * yi
	return (max(-mi, 0))
}

# правило Хебба для весов
upd_hab <- function(xi, yi, w, eta) {
	W <- w + eta * yi * xi
	return(W)
}
```

### Пример 
Результат работы алгоритма при помощи Правило Хэбба

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/map_habb.png)

## Логистическая регрессия

Метод логистической регрессии основан на довольно сильных вероятностных предположениях, которые имеют несколько интересных последствий:

1. линейный классификатор оказывается оптимальным байесовским;
2. однозначно определяется функция потерь;
3. можно вычислять не только принадлежность объектов классам, но также получать и численные оценки вероятности их принадлежности.

Рассмотрим основные функции:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/Log_loss.png) - функция потерь

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/Log_upd.png) - функция обновления весов

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/ver.png) - вероятность принадлежности к классу 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/sigmoida.png) - сигмоида

Реализация

```R
sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
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
```

### Пример работы логистической регрессии, чем больше вероятность принадлежность элекента к классу, тем темнее цвет:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/map_log.png)

Результат работы всех трёх алгоритмов ADALINE(зелёным), Правило Хэба(красным), Логистическая регрессия(синим):

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/line/img/map_combo.png)
