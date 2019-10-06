# СМПР 

1. Метрические алгоритмы классификации 
- [1NN](#1NN) 
- [KNN](#KNN)
- [KWNN](#KWNN)
- [Loo](#Метод-скользящего-контроля(Loo))
- [Метод парзеновского окна](#Метод-парзеновского-окна)
# Метрические алгоритмы классификации 
## 1NN 
---
Метод ближайших соседей (1NN). Относит классифицируемый объект к тому классу , к которому принадлежит его ближайший сосед 


![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/1.png) 



Суть метода: 

1.Рассмотрим обучающую выборку "Ирисы Фишера". Это будут наши "соседи". 

2.Найдём кратчайшее расстояние от тестируемого объекта к соседям 

3.Сортируем выборку согласно расстояния до классифицируемого объекта 

4.Определяется класс тестируемого объекта. 

Функция **W~y~=(z,X^l^,k)=w~y~(z,x~z,1~,...,x~z,k~)** - определяет степень принадлежности объекта x классу y 

где x~z,j~ - j-ый по близости к объекту z 

y~z,j~ - ответ на j-ом соседе 

**Реализация весовой функции:** 

``` 
l <- dim(xl)[1] 
n <- dim(xl)[2] - 1 
## Матрица расстояний 
distances <- matrix(NA, l, 2) 
for (i in 1:l) 
{ 
distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z)) 
} 
## Сортируем по возрастанию 
orderedXl <- xl[order(distances[, 2]), ] 

``` 

## Пример 

Рассмотрим выборку "Ирисы Фишера" и некую точку Z(1.7,1) 

Применим метод 1NN и получим , что Z принадлежит классу "красных кружочков" 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/2.png) 

## **Преимущества** 

1. Простота в реализации :) 

## **Недостатки** 

1.Неустойчивость к погрешностям 

2.Отсутствие параметров, которые можно было бы настраивать по выборке 

3.Алгоритм полностью зависит от того, насколько "удачно" выбрана метрика р 

4.Низка качество классификации 

## KNN 
---

Метод k ближайших соседей(KNN). Относит классифицируемый объект к тому классу , к которому принадлежат его k ближайшие соседи. 

Суть метода: 

1.Рассмотрим обучающую выборку "Ирисы Фишера". Это будут наши "соседи". 

2.Найдём кратчайшее расстояние от тестируемого объекта к соседям 

3.Сортируем выборку согласно расстояния до классифицируемого объекта 

4.Определяется класс к которому принадлежит большая часть из ближайших k соседей. 

Функция **W~y~=(z,X^l^,k)=w~y~(z,x~z,1~,...,x~z,k~)** - определяет степень принадлежности объекта x классу y 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/4.png) 

x~z,j~ - j-ый по близости к объекту z 

y~z,j~ - ответ на j-ом соседе 

k выбираем методом скользящего контроля Loo 

**Реализация KNN функции:** 

``` 
kNN <- function(xl, z, k) 

{ 

orderedXl <- sortObjectsByDist(xl, z) 

n <- dim(orderedXl)[2] - 1 

classes <- orderedXl[1:k, n + 1] 

counts <- table(classes) 

class <- names(which.max(counts)) 

return (class) 

} 

``` 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/3.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Looknn.png) 

## Преимущества 

1. Простота реализации 

## Недостатки 

1. Нужно хранить всю выборку 

2. При k=1 может иметь погрешность 

3. При большом k , алгоритм выдаёт одинаковый ответ.

## KWNN
---
Метод k-взвешенных ближайших соседей (KWNN). Имеется объект u , необходимо определить к какому классу он относится. 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/kwnn.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/kwnn1.png) 

### Алгоритм

1. Выбирается в каждом классе k ближайших объектов к u.
2. Находим среднее расстояния до k ближайших объектов.
3. Выбираем класс с наименьшим расстоянием и относим объект u к этому классу.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/3.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/6.png)

Найдём оптимальное q для алгоритма kwnn с помощью Loo, при k=6
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/7.png)

Оптимальное q при k=6 - это q=1
**Реализация KWNN:**

```
kwNN <- function(xl, z, k, q) 
{
  n <- dim(orderedXl)[2] - 1
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)  
  classes <- orderedXl[1:k, n + 1] 
  counts <- table(classes)
  name <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:k ){
    w <- q ^ i
    name[[classes[i]]] <- name[[classes[i]]] + w
  }
  cl  ass <- names(which.max(name))
  return (class)
}

```
## Преимущество:

1. Простая реализация
2. Учитывает расстояние до соседа

## Недостатки

1. Нужно хранить всю выборку


## Сравнение KNN и KWNN
---
KWNN отличается от KNN тем , что учитывает ранг соседей от классифицируемого объекта, тем самым уменьшая шанс ошибиться .

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/7kwnn.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/7knn.png)

## Метод скользящего контроля (Loo):
---
1.Удаляем 1 элемент из выборки.

2.Определяем при помощи KNN или KWNN к какому классу элемент принадлежит

3.Проверяем правильно ли классифицировали элемент, если нет , то увеличиваем счетчик.

4.Для каждого элемента проделываем

5.Делим полученный счетчик на количество элементов (это будет ошибка)

6.Проделываем для всех k, ответом будет k с наименьшей ошибкой.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Looknn.png) 

## Реализация метода Loo:

```
Loo <- function(k,xl)
{
    sum =0
    for(i in 1:dim(xl)[1])
    {
        tmpXL <- rbind(xl[1:i-1, ],
        xl[i+1:dim(xl)[1],])
        xi <- c(xl[i,1], xl[i,2])
        class <-kNN(tmpXL,xi,k)
        if(class != xl[i,3])
             sum=sum+1
    }
    sum=sum/dim(xl)[1]
    return(sum)
}
```
# Метод парзеновского окна

Рассмотрим весовую функция 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо2.png)  как функцию не от ранга соседа, а как функция от расстояния ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо.png) 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо1.png)
,где K - невозрастающая и неотрицательная функция ядра  
В этом случае метрический классификатор:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо3.png) 

# Метод парзеновского окна

Рассмотрим весовую функция 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо2.png)  как функцию не от ранга соседа, а как функция от расстояния ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо.png) 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо1.png)
,где K - невозрастающая и неотрицательная функция ядра  
В этом случае метрический классификатор:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпо3.png) 

h - шириной окна , u - играет ту же роль, что и число соседей. "Окно" - это сферическая окрестность u радиуса h, при попадании в которою облегающий объект xi голосует за отнесение объекта u к классу yi

Рассмотрим формулы всех ядер:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/мпоф.png)

## Ядро Епонечникова

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Loo_epan.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/map_epan.png)
 
Реализация функции
```
epan <- function(r,h){
  if(abs(r/h) <= 1){
      return (3/4*(1-(r/h)^2))
    } 
   else {
      return(0)
  }
}
```

## Ядро Прямоугольное

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Loo_rect.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/map_rect.png)
 
Реализация функции
```
rectangle <- function(r,h){
  if(abs(r/h) <= 1){
      return (0.5)
    } 
   else {
      return(0)
  }
}
```
## Ядро Треугольное

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Loo_trey.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/map_trey.png)

Реализация функции
```
trey <- function(r,h){
  if(abs(r/h) <= 1){
      return ((2*pi)^(-0.5)*exp(-0.5*(r/h)^2))
    } 
  else {
      return(0)
  }
}
```
## Ядро Квадратное

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Loo_kvad.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/map_kvad.png)
 
Реализация функции
```
kvad <- function(r,h){
  if(abs(r/h) <= 1){
      return (15/16*(1-(r/h)^2)^2)
    } 
  else {
      return(0)
  }

}
```
## Ядро Гаусса

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/Loo_gaus.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/map_gaus.png)
 
Реализация функции
```
gaus <- function(r,h){
 if(abs(r/h) <= 1){
    return ( (2*pi)^(-1/2) * exp(-1/2 * (r/h)^2 ) )
  } 
  else {
    return(0)
  }
}
```





