# СМПР 

1. Метрические алгоритмы классификации 
- [1NN](#1NN) 
- [KNN](#KNN)
- [KWNN](#KWNN)
- [Loo](#Метод-скользящего-контроля(Loo))
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
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/5.png) 

## Преимущества 

1. Простота реализации 

## Недостатки 

1. Нужно хранить всю выборку 

2. При k=1 может иметь погрешность 

3. При большом k , алгоритм выдаёт одинаковый ответ.

## KWNN
---
Метод k-взвешенных ближайших соседей (KWNN). Имеется объект u , необходимо определить к какому классу он относится.

### Алгоритм

1. Выбирается в каждом классе k ближайших объектов к u.
2. Находим среднее расстояния до k ближайших объектов.
3. Выбираем класс с наименьшим расстоянием и относим объект u к этому классу.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/3.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/6.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/7.png)
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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/img/5.png) 

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



