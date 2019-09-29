# СМПР 

1. Метрические алгоритмы классификации 
- [1NN](#1NN) 
# Метрические алгоритмы классификации 
## 1NN 

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

