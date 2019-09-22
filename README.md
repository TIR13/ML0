# СМПР

1. Метрические алгоритмы классификации
    - [1NN](#1NN)
    

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
##  Матрицf расстояний    
distances <- matrix(NA, l, 2)          
for (i in 1:l)      
{         
   distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
}
##  Сортируем по возрастанию   
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



