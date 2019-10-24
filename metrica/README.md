#  Метрические алгоритмы классификации 
- [1NN](#1NN) 
- [KNN](#KNN)
- [KWNN](#KWNN)
- [LOO](#Метод-скользящего-контроля(LOO))
- [Метод парзеновского окна](#Метод-парзеновского-окна)
- [Метод Потенциальных функций](#Метод-Потенциальных-функций)
- [Алгоритм STOLP](#Алгоритм-STOLP)
# Метрические алгоритмы классификации 
## 1NN 
---
Метод ближайших соседей (1NN). Относит классифицируемый объект к тому классу , к которому принадлежит его ближайший сосед 


![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/1.png) 



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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/2.png) 

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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/4.png) 

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
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/3.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Looknn.png) 

## Преимущества 

1. Простота реализации 

## Недостатки 

1. Нужно хранить всю выборку 

2. При k=1 может иметь погрешность 

3. При большом k , алгоритм выдаёт одинаковый ответ.

## KWNN
---
Метод k-взвешенных ближайших соседей (KWNN). Имеется объект u , необходимо определить к какому классу он относится. 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/kwnn.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/kwnn1.png) 

### Алгоритм

1. Выбирается в каждом классе k ближайших объектов к u.
2. Находим среднее расстояния до k ближайших объектов.
3. Выбираем класс с наименьшим расстоянием и относим объект u к этому классу.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/3.png) 
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/6.png)

Найдём оптимальное q для алгоритма kwnn с помощью Loo, при k=6
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/7.png)

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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/7kwnn.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/7knn.png)

## Метод скользящего контроля (LOO):
---
1.Удаляем 1 элемент из выборки.

2.Определяем при помощи KNN или KWNN к какому классу элемент принадлежит

3.Проверяем правильно ли классифицировали элемент, если нет , то увеличиваем счетчик.

4.Для каждого элемента проделываем

5.Делим полученный счетчик на количество элементов (это будет ошибка)

6.Проделываем для всех k, ответом будет k с наименьшей ошибкой.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Looknn.png) 

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
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/мпо2.png)  как функцию не от ранга соседа, а как функция от расстояния ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/мпо.png) 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/мпо1.png)
,где K - невозрастающая и неотрицательная функция ядра  
В этом случае метрический классификатор:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/мпо3.png) 

h - шириной окна , u - играет ту же роль, что и число соседей. "Окно" - это сферическая окрестность u радиуса h, при попадании в которою облегающий объект xi голосует за отнесение объекта u к классу yi

Рассмотрим формулы всех ядер:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/мпоф.png)

## Ядро Епонечникова

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Loo_epan.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_epan.png)
 
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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Loo_rect.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_rect.png)
 
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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Loo_trey.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_trey.png)

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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Loo_kvad.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_kvad.png)
 
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

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/Loo_gaus.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_gaus.png)
 
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
## Плюсы Парзеновского окна:
- При правильно выбраном h алгоритм способен классифицировать объект с хорошим качеством;
- Алгоритм прост в реализации;
- Учитывются все точки с одинаковым расстоянием;
## Минусы:
- Нужно подбирать h для каждой выборке
- Требуется хранить выборку целиком;


# Метод Потенциальных функций
---
Если в методе Парзеновского окна, центр окна поместить в классифицируемый объект, то получим метод Потенциальных функций.
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/pf1.png)

Реализация потенциальной функции:

```
pot_func <- function(potentials,xl,y,h){

n <- dim(xl)[1]
w <- rep(0,3)
names(w) <- c("setosa", "versicolor", "virginica")

for(i in 1:n)
{
   x <- xl[i,1:2]
   class <- xl[i,3]
   r <- euclideanDistance(x,y);
   w[class] <- potentials[i]*gaus(r,h)+w[class]

}

class <- names(which.max(w))

if(max(w)==0){
   return ("0")
}
else{
   return (class)
}

}
```

Алгоритм нахождения gamma[i]:

1. gamma[i]=0 i=1,...,n
2. Из выборки выбирается элемент x[i]
3. Вызываем для этого объекта функцию потенциалов
4. Если полученный класс не совпал с реальным, то сила потенциала для выбранного объекта увеличивается на 1. 
Иначе снова выбирается объект и классифицируется.
5. Алгоритм классификации с полученными значениями потенциалов запускается для каждого объекта выборки.
6. Подсчитывается число ошибок.
7. Если число ошибок меньше заданного, то алгоритм завершает работу. Иначе снова выбирается объект из выборки.


Реализация:

```
potentials <- function(xl,class,n,h,errors){
e <- 100
pots <- rep(0,n)

   while(e>error){

      while(TRUE){

         z <- sample(1:n,1)
         x <- xl[z,1:2]
         point <- pot_func(pots,xl,x,h)

         if (colors[point] != colors[class[z]]) {
            pots[z] <- pots[z] + 1
            break
         }
      }

      e <- 0

      for (i in 1:n) {
          x <- xl[i,1:2]
          points <- xl[-i,1:3]
              if (colors[pot_func(pots,points,x, h)]!= colors[class[i]]){
                 e <- e + 1
              }
      }
   }

return(pots)

}
```
Результат работы алгоритма для Гауссовского ядра, при h=1

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/poten_func.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/pf_map.png)

Список потенциалов:

```
[1] 1
[1] 51
[1] 71
[1] 92
[1] 102
[1] 107
[1] 120
[1] 147

```

## Плюсы:

- высокая точность классификации

## Минусы:

- сложная реализация
- при маленьком eps долгое выполнение


## Алгоритм STOLP

Отступ - величина, показывающая, степень типичности объекта к классу. Отступ равен разности между степенью близости объекта к своему классу и максимальной близостью объекта к в другому классу. Отступ отрицателен, тогда и только тогда, когда алгоритм допускает ошибку на данном объекте.
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/margin.png) 

Объекты по отступам делятся на виды:

 - Эталонные объекты - наиболее типичные представители своего класса. (Отступ - большое положительное число)
 - Неинформативные - объекты, не влияющие значительным образом на качество классификации (Отступ - положительное число)
 - Пограничные - объекты, имеющие отступ, близкий к нулю. Незначительное изменение в выборке может повлиять на их классификацию.
 - Ошибочные - объекты с отрицательными отступами, классифицируемые неверно. 
 - Шумовые объекты (выбросы) - малая группа объектов с большими отрицательными отступами. Их удаление улучшает качество классификации.

Алгоритм СТОЛП (STOLP) — алгоритм отбора эталонных объектов для метрического классификатора. Смысл алгоритма, оставить в выборке только эталонные объекты. На вход подаётся выборка, допустимый порог ошибок и порог фильтрации выбросов.

Алгоритм:

1. Удалить из выборки ошибочные элементы.
2. Взять по одному объекту из каждого класса с наибольшим отступом и добавить в множество эталонов.
3. Классифицировать объекты обучающей выборки, взяв в качестве обучающей выборки для этого множество эталонов. Посчитать число ошибок.
4. Если число меньше заданного числа, то завершить алгоритм.
5. Иначе присоединить ко множеству эталонов объекты с наименьшим отступом из каждого класса.
6.Повторять шаги 3-5 до тех пор, пока множество эталонов и обучающая выборка не совпадут.

Функция нахождения отступа:

```
margin <- function(xl,classes,z,class){

	class1 <- xl[which(classes==class), ]
	class2 <- xl[which(classes!=class), ]
	margin <- parzen(class1,z[1:2],1) - parzen(class2,z[1:2],1)
	return(margin)
}

```
Осталось 5 эталонных объектов. Скорость работы метода после алгоритма заметно улучшилась,а именно с 29 mins до 1 mins.

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/STOLP.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/stolp_map.png)

Отступы для Парзеновского окна:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/grafik_stolp.png)

## FRIS STOLP
Одной из основных проблем, возникающих при решении задачи классификации каких-либо объектов, является проблема выбора меры схожести. Чаще всего в этой роли выступает кратчайшее расстояние, однако в некоторых задачах лучше использовать иную меру. Например функцию конкурентного сходства (Fris-фукция ) FRiS — мера схожести двух объектов относительно некоторого третьего объекта. Эта функция позволяет не просто сказать, похожи объекты друг на друга или нет, но и уточнить ответ на вопрос «по сравнению с чем?». Это позволяет большее количество факторов при классификации.

Пусть дано некоторое множество объектов М с заданной метрикой ρ(a, b). Функция конкурентного сходства объектов a, b ∈ M относительно x ∈ M задается так:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris.png)

Реализацтя функции

```R
firis <- function(a,u,b){
	S <- (dis(a,b)-dis(a,u))/(dis(a,b)+dis(a,u))
	return(S)
}

```

### Пример

Пусть даны 2 класса объектов (красные и зелёные). Нужно определить к какому классу отнести черный квадрат

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_stolp.png)

Квадрат ближе находится к объектам зелёного класса, но судя по структуре он принадлежит объектам из красного класса. Рассмотрим результат работы при помощи Fris функции и без неё

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_stolp1.png)
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_stolp2.png)

Другим применением FRiS-функции является один из алгоритмов отбора эталонных образцов для метрического классификатора, именуемый FRiS-STOLP.

Рассмотрим алгоритм
Пусть дана обучающая выборка ,![raspr](http://www.machinelearning.ru/mimetex/?X^l=(x_i,%20y_i)_{i=1}^l) где ![raspr](http://www.machinelearning.ru/mimetex/?x_i) - объекты, ![raspr](http://www.machinelearning.ru/mimetex/?y_i=y^*(x_i))  - классы, которым принадлежат эти объекты. 

Опишем две вспомогательные функции:

NN(u,U) – возвращает ближайший к u объект из множества U.
![raspr](http://www.machinelearning.ru/mimetex/?FindEtalon(X_y;\Omega)) – исходя из набора уже имеющихся эталонов ![raspr](http://www.machinelearning.ru/mimetex/?\Omega) и набора ![raspr](http://www.machinelearning.ru/mimetex/?X_y) элементов класса Y, возвращает новый эталон для класса Y

Для каждого объекта  ![raspr](http://www.machinelearning.ru/mimetex/?x%20\in%20X_y) вычисляются две характеристики:

 - обороноспособность объекта x:  

![raspr](http://www.machinelearning.ru/mimetex/?D_x%20=%20\frac{1}{\left|%20X_y%20\right|%20-1}\sum_{u%20\in%20X_y%20\setminus%20x}S%20\left(u,x%20|%20NN(u,\Omega)%20\right))
 - толерантность объекта x (количественная оценка, насколько объект x в роли эталона класса y не мешает эталонам других классов): 

![raspr](http://www.machinelearning.ru/mimetex/?T_x%20=%20\frac{1}{\left|%20X^l%20\setminus%20X_y%20\right|}\left(\sum_{v%20\in%20X^l%20\setminus%20X_y}S%20\left(v,x%20|%20NN(v,\Omega)%20\right)\right))

На основании полученных характеристик вычисляется эффективность объекта x

![raspr](http://www.machinelearning.ru/mimetex/?E_x%20=%20\lambda%20D_x%20+%20(1-\lambda)%20T_x)


 Функция FindEtalon возвращает объект ![raspr](http://www.machinelearning.ru/mimetex/?x%20\in%20X^l) с максимальной эффективностью ![raspr](http://www.machinelearning.ru/mimetex/?E_x)

![raspr](http://www.machinelearning.ru/mimetex/?x:=arg\max_{x%20\in%20X_y}{E_x})

Параметр ![raspr](http://www.machinelearning.ru/mimetex/?\lambda%20\in%20[0,1]))


Сам алгоритм FRiS-STOLP состоит из следующих шагов:

1. Инициализировать начальные множества эталонов. Для всех классов  y ∈ Y

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_5.png)

2. Инициализировать искомые множества эталонов. Для всех классов  y ∈ Y

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_6.png)

3. Повторять пункты 4-6, пока множество рассматриваемых объектов непусто ![raspr](http://www.machinelearning.ru/mimetex/?\left(%20X^l%20\not=%20\emptyset%20\right))

4. Сформировать множество U правильно классифицированных объектов:

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_7.png)

5. Удалить правильно классифицированные объекты из дальнейшего рассмотрения: 
   -  из множеств эталонов: для каждого ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_8.png)
    -  из обучающей выборки:
![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_9.png)

6. Добавить новый эталон для каждого класса y ∈ Y

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_10.png)

7. Вернуть искомые множества эталонов ![raspr](http://www.machinelearning.ru/mimetex/?\Omega_Y) для каждого класса y ∈ Y


### Результат работы алгоритма FRIS STOLP:

 Множество эталонов :

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/fris_Etalone.png)


Карта классификация : 

![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/metrica/img/map_frisstolp.png)

### Преимущества:

- Высокий уровень классификации
- При имеющих Эталонов быстрая классификация 

### Недостатки

- Сложная реализация
- Долгое нахождение Эталонов
