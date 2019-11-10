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
Будем полагать, что все объекты описываются n числовыми признаками. Обозначим через ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/5b.gif) произвольный элемент пространства объектов ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/6b.gif). Предположим, что все признаки ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/9b.gif). являются независимыми случайными величинами. Следовательно, функции правдоподобия классов представимы в виде, ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/7b.gif). где ![raspr](https://raw.githubusercontent.com/TIR13/ML0/master/bayes/img/8b.gif). - плотность распределений значений jго признака для класса y. Оценивать n одномерных плотностей гораздо проще, чем одну n-мерную плотность. Однако данное предположение крайне редко работает на практике, поэтому алгоритмы, использующий его, называют наивным байесовким методом. 

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
