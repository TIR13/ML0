# Байесовские алгоритмы классификации
- [Линии уровня нормального распределения](#Линии-уровня-нормального-распределения)

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
