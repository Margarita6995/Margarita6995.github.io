#Задание. Получите из данного вектора вектор в котором будет обратный порядок элементов

a<-seq(-8,10,.5)
a
##  [1] -8.0 -7.5 -7.0 -6.5 -6.0 -5.5 -5.0 -4.5 -4.0 -3.5 -3.0 -2.5 -2.0 -1.5
## [15] -1.0 -0.5  0.0  0.5  1.0  1.5  2.0  2.5  3.0  3.5  4.0  4.5  5.0  5.5
## [29]  6.0  6.5  7.0  7.5  8.0  8.5  9.0  9.5 10.0
x=a[length(a):1]
x
##  [1] 10.0  9.5  9.0  8.5  8.0  7.5  7.0  6.5  6.0  5.5  5.0  4.5  4.0  3.5
## [15]  3.0  2.5  2.0  1.5  1.0  0.5  0.0 -0.5 -1.0 -1.5 -2.0 -2.5 -3.0 -3.5
## [29] -4.0 -4.5 -5.0 -5.5 -6.0 -6.5 -7.0 -7.5 -8.0

#Задание. Вставьте число 99 в середину данного вектора

y=c(a[1:round(length(a)/2)],99,a[(round(length(a)/2)+1):length(a)])

#Задание. Докажите, что arcos(cos(x))=x

x=0:3
y=acos(cos(x))
x==y
## [1]  TRUE FALSE  TRUE FALSE
x=c(sin(10),log(5,8),log(3,exp(1)),log(38.6,10),cos(pi+1), exp(1)^(cos(0)),atan(+Inf))
names(x)=c("sin(10)","log(5,8)","ln3","lg(38.6)","cos(pi+1)","e^cos(0)","arctn(+Inf)");
sort(x)
##     sin(10)   cos(pi+1)    log(5,8)         ln3 arctn(+Inf)    lg(38.6) 
##  -0.5440211  -0.5403023   0.7739760   1.0986123   1.5707963   1.5865873 
##    e^cos(0) 
##   2.7182818

#Логические операции

! = not
| = or
& = and
A U B - объединение

x=seq(-10,10,.01)
y=x[(x>-10 & x< -3) | (x>0 & x<6 & x!=3)]

#Факторы и функция summary()

x = factor("Mouse")

f=c(rep("mouse",3),rep("rat",2),rep("fly",11))
f=rep(c("mouse","rat","fly"),c(3,2,11))
x=factor(f)
summary(x)
##   fly mouse   rat 
##    11     3     2
summary(f)
##    Length     Class      Mode 
##        16 character character
summary(1:10)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    3.25    5.50    5.50    7.75   10.00
0

summary(1:10)[4]
## Mean 
##  5.5

#Задание посчитайте долю каждого нуклеотида в векторе содержащего последовательность ДНК

dna=factor(rep(c("T","G","C","A"),c(16,38,6,10)))
ratio=summary(dna)/sum(summary(dna))
#Функция sample()

#Создает подвыборку из выборки, в том числе путем перемешивания

"sample: * mixing * generating"

dna=factor(rep(c("T","G","C","A"),c(16,38,6,10)))
sample(dna)
##  [1] G C T C G A T G C T G G G T G A G G G T G G T G A G G G G A G G G G G
## [36] G A A G A C G G T T G G A T T G A G T T G G G T G T C G G T T A G G C
## Levels: A C G T
dna
##  [1] T T T T T T T T T T T T T T T T G G G G G G G G G G G G G G G G G G G
## [36] G G G G G G G G G G G G G G G G G G G C C C C C C A A A A A A A A A A
## Levels: A C G T
# sample(dna,size=84, replace=FALSE) 
# подобный код выдаст ошибку, т.к. мы просим создать выборку больше исходника
# а вот следующий код будет работать, т.к. replace=TRUE генерирует выборки любого 
# размера исходя из пропопрций элемнтов данной выборки
sample(dna,size=84, replace=TRUE)
##  [1] G G G G G G C G G T G T G T T G G T G G G T T G G A G G T G G G G G C
## [36] G G G T T T G G G C C T T G G G G T T A G G G A G C G G G G T G G T T
## [71] C G G G T C G A T G G A G T
## Levels: A C G T
sampledna=sample(c("A","T","G","C"), size=10000,replace=TRUE)
ratio=summary(factor(sampledna))/length(sampledna)
ratio2=summary(factor(dna))/length(dna)
ratio
##      A      C      G      T 
## 0.2483 0.2526 0.2465 0.2526
ratio2
##          A          C          G          T 
## 0.14285714 0.08571429 0.54285714 0.22857143
#Функции union(x,y),intersect(x,y),setdiff(x,y)

x=1:10
y=6:14
union(x,y)
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14
intersect(x,y)
## [1]  6  7  8  9 10
setdiff(x,y)
## [1] 1 2 3 4 5
setdiff(y,x)
## [1] 11 12 13 14
#Уничтожение всех переменных в памяти

rm(list=ls())
#Функция cumsum()

cumsum(1:10)
##  [1]  1  3  6 10 15 21 28 36 45 55
#Задание. Создайте длинные последовательности с заданными пропорциями элементов

dna2=sample(c("A","T","G","G","G","C"), size=3826513,replace=TRUE)
ratio=summary(factor(dna2))/length(dna2)
ratio
##         A         C         G         T 
## 0.1667908 0.1664576 0.5000435 0.1667082
dna2=sample(c("A","T","G","C"), size=3826513,prob=c(.5/3,.5/3,.5,.5/3),replace=TRUE)
ratio=summary(factor(dna2))/length(dna2)
ratio
##         A         C         G         T 
## 0.1664001 0.1668597 0.4996646 0.1670756
## Data frames -Таблицы
#Выборки из таблиц

iris[iris$Species==c("setosa","virginica"),]
iris[iris$Species=="setosa" | iris$Species=="virginica",c("Petal.Length","Species")]

#Виды у которых Petal.Length>2 но <3
summary(factor(iris[(iris$Petal.Length>2 & iris$Petal.Length<6),"Species" ]))

#Создание функций - Functioning
my_func = function(p) 
{
  x=p^2
  y=x^3
  print("result")
  return(c(x,y))
  
}
my_func(2)

mathses = function(p,q)
{
  result = c(p+q,log(p+q,10),exp(p+q))
  names(result) = c("summ", "log","exp")
  return(result)
}
mathses(2,3)

#Проверки условий
mf = function(p)
{
  
  if (p>0)
  {
    x=log(p)
    return(x)
  }
  else
  {
    return(NULL)      
  }
}
mf(-1)

myfactorial = function(p) 
{
  if (p>0 & p%%1==0)
  {return(gamma(p+1))}
  else
  {return(NULL)}
}

myfactorial(5)


myfactorial = function(p) 
{
  if (p>0 & p%%1==0)
  {return(gamma(p+1))}
  else
  {return(NULL)}
}

mydistribution = function(x,p)
{
  if ( x>=0 & x%%1==0 & p<=1 & p>0 )
  {
    return(p*(1-p)^x)        
  }
  else
  {
    return(0)
  }
  
}
mydistribution(3,.4)
