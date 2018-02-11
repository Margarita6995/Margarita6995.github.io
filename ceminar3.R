#Циклы и графика
#For - циклы с подсчетом повторов

y=c()
for (i in 1:100)
{
  x=i+2
  y=c(y,x)
}
y

l = list(1:10,-2:10,-9:-3,400:1000, 120:190)
mn=c()
for (i in 1:length(l))
{
  mn=c(mn,mean(l[[i]]))
}
mn

#Вычисление числа Пи методом Монтекарло
#Просто

x=runif(100000,-1,1)
y=runif(100000,-1,1)

Pi=0
Nr=0
Ns=0
for (i in 1:100000)
{
  
  if ((x[i]^2+y[i]^2)>1)
  { 
    Ns=Ns+1
  }
  else
  {
    Nr=Nr+1
  }
}
Pi=4*Nr/(Ns+Nr)
Pi

#Красиво

pis=c()
for (i in seq(10,10000,10))
{ x=runif(i,-1,1)
y=runif(i,-1,1)
z=table(x^2+y^2<=1)
my_p=4*z[2]/i
pis=c(pis,my_p)
}
plot(pis,type="l")

hist(pis,nclas=100)
mean(pis)

#Взяв iris data.frame, для первых двух колонок получить список содержащий матрицы
l=list()
for (i in 1:length(iris[,1]))
{
  v=c(iris[i,1],sum(iris[,1])-iris[i,1],iris[i,2],sum(iris[,2])-iris[i,2])
  dim(v)=c(2,2)
  l2=list(v)
  l=c(l,l2)
}
l

#Функция Apply
apply(iris[,1:4],2,sum)
tapply(iris$Sepal.Length, iris$Species, sum)

#Базовая графика - функция plot(x,y)
x=c(2,8,6)
y=c(3,7,9)
plot(5:10)
plot(x,y)
plot(x,y, type="l")
plot(x,y, type="b")
plot(x,y,type="h")
plot(x,y,type="s", col=rgb(.90,.10,.10,.9))
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), type="h")
plot(seq(-10,10,.01), sin(seq(-10,10,.01)), col=rgb(.90,.10,.10,.9), type="h", xlim=c(-20,20), ylim=c(-5,5))
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), col=rgb(.90,.10,.10,.9), type="h", xlim=c(-20,20), ylim=c(-5,5),main="My first cos(x)",xlab="x",ylab="sin(x)")
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), col=rgb(.90,.10,.10,.9), type="l", xlim=c(-20,20), ylim=c(-5,5),main="My first cos(x)",xlab="x",ylab="sin(x)",lwd=.4)
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), col=rgb(.90,.10,.10,.9), type="l", xlim=c(-20,20), ylim=c(-5,5),main="My first cos(x)",xlab="x",ylab="sin(x)",lwd=4, lty=4)
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), col=rgb(.90,.10,.10,.9), type="b", xlim=c(-20,20), ylim=c(-5,5),main="My first cos(x)",xlab="x",ylab="sin(x)",lwd=4, lty=4, pch=4)
plot(seq(-10,10,.01),sin(seq(-10,10,.01)), type="l")
lines(seq(-10,10,.01),rep(c(0),2001), type="l", col="red")
x=(seq(-10,10,.01))
plot(x, (1-x^2)^.5, type="l",ylim=c(1,-1), xlim=c(-1,1))
lines(x, -((1-x^2)^.5))
lines(rep(c(0),401),seq(-2,2,.01))
lines(seq(-2,2,.01),rep(c(0),401))

#Пакет ggplot2
library(ggplot2)

str(mtcars)
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

#Давайте построим график рассматривая cyl как фактор
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_point()

#Точечная диаграмма
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

#Легенда
ggplot(mtcars, aes(x = wt, y = mpg, col = disp)) +
  geom_point()

#Размер точек
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

#Добавим geom_point() и geom_smooth() через символ 
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth(method = 'gam')
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = 'gam')

ggplot(diamonds, aes(x = carat, y = price)) +  geom_smooth(method = 'gam')

ggplot(diamonds, aes(x = carat, y = price, col=clarity)) +
  geom_smooth(method = 'gam')

ggplot(diamonds, aes(x = carat, y = price, col=clarity)) +
  geom_point(alpha = 0.4)
dia_plot = ggplot(diamonds, aes(x = carat, y = price))
dia_plot + geom_point()
dia_plot + geom_point(aes(col=clarity))

