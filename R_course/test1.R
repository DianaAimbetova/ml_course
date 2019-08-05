my_v1 = c(1,2,3,-5,-7)
my_v1[my_v1>0]

age = c(16,18,22,27)
is_married = c(FALSE, FALSE, TRUE, TRUE)
name = c('Olga', 'Maria', 'Nastya', 'Polina')
data = list(age,is_married,name)
data[[1]][2]
data[[2]][2]
data[[3]][2]

df = data.frame(Name = name, Age = age, Status = is_married)
typeof(df)

?read.table

mydata = read.csv('https://stepik.org/media/attachments/lesson/11481/evals.csv')
head(mydata,3)
View(mydata)
str(mydata)
summary(mydata)
a = names(mydata)
a

#variables
mydata$score
mydata$score*2
mydata$ten_point_scale = mydata$score*2
summary(mydata$ten_point_scale)
mydata$number = 1:nrow(mydata)
nrow(mydata)
ncol(mydata)
mydata$score[1:10]
mydata[1,1]
mydata[c(1,2,23,193),1]
mydata[5,]
mydata[,1]
mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female',1:4])

subset(mydata, gender=='female')
subset(mydata, score>3.5)

mydata2 = mydata[mydata$gender=='female',]
mydata3 = mydata[mydata$gender=='male',]
mydata4 = rbind(mydata2, mydata3)


mydata5 = mydata[,1:10]
mydata6 = mydata[,11:23]
mydata7 = cbind(mydata5,mydata6)


library(help = "datasets")
data(mtcars) 
df = mtcars


df = mtcars
df$even_gear = df[df$gear%%2==1,10]
df[df$gear%%2==0,10]

mtcars$even_gear = 1 - mtcars$gear %% 2

mpg_4 = mtcars[mtcars$cyl==4,1]
mpg_4


mini_mtcars = mtcars[c(3,7,10,12,32),]
mini_mtcars

new_data <- subset(mtcars, cyl != 3 & qsec > mean(qsec))
new_data

#if-else
a = 0

if(a > 0) {
  print('positive')
} else if(a < 0) {
  print('negative')
} else {
  print('zero')
}


ifelse(a>0, 'positive', 'negative')


a = c(1,-1)

#for
for(i in 1:100){
  print(i)
}

for(i in 1:nrow(mydata)){
  print(mydata$score[i])
}

for(i in 1:nrow(mydata)){
  if(mydata$gender[i]=='male'){
    print(mydata$score[i])
  }
}


mydata$quality = rep(NA, nrow(mydata))

for(i in 1:nrow(mydata)){
  if(mydata$score[i]>4){
    mydata$quality[i] = 'good'
  } else {
    mydata$quality[i] = 'nad'
  }
}

mydata$quality = ifelse(mydata$score>4, 'good', 'bad')


#while
i = 1
while(i < 51){
  print(mydata$score[i])
  i = i+1
}


mtcars$new_var = ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)


?AirPassengers
str(AirPassengers)

test = AirPassengers
test[1]

good_months = vector()
for(i in 2: 144){
    if(AirPassengers[i] > AirPassengers[i-1]){
      good_months[i-1] = AirPassengers[i]
  }
}
good_months = unique(good_months)
good_months
good_months <- good_months[!is.na(good_months)]


for(i in 1: (AirPassengers)){
print(i)
  }
length(AirPassengers)


AirPassengers[1:144]

i=1
moving_average = vector()

while(i<length(AirPassengers)){
  a = mean(AirPassengers[i:(i+9)])
  moving_average[i] = a
  i = i+1
}
moving_average <- moving_average[!is.na(moving_average)]
moving_average

mtcars$vs = factor(mtcars$vs, labels=c('V', 'S'))
mtcars$am = factor(mtcars$am, labels=c('Auto', 'Manual'))
str(mtcars)
median(mtcars$mpg)
range(mtcars$mpg)
sd(mtcars$mpg)
mean(mtcars$mpg[mtcars$cyl==6])

result = sd(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20 ])
result


mean_hp = aggregate(x = mtcars$hp, by=list(mtcars$am), FUN=mean)
mean_hp
colnames(mean_hp) = c('am', 'hp')
aggregate(hp ~ am, mtcars, mean)
aggregate(hp ~ am+vs, mtcars, mean)

aggregate(x = mtcars[,-c(8,9)], by=list(mtcars$am), FUN=median)
aggregate(cbind(mpg,disp) ~ am+vs, mtcars,sd)
aggregate(cbind(hp,disp) ~ am, mtcars, sd)   


library(psych)

?describe          
describe(x=mtcars[, -c(8,9)])

describeBy(x=mtcars[,-c(8,9)], group = mtcars$vs)
x = describeBy(x=mtcars[,-c(8,9)], group = mtcars$vs, mat = TRUE, digits = 1, fast=TRUE)

y = describeBy(x=mtcars[,-c(8,9)], group=list(mtcars$vs, mtcars$am), mat=T, digits = 1, fast = T)


sum(is.na(mtcars))

typeof(airquality)

df = airquality
subset = subset(df, airquality$Month%in%c(7,8,9))
test = aggregate(Ozone ~ Month , subset,length )



a = describeBy(x=airquality[,c(1,2,3,4)], group = airquality$Month, mat = T, digits = 1)


b = iris


median(iris$Petal.Length[iris$Species=='virginica'])
median(iris$Petal.Width[iris$Species=='virginica'])
median(iris$Sepal.Length[iris$Species=='virginica'])
median(iris$Sepal.Width[iris$Species=='virginica'])

l = vector(23, 10, 16, 19, 23, 22, 16, 21, 24, 20, 22, 21, 19, 25, 22, 14, 22, 14, 16, 15, NA, 24, NA, NA, NA, 23, 15, 21, 24, NA, NA, NA, 18, 21, 18, NA, 17, 20, 17, NA)


#visualization
hist(mtcars$mpg, breaks = 20, xlab = 'MPG')

boxplot(mpg ~ am, data = mtcars)

plot(mtcars$mpg, mtcars$hp)


library(ggplot2)

ggplot(mtcars, aes(x=mpg)) + geom_histogram(fill='white', col='black', bandwidth = 2)


ggplot(mtcars, aes(x=mpg, fill = factor(am))) + geom_dotplot()

ggplot(mtcars, aes(x=mpg, fill = factor(am))) + geom_density(alpha=0.5)


ggplot(mtcars, aes(x=factor(am), y=hp, col=factor(vs))) + geom_boxplot()


ggplot(mtcars, aes(x=mpg, y=hp, col=factor(vs),size=qsec)) + geom_point()


my_plot = ggplot(mtcars, aes(x=mpg, y=hp, col=factor(vs),size=qsec)) + geom_point()


my_plot2 = ggplot(mtcars, aes(x=factor(am), y=hp, col=factor(vs)))

my_plot2 + geom_boxplot()



ggplot(airquality, aes(x=factor(Month), y=Ozone))+geom_boxplot()
geom_s


ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))


getwd()
setwd('../Desktop/mlcourse.ai/R_course/')


write.csv(mtcars,'mtcars.csv')
mean(10**6:10**9)


#function

my_calc = function(x,y){
  s = x + y
  d = x - y
  return(c(s,d))
}

my_calc(10,15)


distr1 = rnorm(100)
distr1[1:30] = NA

distr1[is.na(distr1)] = mean(distr1, na.rm = T)
distr1


my_na_rm = function(x){
  if(is.numeric(x) == T){
    stat_test = shapiro.test(x)
    if(stat_test$p.value > 0.05){
      x[is.na(x)] = mean(x, na.rm = T)
      print('mean')
    }
    else {
      x[is.na(x)] = median(x, na.rm = T)
      print('median')
    }
  return(x)
  } else {
    print('X is not numeric')
  }
}
my_na_rm(rnorm(100))


d1 = rnorm(1000)
d2 = runif(1000)
d1[1:50] = NA
d2[1:50] = NA

my_na_rm(d1)
my_na_rm(d2)

source('my_na_rm.R')
