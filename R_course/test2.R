df = read.csv('grants.csv')
df$status = as.factor(df$status)

describe(x=df)
levels(df$status) = c('Not funded', 'funded')
df$status = factor(df$status, labels = c('Not funded', 'funded'))

#Table

t1 = table(df$status)
t1
dim(t1)
t2 = table(status = df$status, field = df$field)
t2
dim(t2)
prop.table(t2,2)
t3 = table(years = df$years_in_uni, field = df$field, status = df$status)
t3

dim(t3)


dimnames(HairEyeColor)
sum(HairEyeColor[ , 'Green' ,'Female'])

red_men <- prop.table(HairEyeColor[,,'Male'],2)['Red','Blue']

barplot(t2, legend.text = T, args.legend = list(x='topright'))


mosaicplot(t2)

mydata <- as.data.frame(HairEyeColor)



library(ggplot2)
test = subset(x = mydata, Sex == 'Female')
obj <- ggplot(data = test, aes(x =Hair , y = Freq, fill=Eye)) + 
geom_bar(stat="identity", position = position_dodge()) + 
scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

obj


#Binomial test
binom.test(x=5, n=20, p=0.5)
binom.test(t1)


#Chi-square
chi = chisq.test(t1)
chi$expected
chi$observed


#Fisher's exact test
fisher.test(t2)



brown_hair = HairEyeColor['Brown',,'Female']
brown_hair

chi = chisq.test(brown_hair)
chi



diamonds_table = table(diamonds$cut, diamonds$color)
diamonds_table
chi = chisq.test(diamonds_table)
chi
main_stat = vector()
main_stat[1] = chi$statistic
typeof(main_stat)







data = diamonds
carat_mean = mean(data$carat)
price_mean = mean(data$price)
data$factor_carat = 0
data$factor_carat[data$carat >= carat_mean] = 1
data$factor_price = 0
data$factor_price[data$price >= price_mean] = 1
diamonds_table = table(data$factor_carat, data$factor_price)
chi = chisq.test(diamonds_table)
main_stat[1] = chi$statistic
main_stat



fisher_test = fisher.test(table(mtcars$am, mtcars$vs))$p.value
fisher_test


df = iris
str(df)
df1 = subset(df,Species!='setosa')

hist(df1$Sepal.Length)
ggplot(df1, aes(x=Sepal.Length))+geom_histogram(fill='white', col='black', binwidth=0.4)+
  facet_grid(Species~.)

ggplot(df1, aes(x=Sepal.Length, fill=Species)) + geom_density(alpha=0.5)


ggplot(df1, aes(x=Species, y=Sepal.Length)) + geom_boxplot()


#test for normality
shapiro.test(x = df1$Sepal.Length)
shapiro.test(x=df1$Sepal.Length[df1$Species=='versicolor'])
shapiro.test(x=df1$Sepal.Length[df1$Species=='virginica'])


#test for homogenity
bartlett.test(Sepal.Length ~ Species, df1)

#t-critery
test1 = t.test(Sepal.Length ~ Species, df1)
test1$p.value
t.test(df1$Sepal.Length, mu = 8)


t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

df2 = ToothGrowth
df3 = subset(df2, (supp == 'VC' & dose ==  2.0)  | (supp == 'OJ' & dose == 0.5))
t_stat = t.test(df3$len, mu = mean(df3$len))$p.value
t_stat
df2
df3


df4 = read.csv('lekarstva.csv')
str(df4)
t_test = t.test(df4$Pressure_before, df4$Pressure_after, paired = T)
t_test

# for trusted intervals
install.packages("Hmisc")
mean_cl_normal(df1$Sepal.Length)

ggplot(df1, aes(Species,Sepal.Length)) + 
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width=0.1) +
stat_summary(fun.y = mean, geom='point',size=4)


ggplot(df1, aes(x=Species, y=Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='pointrange', size = 2)


#non-parametric test
wilcox.test(Petal.Length ~ Species, df1)

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)








#bartlett test

