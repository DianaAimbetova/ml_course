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


my_vector = c(1, 2, 3, NA, NA)
NA.position = function(x){
  result = vector()
  counter = 1
  for(i in 1: length(x)){
    if(is.na(x[i])){
      result[counter] = i
      counter = counter + 1
    }
  }
  return(result)
}

test = NA.position(my_vector)
test

NA.counter = function(x){
  result = 0
  counter = 1
  for(i in 1: length(x)){
    if(is.na(x[i])){
      result = counter
      counter = counter + 1
    }
  }
  return(result)
}

test = NA.counter(my_vector)
test

NA.position <- function(x){    
  which(is.na(x))}


NA.counter <- function(x){    
  return(sum(is.na(x)))}


dir(pattern = '*.csv')
grants = data.frame()
for(i in dir(pattern = '*.csv')){
  temp_df = read.csv(i)
  grants = rbind(temp_df,grants)
}


read_data  = function(){
  df = data.frame()
  counter <<- 0
  for(i in dir(pattern = '*.csv')){
    temp_df = read.csv(i)
    df = rbind(temp_df, df)
    counter <<- counter + 1
  }
  print(paste(as.character(counter), 'files were combined.'))
  return(df)
}

grants2 = read_data()

my_vector = c(1, -2, 3, NA, NA)


filtered.sum = function(x){
  result = 0
  for(value in x){
    if(!is.na(value) & value > 0){
      result = result + value
    }
  }
  return(result)
}
test = 0
test = filtered.sum(my_vector)


filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}


outliers.rm = function(x){
}


d = rbinom(1000, 100, .5)
df = data.frame(a = test)
my_plot = ggplot(df, aes(y=a)) + geom_boxplot()
my_plot



outliers.rm = function(x, na.rm = TRUE){
  qnt = quantile(x, probs=c(.25, .75), na.rm = na.rm)
  iqr = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - iqr)] <- NA
  y[x > (qnt[2] + iqr)] <- NA
  return(y)
}
test = outliers.rm(d)
