library(froeign)
library(bnlearn)

age = c(21,20,18,19,21)
price = c(100,200,300,400,100)
bool = c("T","F","T","F","T")

my.data = data.frame(age,price,bool)
my.data


my.data$age = as.factor(my.data$age) ;
my.data$price = as.factor(my.data$price) ;
my.data$bool = as.factor(my.data$bool) ;

d = discretize(my.data, method = 'hartemink', breaks = 4, ibreaks = 20)

my.data

str(my.data)

tan = tree.bayes(my.data,"age") #specify the root node

#Code doesn't work :!
