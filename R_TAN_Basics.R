#Constructing a test Bayesian Network (TAN)
library(bnlearn)

learning.test = mtcars #See data() for other available data sets
learning.test
head(learning.test)

#factor the features
learning.test$mpg = as.factor(learning.test$mpg)
learning.test$cyl = as.factor(learning.test$cyl)
learning.test$disp = as.factor(learning.test$disp)
learning.test$hp = as.factor(learning.test$hp)
learning.test$drat = as.factor(learning.test$drat)
learning.test$wt = as.factor(learning.test$wt)
learning.test$qsec = as.factor(learning.test$qsec)
learning.test$vs = as.factor(learning.test$vs)
learning.test$am = as.factor(learning.test$am)
learning.test$gear = as.factor(learning.test$gear)
learning.test$carb = as.factor(learning.test$carb)




tan = tree.bayes(learning.test,"mpg") #specify the root node
fitted = bn.fit(tan, learning.test, method = "bayes") #learn the parameters
fitted
pred = predict(fitted, learning.test) #?
pred #?
table(pred, learning.test[, "mpg"]) #?

graphviz.plot(tan)






