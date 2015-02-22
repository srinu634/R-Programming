#bnlearn basics

library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(forecast)

attributes  = c("NAME","AGE","SCORE","RACE") ;

g = empty.graph(attributes)

modelstring(g) = " [NAME][SCORE][RACE|SCORE:NAME][AGE]"

training.data = bayesiansampledata[1:2,]
test.set = bayesiansampledata[3:4,]

d = chiM(training.data, alpha = 1)
frame = data.frame(d$Disc.data) 
frame

n = nrow(frame)


col_names <- names(frame)
# do do it for some names in a vector named 'col_names'
frame[,col_names] <- lapply(frame[,col_names] , factor)
#frame


col_names <- names(test.set)
# do do it for some names in a vector named 'col_names'
test.set[,col_names] <- lapply(test.set[,col_names] , factor)
#test.set

# frame$NAME  = as.factor(frame$NAME)
# frame$SCORE  = as.factor(frame$SCORE)
# frame$AGE  = as.factor(frame$AGE)
# frame$RACE  = as.factor(frame$RACE)
# 
# test.set$NAME  = as.factor(test.set$NAME)
# test.set$SCORE  = as.factor(test.set$SCORE)
# test.set$AGE  = as.factor(test.set$AGE)
# test.set$RACE  = as.factor(test.set$RACE)




######################
#Constructing the graph manually
plot(g)

graphviz.plot(gtemp)

gtemp = g
modelstring(gtemp) = " [NAME|AGE][SCORE|NAME][RACE|SCORE:NAME:AGE][AGE]"


score(g, frame, type = "k2")

fit  = bn.fit (g,frame)

coefficients(fit)
pred = predict(fit$NAME, test.set)


cbind(pred, test.set[, "NAME"])

temppred = pred
temp.testset = test.set

temppred <- as.numeric(as.character( temppred ))
temp.testset[,"NAME"] <- as.numeric(as.character( temp.testset[,"NAME"] ))

accuracy(f = temppred, x = temp.testset[, "NAME"])

#table(pred, bayesiansampledata[,"NAME"])

