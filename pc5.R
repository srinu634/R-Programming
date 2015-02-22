library(infotheo)
library(foreign)
library(bnlearn)
library(discretization)
library(ROCR)
library(pROC)
library(forecast)


pc5 = read.arff("C:\\Users\\redhawk\\Desktop\\Thesis\\datasets\\pc5.arff") #Load the dataset

excludevars <-  names(pc5) %in% c("PARAMETER_COUNT")
pc5 <- pc5[ !excludevars  ]
#str(pc5)
pc5.test = pc5[600:705,] # Build a Test set


# str(pc5) 
#options(error=recover)
#reach_full_in <- reachability(krack_full, 'in')
# Change the Strings Y,N to 1,0
pc5$c <- as.numeric(factor(pc5$c , levels=c("FALSE" ,"TRUE") ) ) 
pc5.test$c <- as.numeric(pc5.test$c , levels=c("FALSE" ,"TRUE") ) 

#traceback()
#discretize the data for bayesian networks
pc5.disc = chiM(pc5, alpha = 0.05) 
pc5.test.disc = chiM(pc5.test,alpha=0.05)

#load the data into a data frame
pc5.disc.data = data.frame(pc5.disc$Disc.data)
pc5.test.data = data.frame(pc5.test.disc$Disc.data)


#Change all the discretized values into factors
pc5.disc.data[,names(pc5)] <- lapply(pc5.disc.data[,names(pc5)] , factor) 
pc5.test.data[,names(pc5.test)] <- lapply(pc5.test.data[,names(pc5.test)] , factor) 
#str(pc5.test.set.data)


#Building a Naive Bayes classifier
pc5.bn = naive.bayes(pc5.disc.data, "c")
pc5.pred.bn = predict(pc5.bn, pc5.disc.data)  #2nd parameter should be pc5.test.data
table(pc5.pred.bn,pc5.disc.data[,"c"])   #output the prediction matrix

#Change the outputs to numeric values; Happens at two levels. 1) Change the params to characters 2) Change the characters to numeric.
pc5.given.bn <- as.numeric(as.character(pc5.pred.bn))
pc5.predicted.bn <- as.numeric(as.character(pc5.disc.data[,"c"]))
accuracy(f = pc5.given.bn , x = pc5.predicted.bn)  #print the accuracy


#Building a Tree Augmented Network classifier
pc5.tan = tree.bayes(pc5.disc.data, "c")
#graphviz.plot(pc5.tan)
pc5.fitted = bn.fit(pc5.tan, pc5.disc.data)
#coefficients(pc5.fitted)
pc5.pred.tan <- predict(pc5.fitted$c, pc5.disc.data) #2nd parameter should be pc5.test.data
#pc5.pred.tan <- as.numeric(pc5.pred.tan)
#pc5.perf.tan <- performance(pc5.pred.tan,  measure = "tpr",x.measure =   "fpr")
table(pc5.pred.tan, pc5.disc.data[, "c"]) #output the prediction matrix
#Change the outputs to numeric values; 
pc5.given.tan <- as.numeric(as.character(pc5.pred.tan))
pc5.pred.tan <- as.numeric(as.character(pc5.disc.data[,"c"]))
accuracy(f = pc5.given.tan , x = pc5.pred.tan)  #print the accuracy

#score(pc5.tan, pc5.disc.data, type = "bde")
#score(pc5.bn,pc5.disc.data,type="bde")
